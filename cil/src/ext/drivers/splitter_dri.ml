(*===========================================================================*)
(* CIL Module for splitting device drivers into a user-component and a
 * kernel-component, aka, "the beast". This assumes as input a
 * completely colored call-graph.
 * 
 * The splitter is the most complex component of our tool. Most of the
 * complexity is in the marshaling/demarshaling functions, which,
 * though conceptually simple, have to check for a *lot* of cases.
 * 
 * Vinod Ganapathy <vg@cs.wisc.edu>, September 28, 2006.
 * Matt Renzelmann <mjr@cs.wisc.edu> August, 2010
 * Asim Kadav <kadav@cs.wisc.edu> February, 2012
 *)
(*===========================================================================*)

open Cil
open Str
open Scanf
open Utils_dri
open Splitter_globals_dri
open Splitter_utils_dri
open Splitter_marshcode_dri
open Marshannot_dri (* Marshaannot: Support for marhsaling annotations *)
open Cgcomp_dri    (* Callgraph: we need this to find interface functions *)
open Marshal_dri   (* Marshaling analysis *)

(*
 * Copy all the function names into a giant array called function_id_map
 * Used to make pretty printing during runtime.
 *)
class pass_function_id_map = object (self)
  (* Top level *)
  method top_level (f: file) : unit =
    begin
      f.globals <- List.append (gen_function_id_map()) f.globals;
    end
end
  
(*---------------------------------------------------------------------------*)
(** pass_modif_const
 * is a transformation that converts constant arguments in function calls
 * notably string constants, into calls with variables instead. The variables
 * are new globals, and are intialized with the string constant. This
 * transformation allows us to write into these globals without suffering a
 * segmentation fault
 *)

let generate_check_code (lv: lval) : stmt =
begin
     let check_var =  (expify_lval lv) in
     let false_block = (mkBlock [(mkEmptyStmt ())]) in
     let true_block  = (mkBlock
	  [(mkStmt(Return(Some(Const(CInt64((Int64.of_int (0-22) ),IInt,None))),
								  locUnknown)))]) in
    let new_stmt = (mkStmt (If (check_var, true_block, false_block, locUnknown))) in
    new_stmt;
end

class pass_modif_const = object (self)
  inherit nopCilVisitor
  val mutable added_globals : global list = [];
  val mutable added_varinfos : varinfo list = [];

  method get_strings_varinfos () : varinfo list = added_varinfos;
  method get_strings_globs () : global list = added_globals;

  method private add_string (prefix: string) (i: int) (s: string) : varinfo =
    begin
      let varinitinfo : initinfo = { init = None; } in
      let newglobnm = Printf.sprintf "%s_%d" prefix i in
      let strlen = (String.length s) + 1 in
      let globtyp = create_array_typ (TInt(IChar, [])) strlen in
      let newglob = get_global_variable globtyp newglobnm in
      let varinit = SingleInit(Const(CStr(s))) in
      varinitinfo.init <- Some(varinit);
      added_globals <- added_globals @ [GVar(newglob,varinitinfo,locUnknown)];
      added_varinfos <- added_varinfos @ [newglob];
      newglob;
    end

  method private analyze_expr_for_constants (e: exp) : exp =
    begin
      match e with
        | Const(con) ->
            (match con with
               | CStr(constr) ->
                   let newglob = self#add_string "str" (Hashtbl.hash constr) constr in
                   expify_lval (lvalify_varinfo newglob);
               | _ -> e;
            );
        | SizeOfE(e') ->
            SizeOfE(self#analyze_expr_for_constants e');
        | AlignOfE(e') ->
            AlignOfE(self#analyze_expr_for_constants e');
        | UnOp(op,e',t) ->
            UnOp(op, (self#analyze_expr_for_constants e'), t);
        | BinOp(op,e1,e2,t) ->
            BinOp(op, (self#analyze_expr_for_constants e1),
                  (self#analyze_expr_for_constants e2), t);
        | CastE(t,e') ->
            CastE(t,(self#analyze_expr_for_constants e'));
        | _ -> e;
    end

  (* Visit each Call and Set instruction and replace string constants
   * with a newly manufactured global *)
  method vinst (i: instr) : instr list visitAction =
    match i with
      | Call(lvopt, callexp, argslist, loc) ->
          let newargslist = ref [] in
          for i = 0 to (List.length argslist) - 1 do
            let ith = (List.nth argslist i) in
            let newith = (self#analyze_expr_for_constants ith) in
            newargslist := !newargslist @ [newith];
          done;
          let newcall = Call(lvopt, callexp, !newargslist, loc) in
          ChangeTo([newcall]);
      | Set(lv, e, loc) ->
          let newe = (self#analyze_expr_for_constants e) in
          let newset = Set(lv, newe, loc) in
          ChangeTo([newset]);
      | _ -> DoChildren;

  (* Visit each global initializer and replace string constants with
   * newly manufactured globals *)
  method private handleinit(i: init) : init =
    match i with
      | SingleInit(e) ->
          let newe = (self#analyze_expr_for_constants e) in
          SingleInit(newe);
      | CompoundInit(t, il) ->
          let newil = ref [] in
          for i = 0 to (List.length il) - 1 do
            let ith = (List.nth il i) in
            let (off, oldi) = ith in
            let newi = (self#handleinit oldi) in
            newil := !newil @ [(off, newi)];
          done;
          CompoundInit(t, !newil);

  method vinit (v: varinfo) (o: offset) (i: init) : init visitAction =
    ChangeTo((self#handleinit i))

  (* Top level *)
  method top_level (f: file) : unit =
    begin
      visitCilFile (self :> cilVisitor) f;
      added_globals <- remove_repeats added_globals;
      f.globals <- added_globals @ f.globals;
    end
end

class pass_fix_storage = object (self)
  inherit nopCilVisitor

  (** True if we are generating master code, false if we are generating slave *)
  val mutable gen_kern : bool = true

  (** Functions that we don't want to generate marshaling code for, since we
   * already have user-level implementations.  Example: strcpy, strlen, memcpy etc.
   *)
  val mutable nonstubbed_functions: (string, bool) Hashtbl.t = Hashtbl.create 5;

  (** We also need a vinst to consider functions that are called but that
   * don't have a fundec - because they do not have an implementation here,
   * e.g., things like _spin_lock, _spin_lock_irqsave etc., that are
   * implemented elsewhere in the kernel. These are "extern" functions.
   * This is used only in user-space.
   *)
  method vinst (i: instr) : instr list visitAction =
    begin
      if (gen_kern = false) then
        (match i with
           | Call(_,callexpr,argslist,_) ->
               (match callexpr with
                  | Lval(Var(vi), NoOffset) ->
                      (* Previously we forced all functions on boundaries to be
                         static? In user mode, this doesn't seem like it matters--
                         who cares if we're exporting symbols?

                         Making these static causes problems with Java code,
                         since we really want to be able to access pretty much
                         everything from Java, and if some things are static,
                         the Java code is effectively locked out.
                      *)
                      if (should_split nonstubbed_functions vi.vname) = true then
                        vi.vstorage <- Static;
                      
                      DoChildren;
                  | _ -> DoChildren;
               );
           | _ -> DoChildren;
        )
      else
        DoChildren;
    end

  method top_level
    (f: file)
    (param_gen_kern: bool)
    (param_nonstub_functions: (string, bool) Hashtbl.t)
    : unit =
    begin
      nonstubbed_functions <- param_nonstub_functions;
      gen_kern <- param_gen_kern;
      visitCilFile (self :> cilVisitor) f;
    end
end

class pass_instrument_funcs = object (self)
  inherit nopCilVisitor
  val mutable is_kern = ref 0;  
  method vfunc (f: fundec) : fundec visitAction =
    begin
      let instrument_arg = mkString f.svar.vname in
      let instrument_args = [instrument_arg] in
      let instrument_fundec = emptyFunction "record_function" in
      let instrument_exp = expify_fundec instrument_fundec in
      let instrument_instr = Call(None,
                                  instrument_exp,
                                  instrument_args,
                                  locUnknown) in
      let instrument_call = Instr([instrument_instr]) in
      let instrument_stmt = mkStmt instrument_call in
      f.sbody.bstmts <- [instrument_stmt] @ f.sbody.bstmts;

      (* Code for on-demand logging 
      if (!is_kern == 0) then (
          let counter = ref 0 in
          let varlist = ref [] in
          let instrument_arg = mkString f.svar.vname in
          let instrument_args = [instrument_arg] in

          let (retType,argslist,_,_) = splitFunctionType f.svar.vtype in

          argsList = Cil.argsToList(argslist);
          match argslist with


          let instrument_fundec = emptyFunction "odft_insert_klog_hash" in
          let instrument_exp = expify_fundec instrument_fundec in
          let instrument_instr = Call(None,
          instrument_exp,
          instrument_args,
          locUnknown) in
          let instrument_call = Instr([instrument_instr]) in
          let instrument_stmt = mkStmt instrument_call in
          f.sbody.bstmts <- [instrument_stmt] @ f.sbody.bstmts;
      );
      *)
      DoChildren;
    end

  method top_level
    (f: file) (kern: int)
    : unit = 
        is_kern := kern;
        visitCilFile (self :> cilVisitor) f;
end

class pass_find_addr_taken = object (self)
  inherit nopCilVisitor

  (** A list of functions whose addresses are taken. We must register these
      with the object tracker.  Includes only those functions for which
      we've encountered a function _definition_, e.g. the body.
  *)
  val mutable fn_addr_taken_defn : fundec list = [];
  (** A list of functions whose addresses are taken, but for which all we've
      observed is the _declaration_ (e.g. prototype) and not the body.
      This list may include some functions already present in the previous
      list, like those that have both a prototype and body.  If we're taking
      the address of a function that has only a prototype, then we need to
      supply the body.  Happens in the user-half with kernel functions.
  *)
  val mutable fn_addr_taken_decl : varinfo list = [];

  method get_defns () : fundec list = fn_addr_taken_defn

  method get_decls () : varinfo list = fn_addr_taken_decl

  (** Used to find functions implemented in the kernel
      whose addresses are taken in the user.  We need to provide stub
      functions for these. and register them in the OT.
  *)
  method vglob (g: global) : global list visitAction =
    begin
      (match g with
         | GVarDecl(vi, _) ->
             (match vi.vtype with
                | TFun (a1, a2, a3, a4) ->
                    if (vi.vaddrof = true) then
                      begin
                        fn_addr_taken_decl <- List.append fn_addr_taken_decl [vi];
                      end;
                | _ -> ();
             );
         | GFun(fdec, _) ->
             if (fdec.svar.vaddrof) then
               fn_addr_taken_defn <- List.append fn_addr_taken_defn [fdec];
         | _ -> ();
      );
         DoChildren;
    end

  method top_level (f: file) : unit =
    begin
      visitCilFile (self :> cilVisitor) f;
    end
end



(*---------------------------------------------------------------------------*)
(** Splitting algorithm implementation
 * This takes a file as input, and outputs a modification of the file, based
 * upon which portion is being generated (kernel or user portion). The idea
 * is that this analysis must be run twice: once to obtain the kernel portion,
 * which is the master and once to obtain the user portion, which is the slave.
 *)
(** splitter_unified
 * Main steps:
 * (1) To generate stubs for functions that are marked as kernel functions.
 * (2) For functions that lack a fundec, generate a function body: This is
 *     done separately in the function get_new_fundecs
*)
(**
 * Considers those functions that have the kern annotation.  For
 * those such functions that have fundecs, their function bodies are
 * replaced by a call to the function in the master.  For those such
 * functions that do not have fundecs, a function body is
 * genenerated calling the function in the kernel (with an
 * appropriate extern statement generated.  *)
(*---------------------------------------------------------------------------*)
(** splitter_unified: This generates the master or slave,
 *  i.e., code to execute on the kernel or user side. *)
class virtual splitter_unified = object (self) inherit nopCilVisitor
  (** True if we are generating master code, false if we are generating slave *)
  val mutable gen_kern : bool = true;
  (** A list of marshwrap functions that have been added *)
  val mutable marshwrap_funcs : fundec list = [];
  (** A list of functions to make NoStorage *)
  val mutable nostorage_funcs : fundec list = [];
  (** A temporary varinfo, storing the name of the marshaling buffer *)
  val mutable curr_marshbuf : lval option = None;
  (** A temporary varinfo.  When generating the master, it
   *  stores the name of the current marshaling buffer offset.
   * When generating the slave, it stores the size of the marshaling
   * buffer.
   *)
  val mutable curr_marshoff : lval option = None;
  (** A temporary variable, storing the current ptg *)
  val mutable curr_ptg: (string, (typ * string * string)) Hashtbl.t = Hashtbl.create 5;
  (** A temporary variable, storing the current set of globals accessed *)
  val mutable curr_globs : varinfo list = [];
  (** A temporary variable, storing the current resfld *)
  val mutable curr_resfld: (string, (typ * fieldinfo * typ)) Hashtbl.t = Hashtbl.create 5;
  (** A temporary variable, storing the current resform *)
  val mutable curr_resform: (string, (int * varinfo * typ)) Hashtbl.t = Hashtbl.create 5;
  (** A list of non-interface functions with "kern" or "user" annotations.
   *  When generating the master, it contains those with "user" annotations.
   *  When generating the slave, it contains those with "kern" annotations. *)
  val mutable non_interface_functions: (string, fundec) Hashtbl.t = Hashtbl.create 5;
  (** User or Kern annotations for each function *)
  val mutable annotations : (string, string) Hashtbl.t = Hashtbl.create 117;
  (**
   * Functions that do not matter for splitting. User and Kernel can have
   * individual copies of these functions.  By default we keep the prototype,
   * If the function is also listed in the "delete_proto" list, then we remove
   * the prototype as well.
   *)
  val mutable nonstubbed_functions: (string, bool) Hashtbl.t = Hashtbl.create 117;
  val mutable nonstubbed_functions_delete_proto: (string, bool) Hashtbl.t = Hashtbl.create 117;
  (** Maps function name to true/false.  This list is used only for overriding the
   * static analysis, since the static analysis does not always determine
   * the interface functions correctly.
   * - false if we're a non-interface function
   * - true if we're an interface function
   *)
  val mutable override_interface: (string, bool) Hashtbl.t = Hashtbl.create 117;
  (** Names of interface functions. Only these need to be stubbed and exported *)
  val mutable interface_functions: (string, bool) Hashtbl.t = Hashtbl.create 117;
  (** Names and varinfos of functions that do not have fundecs *)
  val mutable funcs_with_no_fundecs: (string, varinfo) Hashtbl.t = Hashtbl.create 117;
  (** Entry points and their classification / calling context.
   * This feature is used for symbolic execution testing
   * so that we can see precisely what the calling context is of
   * the function in question.
   *)
  val mutable entry_points: (string, string) Hashtbl.t = Hashtbl.create 117;
  
  method get_curr_marshbuf () : lval =
    begin
      match curr_marshbuf with
        | Some(marshbuf) -> marshbuf
        | None -> fatal ["no marshbuf found"]
    end
          
  method get_curr_marshoff () : lval =
    begin
      match curr_marshoff with
        | Some(marshoff) -> marshoff
        | None -> fatal ["no marshoff found"]
    end
   
  (* call_m_dm_in_stub: This function simply generates the temporary variables,
   * finds the list of fields to be marshaled by querying the points-to
   * graph, and calls the DFS based marshaler. Varinfos other than those
   * obtained from the arguments of the functions that need to be marshaled
   * are passed as a list. The marshaling parameter is true if we are generating
   * marshaling code, false otherwise.
   *)
  method call_m_dm_in_stub
    (f_curr: fundec)  (* May be a marsh/demarsh stub.  Should have name prefix *)
    (vl: varinfo list)
    (marshaling: bool)
    : stmt list =
    begin
      let stripped_name = strip_stub_prefix f_curr.svar.vname in
      let ret_stmts = ref [] in

      (* If we're marshaling, we are filling a buffer.  If we are demarshaling,
         we are worrying about the offset in the existing buffer. *)
      let buf_var = get_local_variable f_curr voidPtrType "_buf_" in
      let off_var = get_local_variable f_curr intType "_off_" in
      curr_marshbuf <- Some(lvalify_varinfo buf_var);
      curr_marshoff <- Some(lvalify_varinfo off_var);

      (* Insert a statement initializing marsh buf / off *)
      let stmts_init = self#stubgen_init_vars_alt f_curr buf_var off_var marshaling in
      ret_stmts := List.append !ret_stmts stmts_init;

      let ptg = ref curr_ptg in
      if marshaling then begin
        (* The initial malloc of marshoff must happen in fill_m_dm *)
        (* Get the flattened points-to graph to tell us what to marshal/demarshal *)
        ptg := Marshal_dri.get_flattened_ptg stripped_name false;
        curr_ptg <- !ptg;
        (* Get the globals accessed *)
        curr_globs <- Marshal_dri.get_globals_accessed stripped_name;
        curr_globs <- remove_repeats curr_globs;
      end;

      (* Now send the roots to the DFS-based marshaler *)
      (* Return variable information is stored in vl *)
      let formal_filter elt =
        if elt.vname = "_buf_" || elt.vname = "rqarg" then false else true in
      let modified_formals = List.filter formal_filter f_curr.sformals in
      let roots = curr_globs @ modified_formals @ vl in
      let resfld =
        if marshaling then
          Marshal_dri.get_resolved_fields()
        else curr_resfld
      in
      let resform = 
        if marshaling then
          Marshal_dri.get_resolved_formals()
        else curr_resform
      in
      if marshaling then begin
        curr_resfld <- resfld;
        curr_resform <- resform;
      end;

      for i = 0 to (List.length roots) - 1 do
        let ith = List.nth roots i in
        let ithlv = lvalify_varinfo ith in
        let ithtyp = ith.vtype in
        let marsh_stmts = unified_dfs ithlv ithtyp f_curr f_curr
          !ptg resfld resform buf_var off_var 0
          marshaling gen_kern in
        ret_stmts := List.append !ret_stmts marsh_stmts;
      done;

      if marshaling then
        begin
          let rqarg_init_stmts = self#stubgen_rqarg_init_stmts f_curr in
          ret_stmts := List.append !ret_stmts rqarg_init_stmts;
        end;
      !ret_stmts;
    end

  method virtual stubgen_preconditions : fundec -> fundec -> lval list -> stmt list
  method virtual stubgen_postconditions : fundec -> fundec -> lval list -> stmt list

  (** stubgen_marsh:  Generates code for marshaling stub *)
  method stubgen_marsh_funs (f: fundec) : stmt list =
    begin
      (* 0. See if this function can be handled *)
      let (_,_,isvararg,_) = splitFunctionType f.svar.vtype in
      if isvararg
      then addwarn ["VARARGS not supported in stubgen_marsh_funs for fn:";
                    f.svar.vname;
                    "; any varargs are not marshaled"];
      
      let marshaling_code = self#call_m_dm_in_stub f [] true in

      (* Make sure all formal parameters have names.  Sometimes,
         the function prototype is provided without variable names--
         in these cases, we make some up. *)
      ensure_formals_have_names f;
      marshaling_code;
    end

  method stubgen_demarsh_funs (f: fundec) : stmt list =
    begin
      let (stmt_ret, demarshlist) = self#stubgen_demarsh_retstmt f in
      let demarshaling_code = self#call_m_dm_in_stub f demarshlist false in
      
      (* DEBUG
       * Printf.fprintf stderr "stubgen_demarsh_funs fn %s %s."
      f.svar.vname (stmtlist_tostring demarshaling_code);
      
      Printf.fprintf stderr "---------stubgen_demarsh_funs END -----------.\n";
      *)
      (* Make sure all formal parameters have names.  Sometimes,
         the function prototype is provided without variable names--
         in these cases, we make some up. *)
      ensure_formals_have_names f;
      demarshaling_code @ [stmt_ret];
    end

  (* Marshaling/Demarshaling statemetns *)  
  method stubgen_m_dm_call (f: fundec) (m_dm_func : fundec) : stmt list =
    begin
      let rqarg_vi = get_local_variable f struct_reqargs_typ "rqarg" in
      let rqarg_lval = lvalify_varinfo rqarg_vi in
      let rqarg_param = mkAddrOf rqarg_lval in
      let remainder_of_params = expify_varinfos (make_params f) in
      let params = [rqarg_param] @ remainder_of_params in
      let (stmt_list, vi_option) = self#gen_marshwrap_call
        params m_dm_func.svar.vname m_dm_func.svar.vtype m_dm_func in
      stmt_list;
    end

  method stubgen_init_vars_alt
    (f: fundec)
    (buf_var : varinfo)
    (off_var : varinfo)
    (marshaling: bool) : stmt list =
    begin
      (* Insert a statement initializing (de)marsh(buf/off) *)
      let init_rhs = zero64Uexp in
      let var_buf_lval = lvalify_varinfo buf_var in
      let var_off_lval = lvalify_varinfo off_var in
      let init_stmts =
        if marshaling then
          let init_buf = Instr ([Set(var_buf_lval, init_rhs, locUnknown)]) in
          [mkStmt init_buf]
        else
          let rqarg_varinfo = get_formal_variable f "rqarg" in
          let rqarg_lval = lvalify_varinfo rqarg_varinfo in
          let rqarg_varinfo_deref = mkMem (expify_lval rqarg_lval) NoOffset in
          let rqarg_buf_lval = add_field_to_lval rqarg_varinfo_deref reqargs_data in
          let instr_set_data = Set (var_buf_lval,
                                    expify_lval (rqarg_buf_lval),
                                    locUnknown) in
          let stmt_setdatarqarg = mkStmtOneInstr instr_set_data in
          [stmt_setdatarqarg]
      in

      let ret_stmts = 
        [mkStmt(Instr([Set(var_off_lval, init_rhs, locUnknown)]))]
        @ init_stmts
      in
      ret_stmts;
    end

  method stubgen_rqarg_init_stmts (f: fundec) : stmt list =
    begin
      (* Create a new local variable rqarg of type req_args *)
      let rqarg_varinfo = get_formal_variable f "rqarg" in
      let rqarg_lval = lvalify_varinfo rqarg_varinfo in
      let rqarg_varinfo_deref = mkMem (expify_lval rqarg_lval) NoOffset in

      (* Get the ID of the function to be called, and assign it to rqarg *)
      let tgt_funcid = get_function_id (strip_stub_prefix f.svar.vname) in
      let tgt_funcid_exp = integer tgt_funcid in
      let instr_set_id = Set (add_field_to_lval rqarg_varinfo_deref reqargs_funcid,
                              tgt_funcid_exp, locUnknown) in
      let stmt_setrqargfuncid = mkStmtOneInstr instr_set_id in

      (* Get the buffer that stores the data *)
      let curr_marshbuf_lval = self#get_curr_marshbuf () in
      let curr_marshbuf_exp = expify_lval curr_marshbuf_lval in
      let instr_set_data = Set (add_field_to_lval rqarg_varinfo_deref reqargs_data,
                                curr_marshbuf_exp, locUnknown) in
      let stmt_setrqargdata = mkStmtOneInstr instr_set_data in

      (* Get the length of the buffer *)
      let curr_marshoff_lval =
        match curr_marshoff with
          | Some(marshoff) -> marshoff;
          | None -> fatal ["no marshoff found"];
      in
      let curr_marshoff_exp = expify_lval curr_marshoff_lval in
      let instr_set_marshoff = Set (add_field_to_lval rqarg_varinfo_deref reqargs_length,
                                    curr_marshoff_exp, locUnknown) in
      let stmt_setrqarglength = mkStmtOneInstr instr_set_marshoff in

      [stmt_setrqargfuncid; stmt_setrqargdata; stmt_setrqarglength];
    end

  method stubgen_free_call (f: fundec)
    (function_name : string)
    : stmt list =
    begin
      (* Create the code to free the marshaling buffer *)
      let rqarg_varinfo = get_local_variable f struct_reqargs_typ "rqarg" in
      let rqarg_lval = lvalify_varinfo rqarg_varinfo in
      let rqarg_buf_lval = add_field_to_lval rqarg_lval reqargs_data in
      [gen_marshbuf_free function_name rqarg_buf_lval];
    end
      
  method stubgen_rqarg_init_zero (f: fundec) : stmt list =
    begin
      (* Get the rqarg local variable *)
      let rqarg_varinfo = get_local_variable f struct_reqargs_typ "rqarg" in
      let rqarg_lval = lvalify_varinfo rqarg_varinfo in

      (* Get the ID of the function to be called, and assign it to rqarg *)
      let zero_exp = integer 0 in
      let instr_set_id = Set (add_field_to_lval rqarg_lval reqargs_funcid,
                              zero_exp, locUnknown) in
      let stmt_setrqargfuncid = mkStmtOneInstr instr_set_id in

      (* Get the buffer that stores the data *)
      let instr_set_data = Set (add_field_to_lval rqarg_lval reqargs_data,
                                zero_exp, locUnknown) in
      let stmt_setrqargdata = mkStmtOneInstr instr_set_data in

      (* Get the length of the buffer *)
      let instr_set_marshoff = Set (add_field_to_lval rqarg_lval reqargs_length,
                                    zero_exp, locUnknown) in
      let stmt_setrqarglength = mkStmtOneInstr instr_set_marshoff in

      (* Initialize the return value if applicable *)
      let retval_vi = self#stubgen_demarsh_retval f in
      let stmt_init_retval =
        match retval_vi with
          | (_, None) -> []
          | (_, Some(retval_vi_some)) ->
              let instr_retval_set_0 = Set (lvalify_varinfo retval_vi_some,
                                            zero_exp, locUnknown) in
              [mkStmtOneInstr instr_retval_set_0];
      in
      [stmt_setrqargfuncid; stmt_setrqargdata; stmt_setrqarglength] @ stmt_init_retval;
    end

  method stubgen_switch_stmts (f: fundec) : stmt list =
    begin
      (* Create a new local variable rqarg of type req_args *)
      let rqarg_typ = struct_reqargs_typ in
      let rqarg_varinfo = get_local_variable f rqarg_typ "rqarg" in

      (* Create a call to disp_user/disp_kern *)
      let rqarg_lval = lvalify_varinfo rqarg_varinfo in
      let rqarg_exp = mkAddrOf rqarg_lval in
      let fnarg_lval = mkString f.svar.vname in
      let argslist = [fnarg_lval; rqarg_exp] in
	
      (* <The following code uses unlock_user_thread or disp_kern to
         call a function in the user and is the function that must be
         used with two address spaces> *)
      let (switch_func, _, _) =
        if gen_kern then
          get_disp_userkern_fundec "disp_user"
        else
          get_disp_userkern_fundec "disp_kern"
      in
      let switch_func_expr = expify_fundec switch_func in

(* let call_switch_func = Call(None, switch_func_expr, argslist, locUnknown) in *)
	let call_switch_func = Call(Some ((lvalify_varinfo (get_local_variable f intType "__odft_disp_error"))), switch_func_expr, argslist, locUnknown) in
	let stmt_call_switch_func = mkStmt (Instr [call_switch_func]) in

      (* Save all the statements *)
      [stmt_call_switch_func];
    end

  method stubgen_demarsh_retval
    (f: fundec) :
    (typ * varinfo option) =
    begin
      (* Create a new variable of the appropriate type and add it to f's slocals *)
      (* Also create the "extra" varinfo list for demarshaling *)
      let (ret_var_type,_,_,_) = splitFunctionType f.svar.vtype in
      let ret_var =
        match ret_var_type with
          | TVoid(_) -> None;
          | _ -> Some (get_local_variable f ret_var_type "_retval_");
      in
      (ret_var_type, ret_var);
    end

  method stubgen_demarsh_retstmt (f: fundec) : (stmt * varinfo list) =
    begin
      (* Create a new variable of the appropriate type and add it to f's slocals *)
      (* Also create the "extra" varinfo list for demarshaling *)
      let (ret_var_type, ret_var_varinfo) = self#stubgen_demarsh_retval f in
      let (ret_var_stmt, demarshlist) =
        (match ret_var_varinfo with
           | None -> (mkStmt (Return(None, locUnknown)), []);
           | Some(ret_var_varinfo_some) ->
               let ret_lval = lvalify_varinfo ret_var_varinfo_some in
               let ret_stmt = mkStmt (Return(Some (expify_lval ret_lval), locUnknown)) in
               (ret_stmt, [ret_var_varinfo_some])
        )
      in
      (ret_var_stmt, demarshlist);
    end

  method virtual stubgen_funs : fundec -> fundec -> fundec -> stmt list
    
  (* In a marshwrap function, this code generates the actual call to the function
     being wrapped.  This call takes place between the demarshaling code at the
     start of the function, and the marshaling code at the end of the function.
  *)
  method gen_marshwrap_call
    (callargs : exp list)
    (fdec_name : string)
    (fdec_type : typ)
    (marshwrap_func : fundec) : (stmt list * varinfo option) =
    let marshwrap_stmts = ref [] in

    (* Generate the return value stuff *)
    let (ret_var_type,_,_,_) = splitFunctionType fdec_type in
    let (ret_varinfo_opt, ret_var_opt) =
      match ret_var_type with
        | TVoid(_) -> None, None;
        | _ ->
            let ret_var_varinfo = get_local_variable marshwrap_func ret_var_type "_retval_" in
            (* Initialize any new pointer variables to NULL *)
            if isPointerType ret_var_varinfo.vtype then begin
              let retvar_init = Set((lvalify_varinfo ret_var_varinfo), zero64Uexp, locUnknown) in
              let stmt_retvar_init = mkStmt (Instr [retvar_init]) in
              marshwrap_stmts := List.append !marshwrap_stmts [stmt_retvar_init];
            end;
            Some(ret_var_varinfo), Some (Var(ret_var_varinfo),NoOffset);
    in
    (* First generate the function call to the original C function.
       This function call gets added to the trueblock of the if statement.
    *)
    let callfdec_fundec = emptyFunction fdec_name in
    let callfdec_func = expify_fundec callfdec_fundec in
    let callfdec = Call(ret_var_opt, callfdec_func, callargs, locUnknown) in
    let stmt_callfdec = mkStmt (Instr [callfdec]) in
    marshwrap_stmts := List.append !marshwrap_stmts [stmt_callfdec];
    (!marshwrap_stmts, ret_varinfo_opt);

  (** add_marshwrap_function: Add a marshaling wrapper for "interface"
   * functions that are implemented in the user space, or a marshaling
   * wrapper for functions with no fundec defined in the kernel.
   *)
  method add_marshwrap_function
    (fdec_fundec_opt : fundec option)
    (vinfo_opt : varinfo option)
      : fundec =
    begin
      let stmts = ref [] in
      let newroots = ref [] in
      let callargs = ref [] in
      let (name, marshwrap_funcname, marshwrap_func, fdec, fdec_typ, nofundec) =
        match fdec_fundec_opt with
          | None ->
              (match vinfo_opt with
                 | None ->
                     fatal ["Error add_marshwrap_function, specify one of the two params"]
                 | Some (vinfo) ->
                     let marshwrap_funcname = marshwrap_prefix ^ vinfo.vname in
                     let marshwrap_func = emptyFunction marshwrap_funcname in
                     let stripped_name = strip_stub_prefix vinfo.vname in
                     (stripped_name, marshwrap_funcname, marshwrap_func,
                      marshwrap_func, vinfo.vtype, true)
              );
          | Some (fdec_fundec) ->
              (match vinfo_opt with
                 | None ->
                     let marshwrap_funcname = marshwrap_prefix ^ fdec_fundec.svar.vname in
                     let marshwrap_func = emptyFunction marshwrap_funcname in
                     let stripped_name = strip_stub_prefix fdec_fundec.svar.vname in
                     (stripped_name, marshwrap_funcname, marshwrap_func,
                      fdec_fundec, fdec_fundec.svar.vtype, false)
                 | Some (vinfo) ->
                     fatal ["Error add_marshwrap_function, specify one of the two params"]
              );
      in
      let marshwrap_rettyp = TVoid([]) in
      let marshwrap_typsig = TFun(marshwrap_rettyp, Some([]), false, []) in
      setFunctionType marshwrap_func marshwrap_typsig;

      (* Add __MARSH_WRAP__ function parameters *)
      let marshwrap_arg_buf = makeFormalVar marshwrap_func "_buf_" voidPtrType in
      let marshwrap_arg_ret = makeFormalVar marshwrap_func "_ret_" 
        (TPtr (TComp(struct_marshret_compinfo, []), []))
      in

      (* 0. Call a generic function before we start demarshaling *)
      if gen_kern then
        stmts := !stmts @ [gen_basic_call "MARSHWRAP_LOC1"];

      (* 1. Create a new local variable to store offsets *)
      let offvar = makeLocalVar marshwrap_func "_off_" intType in

      (* 2. Create an initialization for the generic local variables *)
      let init_offvar_lhs = lvalify_varinfo offvar in
      let init_offvar_rhs = zero64Sexp in
      let init_offvar = Set(init_offvar_lhs, init_offvar_rhs, locUnknown) in
      let stmt_init_offvar = mkStmt (Instr [init_offvar]) in
      stmts := List.append !stmts [stmt_init_offvar];
      (* 3. Demarsh the roots *)
      (* 3.1 Get the ptg *)
      let ptg = Marshal_dri.get_flattened_ptg name false in
      (*(Printf.fprintf stderr "marshwrap::::: %s %d\n" name (Hashtbl.length ptg));*)
      let resfld = Marshal_dri.get_resolved_fields() in
      let resform = Marshal_dri.get_resolved_formals() in
      let globs = Marshal_dri.get_globals_accessed name in
      Printf.fprintf stderr "asim get globals for %s.\n" name; 
      let globs = remove_repeats globs in
      (* 3.2 Demarsh globals first *)
      for i = 0 to (List.length globs) - 1 do
        let ith = List.nth globs i in
        newroots := List.append !newroots [ith];
        let ithlv = lvalify_varinfo ith in
	(* Printf.fprintf stderr "Generating global stuff for %s.\n" ith.vname; *) 
        (* Use the marshbuf formal param during demarshaling *)
        stmts := List.append !stmts
          (unified_dfs ithlv ith.vtype marshwrap_func fdec
             ptg resfld resform marshwrap_arg_buf offvar 0
             false gen_kern);
      done;
      (* 3.3 For each formal variable of fdec, create a new local variable the
         new function. *)
      if nofundec = false then
        let roots = fdec.sformals in
        for i = 0 to (List.length roots) - 1 do
          let ith = List.nth roots i in
          let newvar = makeLocalVar marshwrap_func ith.vname ith.vtype in
          (* Initialize any new pointer variables to NULL *)
          if isPointerType newvar.vtype then
            begin
              let newvar_init = Set((lvalify_varinfo newvar), zero64Uexp, locUnknown) in
              let  stmt_newvar_init = mkStmt (Instr [newvar_init]) in
              stmts := List.append !stmts [stmt_newvar_init];
            end;
          newroots := List.append !newroots [newvar];
          callargs := List.append !callargs [newvar];
          let newvarlv = lvalify_varinfo newvar in
          let newvartyp = newvar.vtype in
          (* Use the marshbuf formal param during demarshaling *)
          stmts := List.append !stmts
            (unified_dfs newvarlv newvartyp marshwrap_func fdec
               ptg resfld resform marshwrap_arg_buf offvar 0
               false gen_kern);
        done;
      else
        begin
          (* In this case, we don't have any varinfos, so we have to construct
             them on the basis of the type definition *)
          let (_,argsopt,_,_) = splitFunctionType fdec_typ in
          let roots = Cil.argsToList argsopt in
          for i = 0 to (List.length roots) - 1 do
            let formalname = ref "" in
            let (ithstr,ithtyp,ithattr) = (List.nth roots i) in
            formalname := ithstr;
            if (String.compare !formalname "") = 0
            then formalname := ("arg" ^ (itoa i));
            let newvar = (makeLocalVar marshwrap_func !formalname ithtyp) in
            (* Initialize any new pointer variables to NULL *)
            if (isPointerType newvar.vtype) then begin
              let newvar_init = Set((lvalify_varinfo newvar), zero64Uexp, locUnknown) in
              let stmt_newvar_init = (mkStmt (Instr [newvar_init])) in
              stmts := (List.append !stmts [stmt_newvar_init]);
            end;
            newroots := (List.append !newroots [newvar]);
            callargs := (List.append !callargs [newvar]);
            let newvarlv = (lvalify_varinfo newvar) in
            let newvartyp = (newvar.vtype) in
            (* Use the marshbuf formal param during demarshaling *)
            stmts := (List.append !stmts
                        (unified_dfs newvarlv newvartyp marshwrap_func marshwrap_func
                           ptg resfld resform marshwrap_arg_buf offvar 0
                           false gen_kern));
          done;
        end;

      let marshwrap_arg_buf_lval = lvalify_varinfo marshwrap_arg_buf in
      let free_after_demarsh = gen_marshbuf_free "DEMARSHBUF_FREE2" marshwrap_arg_buf_lval in
      stmts := List.append !stmts [free_after_demarsh];
      (* 4.0 Check preconditions/postconditions *)
      (* 4.1 Set the execution context so that we can do effective assertion
         checking *)

      (* 4.2 Call a generic function before we call the actual implementation *)
      if gen_kern then
        stmts := !stmts @ [gen_basic_call "MARSHWRAP_LOC2"];

      (* We've just demarshaled everything.  Now we're going to call the
         actual implementation of the driver or kernel interface function *)
      (* argslist = the arguments to the interface function we're about to call.
         If we're in user mode, then we also pass those arguments to the
         "check_entry_point" routine.
      *)
      let lval_argslist = lvalify_varinfos !callargs in
      let exp_argslist = expify_varinfos !callargs in
      let (stmts_preconditions, stmts_postconditions) = 
        if gen_kern = false then
          (self#stubgen_preconditions fdec marshwrap_func lval_argslist,
           self#stubgen_postconditions fdec marshwrap_func lval_argslist)
        else
          [], []
      in
      (* 4.2 Call the function itself over here. *)
      let (stmts_new, retval_vi_opt) = self#gen_marshwrap_call
        exp_argslist name fdec_typ marshwrap_func in
      let stmts_new_wrapped =
        stmts_preconditions
        @ stmts_new
        @ stmts_postconditions in
      stmts := List.append !stmts stmts_new_wrapped;
      (match retval_vi_opt with
         | None -> ()
         | Some (retval_vi) -> newroots := List.append !newroots [retval_vi]
      );
      (* 4.3 Call interrupt handlers and USB completion routines. 
      if gen_kern = false then 
        begin
          let result1 = gen_call_empty "call_interrupt_handlers" in
          let result2 = gen_call_empty "execute_completions" in
          stmts := !stmts @ result1 @ result2;
        end;
      *)
      (* 4.3 Call a generic function before we begin marshaling *)
      if gen_kern then
        stmts := !stmts @ [gen_basic_call "MARSHWRAP_LOC3"];

      (* 5. Initialize the offset once again *)
      stmts := List.append !stmts [stmt_init_offvar];

      (* 6. Generate the marshaling code here. We've added the globals and
       * the return variable to newroots, so they will be automatically
       * marshaled at this point. *)
      for i = 0 to (List.length !newroots) - 1 do
        let ith = (List.nth !newroots i) in
        let ithlv = (lvalify_varinfo ith) in
        let ithtyp = (ith.vtype) in
        (* Marshal here *)
        stmts := List.append !stmts
          (unified_dfs ithlv ithtyp marshwrap_func fdec
             ptg resfld resform marshwrap_arg_buf offvar 0
             true gen_kern);
      done;
      (* 7. The return value is the structure marshret_struct. Initialize it
         appropriately at this point and return it. *)
      let star_marshwrap_arg_ret = (Mem((expify_lval (lvalify_varinfo marshwrap_arg_ret)))) in

      (* 7.1 Do _ret_->buf = _buf_. *)
      let marshwrap_arg_buf_exp = expify_lval (lvalify_varinfo marshwrap_arg_buf) in
      let buffield = marshret_buf in
      let star_marshwrap_arg_ret_buf = (star_marshwrap_arg_ret, Field(buffield, NoOffset)) in
      let stmt_setbuf = mkStmtOneInstr (Set (star_marshwrap_arg_ret_buf, marshwrap_arg_buf_exp, locUnknown)) in

      let offvar_exp = expify_lval (lvalify_varinfo offvar) in
      let lenfield = marshret_len in
      let star_marshwrap_arg_ret_len = (star_marshwrap_arg_ret, Field(lenfield, NoOffset)) in
      let stmt_setlen = (mkStmtOneInstr (Set (star_marshwrap_arg_ret_len, offvar_exp, locUnknown))) in

      (* 8. Call a generic function after we're done marshaling *)
      if gen_kern then
        stmts := !stmts @ [gen_basic_call "MARSHWRAP_LOC4"];

      stmts := List.append !stmts [stmt_setbuf; stmt_setlen];

      (*
        let free_after_marsh = (gen_marshbuf_free "MARSHBUF_FREE" marshwrap_arg_buf) in
        stmts := (List.append !stmts [free_after_marsh]);
      *)
      (*marshwrap_func.svar.vstorage <- Static; *)
      marshwrap_func.svar.vstorage <- NoStorage;
      
      marshwrap_func.sbody.bstmts <- !stmts;
      marshwrap_func;
    end

  method gen_stub_triplet
    (base_name : string)
    (fn_type : typ) 
    (fn: fundec)
    (g: global): 
    global list =
    begin
      let new_marshstub_name = stubmarsh_prefix ^ base_name in
      let new_demarshstub_name = stubdemarsh_prefix ^ base_name in
      let new_classicstub_name = base_name in
      
      let new_marshstub = gen_empty_stub new_marshstub_name fn_type 2 in
      let new_demarshstub = gen_empty_stub new_demarshstub_name fn_type 3 in
      let new_classicstub = gen_empty_stub new_classicstub_name fn_type 1 in
      
      Printf.fprintf stderr "Adding fundec for %s\n" new_marshstub_name;
      let new_marsh_stmts = self#stubgen_marsh_funs new_marshstub in
      new_marshstub.sbody.bstmts <- new_marsh_stmts;
      
      Printf.fprintf stderr "Adding fundec for %s\n" new_demarshstub_name;
      let new_demarsh_stmts = self#stubgen_demarsh_funs new_demarshstub in
      new_demarshstub.sbody.bstmts <- new_demarsh_stmts;
      
      Printf.fprintf stderr "Adding fundec for %s\n" new_classicstub_name;
      let new_classic_stmts = self#stubgen_funs new_classicstub new_marshstub new_demarshstub in
      new_classicstub.sbody.bstmts <- new_classic_stmts;
   

      let new_marshglobal = GFun(new_marshstub, locUnknown) in
      let new_demarshglobal = GFun(new_demarshstub, locUnknown) in
      let new_classicglobal = GFun(new_classicstub, locUnknown) in
    
      (* ASIM Remove below statement to generate single copy of functions
       * in kernel *)	 
      if (gen_kern = true) then (
	(*  Change name of original function throughout the file. *)
	fn.svar.vname <- "odft_sfi" ^ fn.svar.vname;

	(* Generate a tap for odft/non-odft with an extern function *)

	(* Step 1: Create a svar for extern function *)
	let ifsfi_fundec = (emptyFunction "odft_function_to_sfi" ) in
	let check_var = (UnOp(LNot, (expify_fundec ifsfi_fundec) , intType)) in
	(* Step 2: Generate check code with this svar *)
	


	(* Step 3: Add these statements to the body of code *)
 
	[new_marshglobal; new_demarshglobal; new_classicglobal; g]
      )	
      else	
      [new_marshglobal; new_demarshglobal; new_classicglobal;];	
      end

  (* User mode specific.
   * Creates new fundecs for functions that are called in the slave (user
   * daemon), but do not have fundecs. The transformation is as follows:
   * For each function USER_FOO called in the user-daemon:
   *      XFORM := ADD a body of the form USER_FOO(X) { FOO(X) },
   * and to add this to the globals of the file. This method just creates
   * the toplevel fundec for the function without the fundec, and calls
   * user_stubgen to generate the body. *)
  method user_add_new_fundecs
    (globals : global list) : global list =
    begin
      let new_stubs = list_bindings funcs_with_no_fundecs in
      (* For each function that is called, but does not have a fundec, create a
         fundec for that function with the same type signature *)
      let new_globals = ref [] in
      for i = 0 to (List.length new_stubs) - 1 do
        let ith = List.nth new_stubs i in
        let base_name = ith.vname in
        (* Don't add anything if the function is on our list
           of those not to bother with, e.g. sprintf
	   *)
	  	
          let dfdec = emptyFunction "ASIMBUG" in
          let dglob = GFun(dfdec,locUnknown) in
	
        if (should_split nonstubbed_functions base_name) = true then
	new_globals := !new_globals @ (self#gen_stub_triplet base_name ith.vtype dfdec dglob);
	done;

      globals @ !new_globals;
    end

  method kernel_add_new_fundecs (globals : global list) : global list =
    begin
      let new_globals = ref [] in
      for i = 0 to (List.length globals) - 1 do
        let ith = List.nth globals i in
        match ith with
          | GFun (fdec, _) ->
              if (is_interface_function interface_functions fdec.svar.vname) = false then
                new_globals := !new_globals @ [ith]
              else
                begin
                  if (should_split nonstubbed_functions fdec.svar.vname) = true then
                    new_globals := !new_globals @ (self#gen_stub_triplet fdec.svar.vname fdec.svar.vtype fdec ith) ;
                    Printf.fprintf stderr "Deleting interface function %s\n" fdec.svar.vname;
                end
          | _ -> new_globals := !new_globals @ [ith];
      done;
      
      !new_globals;
    end

  (* Mode = true:  Delete body, prototype, everything.
     Mode = false:  Delete body contents only, keep prototype
  *)
  method kernel_remove_nonstub (globals: global list) (mode: bool) : global list =
    begin
      let newglobals = ref [] in
      for i = 0 to (List.length globals) - 1 do
        let ith = (List.nth globals i) in
        let result = 
          match ith with
            | GVarDecl(vi,_) ->
                if mode then (* Delete everything *)
                  begin
                    match vi.vtype with
                      | TFun(_,_,_,_) ->
                          begin
                            try
                              if should_split nonstubbed_functions vi.vname = false then
                                begin
                                  ignore (Hashtbl.find non_interface_functions vi.vname);
                                  Printf.fprintf stderr "removing %s from kern\n" vi.vname;
                                  []
                                end
                              else
                                [ith];
                            with Not_found ->
                              [ith];
                          end
                      | _ -> 
                          [ith];
                  end
                else (* Keep it *)
                  [ith];
            | GFun(fdec,_) ->
                begin
                  try
                    ignore (Hashtbl.find non_interface_functions fdec.svar.vname);
                    Printf.fprintf stderr "removing %s from kern\n" fdec.svar.vname;
                    if mode = false then
                      begin
                        (* Delete body only but keep the empty function *)
                        let panic_fdec = expify_fundec (emptyFunction "panic") in
                        let panic_params = [mkString ("Don't call this: " ^ fdec.svar.vname)] in
                        let panic_stmt = Call (None, panic_fdec, panic_params, locUnknown) in
                        let panic_instr = Instr ([panic_stmt]) in
                        let new_panic = mkStmt panic_instr in
                        fdec.sbody <- mkBlock [new_panic];
                        [ith]
                      end
                    else
                      []
                  with Not_found -> 
                    [ith]
                end
            | _ -> [ith];
        in
        newglobals := !newglobals @ result
      done;
      !newglobals;
    end

  (** Main visitor method for same_address_space xform that visits each
   * function. Leaves functions in kernel space untouched. User functions
   * are stubbed. Formally, the transformation that the kernel vfunc
   * implements is:
   *
   * if foo has "kern" annotation
   *      XFORM := foo(X){Body} -> foo(X){Body}
   * if foo has "user" annotation
   *      XFORM := foo(X){Body} -> foo(X){return __stub__foo(X)}
   *
   * For user-mode vfunc, the transformation is:
   * if foo has "kern" annotation
   *     XFORM := __stub__foo(X){Body} -> __stub__foo(X){call foo}
   * if foo has "user" annotation
   *     XFORM := __stub__foo(X){Body} -> __stub__foo(X){Body}
   * For "user" annotation functions that are interface functions,
   *     __stub__foo(X){body} ADD __marshwrap__foo(X){demarsh,call,marsh}
   *)
  method vfunc (f: fundec) : fundec visitAction =
    begin
      (* This code must only run on non-prefixed functions,
         i.e. ones we didn't generate
      *)
      let (prefix, base_name) = strip_stub_prefix_pair f.svar.vname in
      if (String.compare prefix "") != 0 then
        begin
          Printf.fprintf stderr "vfunc on \"%s\", \"%s\", \"%s\"\n" prefix base_name f.svar.vname;
          fatal ["Fatal error!"];
        end;

      (* Check and split *)
      if (should_split nonstubbed_functions f.svar.vname) = true then
        let annot = 
          try
            Hashtbl.find annotations f.svar.vname
          with Not_found -> fatal ["Fatal error in vfunc in splitter_kern"];
        in
        let basic_annot_name = if gen_kern then "kern" else "user" in
        if annot = basic_annot_name then
          begin
            (* kern: If the kernel function is an interface function and could be
               called by the user-space, do not make it static. Otherwise, make it
               static. Leave untouched otherwise. *)
            (* user: If it is an interface function, do not make it static.*) 
            if (is_interface_function interface_functions f.svar.vname) = true then
              begin
                Printf.fprintf stderr "adding %s to interface\n" f.svar.vname;
                nostorage_funcs <- add_to_list f nostorage_funcs;

                let result = self#add_marshwrap_function (Some (f)) None in
                marshwrap_funcs <- add_to_list result marshwrap_funcs;
              end;
              
            DoChildren;
          end
        else
          begin
            (* Interface functions that are implemented in the user-level
               are stubbed. The stub name should be made static. *)
            (* annot = kern: Change function body to have a call to the function in the
             * kernel side *)
            (* annot = user: Generate stub. *)
            if (is_interface_function interface_functions f.svar.vname) = false then
              Hashtbl.add non_interface_functions f.svar.vname f
            else
              if gen_kern = false then
                begin
                  (* If its address is taken, it should not be a static function *)
                  if (f.svar.vaddrof = true)
                  then nostorage_funcs <- add_to_list f nostorage_funcs;
                end;
            DoChildren;
          end;
      else
        DoChildren;
    end;

  (* Top-level function for the kernel *)
  method top_level_kernel
    (f: file)
    (annot_file : string)
    (addr_taken : pass_find_addr_taken)
    (modif_const : pass_modif_const)
    : unit =
    begin
      gen_kern <- true;

      initialize_nooks true;
      populate_annotations annotations annot_file;
      
      populate_nonstubbed_functions
        nonstubbed_functions nonstubbed_functions_delete_proto
        override_interface 1;
      
      populate_interface_functions
        f annotations funcs_with_no_fundecs override_interface interface_functions;

      let obj_fix_storage : pass_fix_storage = new pass_fix_storage in
      obj_fix_storage#top_level f gen_kern nonstubbed_functions;
      
      populate_globals_to_register f.globals (modif_const#get_strings_varinfos());
      let fn_addr_taken_defn = addr_taken#get_defns () in
      let fn_addr_taken_decl = addr_taken#get_decls () in
      let const_strings = modif_const#get_strings_varinfos () in
      populate_function_id_mapping annotations
        fn_addr_taken_defn fn_addr_taken_decl
        const_strings;

      (* Transform all string constants into modifiable variables *)
      let obj_pass_function_id = new pass_function_id_map in
      obj_pass_function_id#top_level f;
      
      visitCilFile (self :> cilVisitor) f;
      populate_execution_mode_map annotations interface_functions;
      
      for i = 0 to (List.length nostorage_funcs) - 1 do
        let ith = (List.nth nostorage_funcs i) in
        ith.svar.vstorage <- NoStorage;
      done;

      f.globals <- self#kernel_add_new_fundecs f.globals;

      (* Add a prototype for the disp_user function *)
      let (disp_user_fundec, _, _) = get_disp_userkern_fundec "disp_user" in
      let disp_user_varinfo = disp_user_fundec.svar in
      f.globals <-
        List.append [GVarDecl((disp_user_varinfo),locUnknown)] f.globals;
      (* Add a declaration for the req_args structure that we defined *)
      f.globals <-
        List.append [GCompTag(struct_reqargs_compinfo,locUnknown)] f.globals;
      (* Add a declaration for the marshret structure that we defined *)
      f.globals <-
        List.append [GCompTag(struct_marshret_compinfo,locUnknown)] f.globals;

      (* Add a marshwrap function for nofundec functions. Exclude VARARG
         fucntions from this, as they have a different function for marshwrap
         generation. *)
      let nofundecs = list_bindings funcs_with_no_fundecs in
      for i = 0 to (List.length nofundecs) - 1 do
        let ith = (List.nth nofundecs i) in
        if (is_interface_function interface_functions ith.vname) = false then
          add_if interface_functions ith.vname true;
        let newmarshwrap = self#add_marshwrap_function None (Some (ith)) in
        (* Without this test, we get code for sprintf.  sprintf
           is a kernel function, and we don't a fundec.
           It's also listed in our "no stub" table.
        *)
        if (should_split nonstubbed_functions ith.vname) = true then
          marshwrap_funcs <- add_to_list newmarshwrap marshwrap_funcs;
      done;

      (* Add functions for marshaling individual data structures *)
      (*self#add_sync_functions f;*)

      (* Add all marshwrap functions to globals *)
      for i = 0 to (List.length marshwrap_funcs) - 1 do
        let ith = (List.nth marshwrap_funcs i) in
        f.globals <- List.append f.globals [GFun(ith,locUnknown)];
      done;

      (* Generate the disploop function *)
      let disploop = gen_disploop gen_kern marshwrap_funcs in
      f.globals <- List.append f.globals [GFun(disploop,locUnknown)];

      (* Generate the registration function *)
      let registerfn = gen_registerfn fn_addr_taken_defn fn_addr_taken_decl interface_functions in
      f.globals <- List.append f.globals [GFun(registerfn,locUnknown)];

      (* Generate the list of globals to be registered *)
      let globs_to_reg = get_globals_to_register () in
      let registerglob = gen_registerglob globs_to_reg in
      f.globals <- List.append f.globals [GFun(registerglob,locUnknown)];

      (* NOTE: The following is an optimization that removes unwanted stubs
       * i.e., those that are never called, from the kernel.
       *)
      (* Remove the non_interface_functions from the kernel's code *)
      f.globals <- self#kernel_remove_nonstub f.globals false;

      (* Gets rid of some unused temporaries that'd we created previously *)
      Rmtmps.removeUnusedTemps f;
    end

  (** Top-level function to implement user pass 2 *)
  method top_level_user
    (f: file)
    (annot_file : string)
    (entry_points_file : string)
    (addr_taken : pass_find_addr_taken)
    (modif_const : pass_modif_const)
    : unit =
    begin
      gen_kern <- false;

      initialize_nooks false;
      populate_entry_points entry_points_file entry_points;
      
      populate_annotations annotations annot_file;
      populate_nonstubbed_functions
        nonstubbed_functions nonstubbed_functions_delete_proto
        override_interface 0;
      populate_interface_functions
        f annotations funcs_with_no_fundecs override_interface interface_functions;

      let obj_fix_storage : pass_fix_storage = new pass_fix_storage in
      obj_fix_storage#top_level f gen_kern nonstubbed_functions;
      
      (* Delete all non-stub function bodies.  By definition, they should not
         be defined, as we are defining them ourselves *)
      f.globals <- delete_nonstubs
        f.globals
        nonstubbed_functions
        nonstubbed_functions_delete_proto;

      populate_globals_to_register f.globals (modif_const#get_strings_varinfos());
      let fn_addr_taken_defn = addr_taken#get_defns () in
      let fn_addr_taken_decl = addr_taken#get_decls () in
      let const_strings = modif_const#get_strings_varinfos () in
      populate_function_id_mapping annotations
        fn_addr_taken_defn fn_addr_taken_decl
        const_strings;

      (* Transform all string constants into modifiable variables *)
      let obj_pass_function_id = new pass_function_id_map in
      obj_pass_function_id#top_level f;

      visitCilFile (self :> cilVisitor) f;
      populate_execution_mode_map annotations interface_functions;
      
      f.globals <- self#user_add_new_fundecs f.globals;

      for i = 0 to (List.length nostorage_funcs) - 1 do
        let ith = List.nth nostorage_funcs i in
        ith.svar.vstorage <- NoStorage;
      done;

      (* Add a prototype for the disp_kern function *)
      let (disp_kern_fundec, _, _) = get_disp_userkern_fundec "disp_kern" in
      let disp_kern_varinfo = disp_kern_fundec.svar in
      f.globals <-
        List.append [GVarDecl((disp_kern_varinfo),locUnknown)] f.globals;
      (* Add a declaration for the req_args structure that we defined *)
      (* Not sure if we need this declaration in the user-daemon, but it's
         harmless to have it there, so put it anyway *)
      f.globals <-
        List.append [GCompTag(struct_reqargs_compinfo,locUnknown)] f.globals;
      (* Add a declaration for the marshret structure that we defined *)
      f.globals <-
        List.append [GCompTag(struct_marshret_compinfo,locUnknown)] f.globals;

      (* Add all marshwrap functions to globals *)
      for i = 0 to (List.length marshwrap_funcs) - 1 do
        let ith = List.nth marshwrap_funcs i in
        f.globals <- List.append f.globals [GFun(ith,locUnknown)];
      done;

      (* Add the dispatch loop function *)
      let disploop = gen_disploop gen_kern marshwrap_funcs in
      f.globals <- List.append f.globals [GFun(disploop,locUnknown)];

      (* Add the function registration function *)
      let registerfn = gen_registerfn fn_addr_taken_defn fn_addr_taken_decl interface_functions in
      f.globals <- List.append f.globals [GFun(registerfn,locUnknown)];

      (* Add the bodies of functions whose addresses are taken but for which
         we don't have a definition, e.g. kernel functions *)
      let kernel_fn_dummies = gen_dummy_bodies interface_functions fn_addr_taken_defn fn_addr_taken_decl in
      f.globals <- List.append f.globals kernel_fn_dummies;

      (* Generate the list of globals to be registered *)
      let globs_to_reg = get_globals_to_register () in
      let registerglob = gen_registerglob globs_to_reg in
      f.globals <- List.append f.globals [GFun(registerglob,locUnknown)];

      (* No globals in the slave should have extern *)
      f.globals <- make_globals_nonextern f.globals; 

      (* Gets rid of some unused temporaries that'd we created previously *)
      Rmtmps.removeUnusedTemps f;
    end
end

let generate_check_code (lv: lval) : stmt =
     begin
       (* let check_var = (UnOp(LNot, (expify_lval lv) , voidPtrType)) in*)
       let check_var =  (expify_lval lv) in
       let false_block = (mkBlock [(mkEmptyStmt ())]) in
       let true_block  = (mkBlock
       [(mkStmt(Return(Some(Const(CInt64((Int64.of_int 75),IInt,None))),
       locUnknown)))]) in
       let new_stmt = (mkStmt (If (check_var, true_block, false_block,
       locUnknown))) in
       new_stmt;
     end

let mkProto (name: string) (args: (string * typ * attributes) list) =
  let fdec = emptyFunction name in
  fdec.svar.vtype <- TFun(intType,
                          Some args, false, []);
  fdec

class splitter_normal = object (self)
  inherit splitter_unified
    (** stubgen_funs: Stub generation *)
  method stubgen_preconditions (f: fundec) (marshstub: fundec) (argslist: lval list) : stmt list = []
  method stubgen_postconditions (f: fundec) (marshstub: fundec) (argslist: lval list): stmt list = []

  method private stubgen_main
    (f: fundec)
    (marshstub: fundec)
    (demarshstub: fundec)
    : stmt list =
    begin
  
      Printf.fprintf stderr "Processing %s.\n" f.svar.vname;	
      let stmts_if_demarsh = ref (mkEmptyStmt ()) in
      (* Marshal everything *)
      let stmts_marshal = self#stubgen_m_dm_call f marshstub in
      (* Transfer control *)
      let stmts_switch = self#stubgen_switch_stmts f in
     
      if gen_kern then
	      stmts_if_demarsh :=  generate_check_code (lvalify_varinfo (get_local_variable f intType "__odft_disp_error")); 

      (* Demarshal everything *)
      let stmts_demarshal = self#stubgen_m_dm_call f demarshstub in
      (* Free the allocated memory *)
      let stmts_free = self#stubgen_free_call f "DEMARSHBUF_FREE" in
      stmts_marshal @ stmts_switch @ [!stmts_if_demarsh] @ stmts_demarshal @ stmts_free
    end

  method stubgen_funs
    (f: fundec)
    (marshstub : fundec)
    (demarshstub: fundec) :
    stmt list =
    begin
      (* See if this function can be handled *)
      let (_,_,isvararg,_) = splitFunctionType f.svar.vtype in
      if isvararg
      then addwarn ["VARARGS not supported in stubgen_marsh_funs for fn:";
                    f.svar.vname;
                    "; any varargs are not marshaled"];
      
      (* Make sure all formal parameters have names.  Sometimes,
         the function prototype is provided without variable names--
         in these cases, we make some up. *)
      ensure_formals_have_names f;

      (* Write some initial statements for the kernel -- sync *)
      let uniq_init_stmts = 
        if gen_kern then 
          [gen_basic_call "STUB_LOC1"]
        else
          []
      in
      let init_stmts = (self#stubgen_rqarg_init_zero f) @ uniq_init_stmts in
      let main_stmts = self#stubgen_main f marshstub demarshstub in
      (* Create the list of statements to return *)
      let uniq_final_stmts = 
        if gen_kern then
          [gen_basic_call "STUB_LOC2"]
        else
          []
      in
      let (stmt_ret, _) = self#stubgen_demarsh_retstmt f in
      let ret_stmts =
        init_stmts
        @ main_stmts
        @ uniq_final_stmts
        @ [stmt_ret] in
      ret_stmts;
    end
end

class splitter_symbolic = object (self)
  inherit splitter_unified

  method private stubgen_include_checks (f: fundec) : bool =
    let strings = [
      "pci_choose_state"; "suspend_late_pci";
      "suspend_pci"; "suspend_usb";
      "hrtimer_forward"; "hrtimer_start";
      "kernel_vmap"
    ] in
    not (List.mem f.svar.vname strings)

  (* These methods are for assertion checking.
     They simply call an exported function to do the checking--very simple.
     The alternative is to generate assertion checking code here, but this
     would be extremely tedious.
  *)
  (* TODO: be sure to initialize the _retval_ variable along all paths--sometimes it's passed
     in to pre condition testing but remains uninitialized.  That's bad. *)
  method stubgen_preconditions (f: fundec) (marshstub: fundec) (argslist: lval list) : stmt list =
    if !do_symdriver_test = true && self#stubgen_include_checks f then
      self#stubgen_entry_point f marshstub 0 argslist
    else
      []
    
  method stubgen_postconditions (f: fundec) (marshstub: fundec) (argslist: lval list): stmt list =
    if !do_symdriver_test = true && self#stubgen_include_checks f then
      self#stubgen_entry_point f marshstub 1 argslist
    else
      []

  method private stubgen_entry_point
    (f: fundec)
    (marshstub: fundec)
    (condition: int)
    (lval_argslist: lval list) : stmt list =
    let add_addr_of(lv: lval) : exp = mkCast (mkAddrOf lv) voidPtrType in
    let ep_addr_of_argslist = List.map add_addr_of lval_argslist in
    let fname = f.svar.vname in
    let (context_string, ep_remaining_args, mode) =
      try
        Hashtbl.find entry_points fname, ep_addr_of_argslist, true
      with Not_found ->
        try
          let kernel_vi = Hashtbl.find funcs_with_no_fundecs fname in
          kernel_vi.vname, ep_addr_of_argslist, true
        with Not_found ->
          ("default", [], false)
    in
    let ep_arg1 = Const (CStr (f.svar.vname)) in
    let ep_arg2 = Cil.integer condition in
    let (ep_arg3_typ,_,_,_) = splitFunctionType f.svar.vtype in
    let ep_arg3 =
      match ep_arg3_typ with
        | TVoid(_) -> [];
        | _ ->
            if (String.compare "default" context_string) <> 0 then
              begin
                let ep_arg3_vi = get_local_variable marshstub ep_arg3_typ "_retval_" in
                let ep_arg3_lval = lvalify_varinfo ep_arg3_vi in
                [expify_lval ep_arg3_lval]
              end
            else
              []
    in
    let ep_args = [ep_arg1; ep_arg2] @ ep_arg3 @ ep_remaining_args in
    let ep_fn_name = context_string ^ "_check" in
    let ep_fundec = emptyFunction ep_fn_name in
    let ep_exp = expify_fundec ep_fundec in
    let ep_instr = Call(None,
                        ep_exp,
                        ep_args,
                        locUnknown) in
    let ep_call = Instr([ep_instr]) in
    let ep_stmt = mkStmt ep_call in
    if mode then
      begin
        let (str_retval_typ, _) = self#stubgen_demarsh_retval f in
        let str_retval = typ_tostring str_retval_typ in
        let str_args = strip_whitespace (get_params_str_fundec f) in
        let fmt = format_of_string
          "CHECK_PROTO: void %s(const char *fn, int prepost, %s retval, %s);\n" in
        let fmt_result = Printf.sprintf fmt ep_fn_name str_retval str_args in
        Printf.fprintf stderr "%s" fmt_result;
      end;
    [ep_stmt];

  method private stubgen_main_kernel
    (f: fundec)
    (marshstub: fundec)
    (demarshstub: fundec)
    : stmt list =
    begin
        
      (* Marshal everything *)
      let stmts_marshal = self#stubgen_m_dm_call f marshstub in
      (* Transfer control *)
      let stmts_switch = self#stubgen_switch_stmts f in
      (* Demarshal everything *)
      let stmts_demarshal = self#stubgen_m_dm_call f demarshstub in
      (* Free the allocated memory *)
      let stmts_free = self#stubgen_free_call f "DEMARSHBUF_FREE" in

      let stmts_main = stmts_marshal @ stmts_switch @ stmts_demarshal @
      stmts_free in 
      (* Printf.fprintf stderr "stubgen_main_kernel:generated kern %s."
      (stmtlist_tostring stmts_demarshal);*)
      stmts_main;
     

    end

  (* Stuff below commented by Asim *)
  method private stubgen_main_user
    (f: fundec)
    (marshstub: fundec)
    (demarshstub: fundec)
    : stmt list =
    begin
      let (execution_mode_stmt, execution_mode_vi) = self#stubgen_execution_mode_call f in 
      let if_concrete_stmt = self#stubgen_if_concrete f execution_mode_vi marshstub demarshstub in
(*      let if_replay_stmt = self#stubgen_if_replay f execution_mode_vi
 *      marshstub demarshstub in *)
      let if_replay_stmt = [] in
      let if_symbolic_stmt = self#stubgen_if_symbolic f execution_mode_vi marshstub demarshstub in
      let if_symonly_stmt = self#stubgen_if_symonly f execution_mode_vi marshstub demarshstub in
(*
      let argslist = lvalify_varinfos f.sformals in
      let preconditions = self#stubgen_preconditions f f argslist in
      let postconditions = self#stubgen_postconditions f f argslist in
      let call_irqs = gen_call_empty "call_interrupt_handlers" in
      let call_completions = gen_call_empty "execute_completions" in
*)
    (*    preconditions @ *)
        execution_mode_stmt @
        if_concrete_stmt @
        if_replay_stmt @
        if_symbolic_stmt @
        if_symonly_stmt (*@
        postconditions @
        call_irqs @
        call_completions   *)
    end

  method private stubgen_if_concrete
    (f: fundec)
    (execution_mode: varinfo)
    (marshstub: fundec)
    (demarshstub: fundec)
    : stmt list =
    begin
      (* Marshal everything *)
      let stmts_marshal = self#stubgen_m_dm_call f marshstub in
      (* Transfer control *)
      let stmts_switch = self#stubgen_switch_stmts f in
      (* Demarshal everything *)
      let stmts_demarshal = self#stubgen_m_dm_call f demarshstub in
      (* Free the allocated memory *)
      let stmts_free = self#stubgen_free_call f "DEMARSHBUF_FREE" in

      (* Wrap it up in an if statement *)
      let stmts_true = stmts_marshal @ stmts_switch @ stmts_demarshal @ stmts_free in
       self#stubgen_execution_mode_if f execution_mode "SYMEXEC_CONCRETE"
        stmts_true; 
    end

  method private stubgen_if_replay
    (f: fundec)
    (execution_mode: varinfo)
    (marshstub: fundec)
    (demarshstub: fundec)
    : stmt list =
    begin
      (* Marshal everything *)
      let stmts_marshal = self#stubgen_m_dm_call f marshstub in
      (* Transfer control *)
      let stmts_switch = self#stubgen_switch_stmts f in
      (* Demarshal and free everything *)
      let stmts_demarsh_body = self#stubgen_m_dm_call f demarshstub in
      let stmts_demarsh_free = self#stubgen_free_call f "DEMARSHBUF_FREE_STD" in
      let stmts_demarsh_body_free = stmts_demarsh_body @ stmts_demarsh_free in
      let (ret_var_type, ret_var_vi_opt) = self#stubgen_demarsh_retval f in
      let ret_var_lval_opt =
        match ret_var_vi_opt with
          | None -> None
          | Some(ret_var_vi_some) -> Some(lvalify_varinfo ret_var_vi_some)
      in
      let stmts_demarsh_all = gen_disp_kern_blocker f ret_var_type ret_var_lval_opt stmts_demarsh_body_free [] in

      (* Generate the main code for replay *)
      let stmts_marshal_switch = stmts_marshal @ stmts_switch in
      let stmts_composite_marshal_switch = self#stubgen_if_replay_body f stmts_marshal_switch in

      (* Wrap it up in an if statement *)
      let stmts_true = stmts_composite_marshal_switch @ stmts_demarsh_all in
       self#stubgen_execution_mode_if f execution_mode "SYMEXEC_REPLAY"
        stmts_true; 
    end

  method private stubgen_if_replay_body
    (f: fundec)
    (stmts_marshal_switch: stmt list)
    : stmt list =
    begin
      let rqarg_vi = get_local_variable f struct_reqargs_typ "rqarg" in
      let rqarg_lval = lvalify_varinfo rqarg_vi in
      let rqarg_param = mkAddrOf rqarg_lval in
      
      let check_buffer_fundec = emptyFunction "get_function_buffer" in
      let check_buffer_exp = expify_fundec check_buffer_fundec in
      let check_buffer_argument1 = integer (get_function_id f.svar.vname) in
      let check_buffer_argument2 = rqarg_param in
      let check_buffer_arguments = [check_buffer_argument1; check_buffer_argument2] in
      let check_buffer_instr = Call(None,
                                    check_buffer_exp,
                                    check_buffer_arguments,
                                    locUnknown) in
      let check_buffer_call = Instr([check_buffer_instr]) in
      let check_buffer_stmt = mkStmt check_buffer_call in

      let reg_fundec = emptyFunction "register_function_buffer" in
      let reg_exp = expify_fundec reg_fundec in
      let reg_argument1 = integer (get_function_id f.svar.vname) in
      let reg_argument2 = rqarg_param in
      let reg_arguments = [reg_argument1; reg_argument2] in
      let reg_instr = Call(None,
                           reg_exp,
                           reg_arguments,
                           locUnknown) in
      let reg_call = Instr([reg_instr]) in
      let reg_stmt = mkStmt reg_call in

      let true_block_stmts = stmts_marshal_switch @ [reg_stmt] in
      let true_block = mkBlock true_block_stmts in
      let false_block_stmts = [check_buffer_stmt] in
      let false_block = mkBlock false_block_stmts in
      let if_stmt = gen_disp_kern_allowed f true_block false_block in
      let all_stmts = if_stmt in
      all_stmts;
    end

  method private stubgen_if_symbolic
    (f: fundec)
    (execution_mode: varinfo)
    (marshstub: fundec)
    (demarshstub: fundec)
    : stmt list =
    begin
      (* Marshal everything *)
      let stmts_marshal = self#stubgen_m_dm_call f marshstub in
      
      (* Transfer control *)
      let stmts_switch = self#stubgen_switch_stmts f in
     
      (* Demarshal everything *)
      let stmts_demarshal = self#stubgen_m_dm_call f demarshstub in
      (* Free the allocated memory *)
      let stmts_free = self#stubgen_free_call f "DEMARSHBUF_FREE" in

      (* Generate the main code for symbolic mode *)
      let stmts_marshal_switch = stmts_marshal @ stmts_switch in
      let stmts_composite_marshal_switch = self#stubgen_if_symbolic_body f stmts_marshal_switch in

      (* Wrap it up in an if statement *)
      let stmts_true = stmts_composite_marshal_switch @ stmts_demarshal @ stmts_free in
       self#stubgen_execution_mode_if f execution_mode "SYMEXEC_IFNEEDED"
        stmts_true;
    end

  method private stubgen_if_symbolic_body
    (f: fundec)
    (stmts_marshal_switch: stmt list)
    : stmt list =
    begin
      let true_block_stmts = stmts_marshal_switch in
      let true_block = mkBlock true_block_stmts in
      let false_block = mkBlock [] in
      let if_stmt = gen_disp_kern_allowed f true_block false_block in
      let all_stmts = if_stmt in
      all_stmts;
    end

  method private stubgen_if_symonly
    (f: fundec)
    (execution_mode: varinfo)
    (marshstub: fundec)
    (demarshstub: fundec)
    : stmt list =
    begin
      (* Demarshal everything *)
      let stmts_demarshal = self#stubgen_m_dm_call f demarshstub in
      (* Free the allocated memory *)
      let stmts_free = self#stubgen_free_call f "DEMARSHBUF_FREE" in

      (* Wrap it up in an if statement *)
      let stmts_true = stmts_demarshal @ stmts_free in
       self#stubgen_execution_mode_if f execution_mode "SYMEXEC_FULL"
        stmts_true; 
    end

  method private stubgen_execution_mode_call (f: fundec)
    : (stmt list * varinfo) =
    begin
      let execution_mode_result = get_local_variable f intType "sym_execution_mode" in
      let execution_mode_result_lval = lvalify_varinfo execution_mode_result in
      let execution_mode_args = [Const (CStr (f.svar.vname))] in
      let execution_mode_fundec = emptyFunction "execution_mode" in
      let execution_mode_exp = expify_fundec execution_mode_fundec in
      let execution_mode_instr = Call(Some(execution_mode_result_lval),
                                      execution_mode_exp,
                                      execution_mode_args,
                                      locUnknown) in
      let execution_mode_call = Instr([execution_mode_instr]) in
      let execution_mode_stmt = mkStmt execution_mode_call in
      ([execution_mode_stmt], execution_mode_result);
    end

  method private stubgen_execution_mode_if
    (f: fundec)
    (execution_mode: varinfo)
    (cmp: string)
    (stmts_true: stmt list)
    : stmt list =
    begin
      let if_lhs = expify_lval (lvalify_varinfo execution_mode) in
      let if_rhs_vi = makeVarinfo false cmp (TInt (IInt, [])) in
      let if_rhs = expify_lval (lvalify_varinfo if_rhs_vi) in
      let if_exp = BinOp(Eq, if_lhs, if_rhs, TInt(IInt, [])) in
      let if_true_block = mkBlock stmts_true in
      let if_false_block = mkBlock [] in
      let if_stmt = mkStmt (If(if_exp, if_true_block, if_false_block, locUnknown)) in
      [if_stmt];
    end


  (** stubgen_funs: Stub generation *)
  method stubgen_funs
    (f: fundec)
    (marshstub : fundec)
    (demarshstub: fundec) :
    stmt list =
    begin
      (* See if this function can be handled *)
      let (_,_,isvararg,_) = splitFunctionType f.svar.vtype in
      if isvararg
      then addwarn ["VARARGS not supported in stubgen_marsh_funs for fn:";
                    f.svar.vname;
                    "; any varargs are not marshaled"];
      
      (* Make sure all formal parameters have names.  Sometimes,
         the function prototype is provided without variable names--
         in these cases, we make some up. *)
      ensure_formals_have_names f;

      (* Write some initial statements for the kernel -- sync *)
      let uniq_init_stmts = 
        if gen_kern then 
          [gen_basic_call "STUB_LOC1"]
        else
          []
      in
      let init_stmts = (self#stubgen_rqarg_init_zero f) @ uniq_init_stmts in

      let main_stmts =
        if gen_kern then
          self#stubgen_main_kernel f marshstub demarshstub
        else
          self#stubgen_main_user f marshstub demarshstub
      in
     
      (* Create the list of statements to return *)
      let uniq_final_stmts = 
        if gen_kern then
          [gen_basic_call "STUB_LOC2"]
        else
          []
      in
      let (stmt_ret, _) = self#stubgen_demarsh_retstmt f in
      let ret_stmts =
        init_stmts
        @ main_stmts
        @ uniq_final_stmts
        @ [stmt_ret] in
      ret_stmts;
    end
end


(*---------------------------------------------------------------------------*)
(** Main function to call the splitter. *)
let do_splitting (f: file)
    (annot_file: string)
    (entry_points_file: string)
    (user_kern: string)
    (feature_sym_normal: string)
    : unit =
  begin
    feature_mode := feature_sym_normal;
    
    (* Pass 1:  just find functions whose addresses are taken *)
    let obj_find_addr_taken = new pass_find_addr_taken in
    obj_find_addr_taken#top_level f;

    (* Transform all string constants into modifiable variables *)
    let obj_pass_modif_const = new pass_modif_const in
    obj_pass_modif_const#top_level f;

    let is_ker = ref 0 in  
    begin
      match user_kern with
        | "produce-kern" ->
                is_ker := 1;    
            let obj_kern : splitter_unified =
              if (String.compare !feature_mode "normal") = 0 then new splitter_normal
               else if (String.compare !feature_mode "sym") = 0 then new
                splitter_symbolic 
              else fatal["Unknown feature mode specified"]
            in
            obj_kern#top_level_kernel f annot_file obj_find_addr_taken obj_pass_modif_const;
            infomsg ["Produced kernel side of the split."];
        | "produce-user" ->
                is_ker := 0;
            (* Pass 2: Stub functions that appear in the kernel and that are
               implemented in this file *)
            let obj_user : splitter_unified =
              if (String.compare !feature_mode "normal") = 0 then new splitter_normal
              else if (String.compare !feature_mode "sym") = 0 then new splitter_symbolic
              else fatal["Unknown feature mode specified"]
            in
            obj_user#top_level_user f annot_file entry_points_file obj_find_addr_taken obj_pass_modif_const;
            infomsg ["Produced user side of the split"];
        | _ -> fatal ["Unknown parameter"];
    end;
(* Commented by asim for performance reasons 
    let obj_instrument_funcs = new pass_instrument_funcs  in
    obj_instrument_funcs#top_level f !is_ker;
*)

  end
