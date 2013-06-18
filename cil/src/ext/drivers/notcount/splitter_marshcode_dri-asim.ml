(*===========================================================================*)
(* This file implements marshaling code generation.  The main
 * splitter_dri calls these functions as necessary.  Many of these
 * functions are standalone, in the sense that they do not rely on
 * global variables.
 * 
 * Vinod Ganapathy <vg@cs.wisc.edu>, September 28, 2006.
 * Matt Renzelmann <mjr@cs.wisc.edu> August, 2010
 *)
(*===========================================================================*)

open Cil
open Str
open Utils_dri
open Marshannot_dri
open Splitter_globals_dri
open Splitter_utils_dri

(**---------------------------------------------------------------------------*)
let initialize_nooks (gen_kern : bool) : unit =
  if gen_kern then
    begin
      nooks_ptrlookup_fn := "nooks_ot_lookup_kern_w";
      nooks_arrayalloc_fn := "nooks_ot_alloc_arraymem_kern_w";
      nooks_registerfn_fn := "nooks_ot_register_kernfn_w";
      nooks_storeoffset_fn := "nooks_ot_storeoffset_kern_w";
    end
  else
    begin
      nooks_ptrlookup_fn := "nooks_ot_lookup_user";
      nooks_arrayalloc_fn := "nooks_ot_alloc_arraymem_user";
      nooks_registerfn_fn := "nooks_ot_register_userfn";
      nooks_storeoffset_fn := "nooks_ot_storeoffset_user";
    end
(**---------------------------------------------------------------------------*)

(** Generates some function bodies for the kernel functions that have had
    their addresses taken in the user half of the driver, but are not interface
    functions (interface functions have been called and will get a body).
*)
let gen_dummy_bodies
    (interface_functions : (string, bool) Hashtbl.t)
    (funclist_defn: fundec list)
    (funclist_decl: varinfo list) :
    global list =
  begin
    let dummy_fn_list = ref [] in
    for i = 0 to (List.length funclist_decl) - 1 do
      let ith = (List.nth funclist_decl i) in
      if (kernfn_already_accounted_for funclist_defn ith) = false &&
        (is_interface_function interface_functions ith.vname) == false
      then
        begin
          Printf.fprintf stderr "dummy function %s\n" ith.vname;

          (* First define the function, parameters etc *)
          let dummy_fn_name = ith.vname in
          let dummy_fn = emptyFunction dummy_fn_name in
          setFunctionTypeMakeFormals dummy_fn ith.vtype;

          let (retType,_,_,_) = splitFunctionType dummy_fn.svar.vtype in
          let ret_stmttype =
            match retType with
              | TVoid (_) -> Return (None, locUnknown)
              | TInt (_, _) -> Return (Some (zero64Uexp), locUnknown)
              | TFloat (_, _) -> Return (Some (zero64Uexp), locUnknown)
              | TPtr (_, _) -> Return (Some (zero64Uexp), locUnknown)
              | TArray (_, _, _) -> Return (Some (zero64Uexp), locUnknown)
              | TFun(_, _, _, _) -> Return (Some (zero64Uexp), locUnknown)
              | TNamed(_, _) -> Return (Some (zero64Uexp), locUnknown)
              | TComp(_, _) -> Return(None, locUnknown) (* Definitely broken *)
              | TEnum(_, _) -> Return(None, locUnknown) (* Definitely broken *)
              | TBuiltin_va_list(_) -> Return(None, locUnknown) (* Definitely broken *)
          in
          let ret_stmt = mkStmt ret_stmttype in
          dummy_fn.sbody <- mkBlock [ret_stmt];

          ensure_formals_have_names dummy_fn;
          dummy_fn_list := List.append !dummy_fn_list [GFun (dummy_fn, locUnknown)];
        end
    done;
    !dummy_fn_list;
  end

(* Generates a call to the nooks_ot_register_xxxfn function *)
let gen_register_call (fdec: exp) (fn_name : string) : stmt =
  begin
    let fn_id = get_function_id fn_name in
    let nooks_registerfn_funcname = !nooks_registerfn_fn in
    let nooks_registerfn_func = emptyFunction nooks_registerfn_funcname in
    let fnarg = fdec in
    let fncodearg = integer fn_id in
    let argslist = [fnarg; fncodearg] in
    let callstmt =
      Call(None, (expify_fundec nooks_registerfn_func), argslist, locUnknown) in
    let stmt_callstmt = (mkStmt (Instr [callstmt])) in
    stmt_callstmt;
  end

(** gen_registerfn:
 * The function that registers functions whose addresses are taken with the
 * object tracker. We must manually insert a call to this function at an
 * appropriate place
 *)
let gen_registerfn
    (funclist_defn: fundec list)
    (funclist_decl: varinfo list)
    (interface_functions : (string, bool) Hashtbl.t)
    : fundec =
  begin
    let stmts = ref [] in
    let registerfn_funcname = "register_functions" in
    let registerfn_func = emptyFunction registerfn_funcname in
    (* Register list of functions which already have bodies *)
    for i = 0 to (List.length funclist_defn) - 1 do
      let ith = List.nth funclist_defn i in
      if is_interface_function interface_functions ith.svar.vname then
        begin
          let fnarg = expify_fundec ith in
          let stmt_callstmt = gen_register_call fnarg ith.svar.vname in
          Printf.fprintf stderr "gen_registerfn: %s\n" ith.svar.vname;
          stmts := List.append !stmts [stmt_callstmt];
        end
    done;

    (* Register the list of functions that we need to create bodies for.
       These are functions defined in the kernel whose addresses are taken
       on the user side *)
    for i = 0 to (List.length funclist_decl) - 1 do
      let ith = (List.nth funclist_decl i) in
      if (kernfn_already_accounted_for funclist_defn ith) = false then
        begin
          let fnarg = expify_lval (lvalify_varinfo ith) in
          let stmt_callstmt = gen_register_call fnarg ith.vname in
          stmts := List.append !stmts [stmt_callstmt];
        end
    done;

    registerfn_func.sbody.bstmts <- !stmts;
    registerfn_func;
  end

(** gen_registerglob:
 * The function that registers global variables with the object tracker.
 * object tracker. We must manually insert a call to this function at an
 * appropriate place
 *)
let gen_registerglob (lval_list: lval list) : fundec =
  begin
    let stmts = ref [] in
    let registerglob_funcname = "register_globals" in
    let registerglob_func = emptyFunction registerglob_funcname in
    (* Register list of global variables *)
    for j = 0 to (List.length lval_list) - 1 do
      let jth = List.nth lval_list j in
      let jth_id = get_lval_id jth in
      let nooks_registerfn_funcname = !nooks_registerfn_fn in
      let nooks_registerfn_func = emptyFunction nooks_registerfn_funcname in
      let lvalarg = ref (expify_lval jth) in
      if (isPointerType (typeOfLval jth)) = false &&
        (isArrayType (typeOfLval jth)) = false
      then lvalarg := (mkAddrOf jth);
      let lvalcodearg = (integer jth_id) in
      let argslist = [!lvalarg; lvalcodearg] in
      let callstmt =
        Call(None, (expify_fundec nooks_registerfn_func), argslist, locUnknown) in
      let stmt_callstmt = (mkStmt (Instr [callstmt])) in
      stmts := (List.append !stmts [stmt_callstmt]);
    done;
    registerglob_func.sbody.bstmts <- !stmts;
    registerglob_func;
  end

(*****************************************************************************)
(** Marshaling code generator implementation *)

(* funcname is one of:
 * ACQUIRE_REC_LOCK - Acquires the recursive lock to prevent multiple kernel
 *   threads from executing in the userdaemon.
 * RELEASE_REC_LOCK - Releases the recursive lock.
 * 
 * Assumes the funcname is called like so:
 * blah ();
 * with no args / return values
 *)
let gen_basic_call
    (funcname : string)
    : stmt =
  begin
    let syncfuncname = funcname in
    let syncfundec = (emptyFunction syncfuncname) in
    let syncfunc = (expify_fundec syncfundec) in
    let call_syncfunc = Call(None, syncfunc, [], locUnknown) in
    let stmt_call_syncfunc = (mkStmt (Instr [call_syncfunc])) in
    stmt_call_syncfunc;
  end

let gen_extra_code
    (fdec : fundec) (* Function containing the resulting stmts *)
    (extra_retval : varinfo) (* varinfo in which we store the return value *)
    (extra_desttype : typ) (* Type of return value *)
    (extra_srclval : lval) (* varinfo we're passing to the function as a parameter*)
    (extra_srctype : typ) (* type of parameter to the function we're calling *)
    (extra_funcname : string) (* Name of function we're calling *)
    (marshaling : bool) (* Are we marshaling or demarshaling *)
    (gen_kern : bool) (* Kernel or user mode? *)
    (after : bool) (* Is this call going after or before we marshaled the extra stuff? *)
    : stmt list = 
  begin
    let extra_func_fundec = emptyFunction extra_funcname in
    let extra_func_exp = expify_fundec extra_func_fundec in
    let extra_param1 = expify_lval extra_srclval in
    let extra_param2 = if marshaling then integer 1 else integer 0 in
    let extra_param3 = if gen_kern then integer 1 else integer 0 in
    let extra_param4_exp = expify_lval (lvalify_varinfo extra_retval) in
    let extra_param4 = if after then extra_param4_exp else integer 0 in
    let extra_params = [extra_param1; extra_param2; extra_param3; extra_param4] in
    let extra_retval_lval = lvalify_varinfo extra_retval in
    let call_extrafunc = Call(Some (extra_retval_lval), extra_func_exp, extra_params, locUnknown) in
    [mkStmt (Instr [call_extrafunc])];
  end
    
(** gen_get_container: This is borrowed from the code in the LInux kernel.
 * Inputs are: (1) an LVAL denoting a pointer to the member, (2) type of the
 * container struct, and (3) name of the member within the struct. Produce the
 * code produced by container_of. The fundec is the function in which we must
 * place the code. Conlv is the variable that is the container.*)
let gen_container_code
    (fdec: fundec)
    (ptr: lval)
    (contype: typ)
    (conlv: lval)
    (memopt: fieldinfo option)
    : stmt list =
  begin
    let retstmts = ref [] in
    (match memopt with
       | None -> retstmts := [];
       | Some(mem) ->
           let mptr_typ = (TPtr(mem.ftype, [])) in
           let mptr_var = (get_temporary_variable fdec None mptr_typ "__mptr") in
           let mptr_lv = (lvalify_varinfo mptr_var) in
           let mptr_exp = (expify_lval mptr_lv) in
           let set_mptr = (Set(mptr_lv, (expify_lval ptr), locUnknown)) in
           let stmt_set_mptr = (mkStmt (Instr [set_mptr])) in
           let nullexp_contype = (mkCast zero64Uexp contype) in
           let nullexp_mem = (mkMem nullexp_contype (Field(mem, NoOffset))) in
           let addrof_nullexp_mem = (mkAddrOf nullexp_mem) in
           let addrof_nullexp_mem_cast = (mkCast addrof_nullexp_mem uintType) in
           let mptr_cast = (mkCast mptr_exp charPtrType) in
           let calc_cont = (BinOp(MinusPP, mptr_cast, addrof_nullexp_mem_cast, voidPtrType)) in
           let cast_calc_cont = (mkCast calc_cont contype) in
           let set_contlv = (Set(conlv, cast_calc_cont, locUnknown)) in
           let stmt_set_contlv = (mkStmt (Instr [set_contlv])) in
           retstmts := [stmt_set_mptr; stmt_set_contlv];
    );
    !retstmts;
  end

(** gen_fillmarshbuf_ptr: Generate marshaling code filling up the
 * marshaling buffer with the value of a pointer. If we have a
 * OFFSETPTR pointer, then we generate marshaling code filling the
 * marshaling buffer with the integer value of the offset instead.  *)
let gen_fillmarshbuf_ptr (fdec: fundec)
    (tomarshal: lval)
    (tomarshal_typ: typ)
    (bufvar : varinfo)
    (currsize: varinfo) : stmt list =
  begin
    let marshptr_func_name = "fill_marshbuf_ptr" in
    let marshptr_fundec = emptyFunction marshptr_func_name in
    let marshptr_func = expify_fundec marshptr_fundec in

    let stripped_name = strip_stub_prefix fdec.svar.vname in
    (*let arg_current_id = integer (get_function_id stripped_name) in*)
    let arg_current_function = Const(CStr(stripped_name)) in

    let arg_tgt_buf = mkAddrOf (lvalify_varinfo bufvar) in
    let arg_currsize = mkAddrOf (lvalify_varinfo currsize) in
    let arg_src_buf = mkAddrOf tomarshal in
    let argslist = [arg_current_function; arg_tgt_buf; arg_currsize; arg_src_buf] in
    let call_marshptr_func = Call(None, marshptr_func, argslist, locUnknown) in
    let stmt_marshptr_call = mkStmt (Instr [call_marshptr_func]) in
    [stmt_marshptr_call];
  end

(** gen_nooks_arrayalloc_call:
 * Generate the call to allocate space for an array. The inputs to this
 * function are (i) lv: an Lval representing the pointer to the head of
 * of the array, which may be changed by the alloc call; (ii) size: a
 * varinfo, that stores the number of elements in the array; and (iii)
 * a type, representing the type of each element in the array.
 *)
let gen_nooks_arrayalloc_call
    (arrhd: lval)
    (size: exp)
    (elemtyp: typ)
    (storerange: bool) : stmt =
  begin
    let nooks_arrayalloc_fundec = (emptyFunction !nooks_arrayalloc_fn) in
    let nooks_arrayalloc_func = (expify_fundec nooks_arrayalloc_fundec) in
    let addrof_arrhd_exp = (mkCast (mkAddrOf arrhd) voidPtrType) in
    let length_arr_exp = size in
    let sizeof_typ = (SizeOf(elemtyp)) in
    let rangestore = if storerange then integer 1 else integer 0 in
    let argslist = [addrof_arrhd_exp; length_arr_exp; sizeof_typ; rangestore] in
    let call_nooks_arrayalloc_func =
      Call(None, nooks_arrayalloc_func, argslist, locUnknown) in
    let stmt_call_nooks_arrayalloc_func =
      (mkStmt (Instr [call_nooks_arrayalloc_func])) in
    stmt_call_nooks_arrayalloc_func;
  end
    
let gen_storeoffset_range_call
    (argslist : exp list)
    : stmt =
  begin
    let storeoff_func_name = nooks_storeoffset_fn in
    let storeoff_fundec = (emptyFunction !storeoff_func_name) in
    let storeoff_func = (expify_fundec storeoff_fundec) in
    let call_storeoff_func = Call(None, storeoff_func, argslist, locUnknown) in
    let stmt_call_storeoff_func = (mkStmt (Instr [call_storeoff_func])) in
    stmt_call_storeoff_func
  end

(** Generate storeoffset call:
 * Generate the call to register an offset into the object tracker. The
 * inputs to the function are (i) lv: representing the head of the structure.
 * (ii) fieldinfo: the fieldinfo of the structure whose offset we want to
 * register, and (iii) the function inside which we want to place this code
 *)
let gen_storeoffset_call
    (lv: lval)
    (finfo: fieldinfo)
    (fdec: fundec) : stmt list =
  begin
    let retval = ref [] in
    let addrof_lv = (mkAddrOf lv) in
    let lvfinfo = (add_field_to_lval lv finfo) in
    let addrof_lvfinfo = (mkAddrOf lvfinfo) in
    let tmp_lv = (get_temporary_variable fdec None voidPtrType "tmp_lv") in
    let tmp_lvfinfo = (get_temporary_variable fdec None voidPtrType "tmp_fld") in
    let set_tmp_lv = Set((lvalify_varinfo tmp_lv), addrof_lv, locUnknown) in
    let stmt_set_tmp_lv = (mkStmt (Instr [set_tmp_lv])) in
    let set_tmp_lvf = Set((lvalify_varinfo tmp_lvfinfo), addrof_lvfinfo, locUnknown) in
    let stmt_set_tmp_lvf = (mkStmt (Instr [set_tmp_lvf])) in
    let fldoff = BinOp(MinusPP, (expify_lval (lvalify_varinfo tmp_lvfinfo)),
                       (expify_lval (lvalify_varinfo tmp_lv)) , TInt(IUInt,[])) in
    let sizeoflv = SizeOf(finfo.ftype) in
    let argslist = [addrof_lv; fldoff; sizeoflv] in
    let storeoff_func_name = !nooks_storeoffset_fn in
    let storeoff_fundec = (emptyFunction storeoff_func_name) in
    let storeoff_func = (expify_fundec storeoff_fundec) in
    let call_storeoff_func = Call(None, storeoff_func, argslist, locUnknown) in
    let stmt_call_storeoff_func = (mkStmt (Instr [call_storeoff_func])) in
    retval := (List.append !retval [stmt_set_tmp_lvf]);
    retval := (List.append !retval [stmt_set_tmp_lv]);
    retval := (List.append !retval [stmt_call_storeoff_func]);
    !retval;
  end

(** gen_fetchmarshbuf_ptr:
 * Generate demarshaling code fetching the value of a pointer from the
 * marshaling buffer. If the todemarshal variable has a const annotation,
 * we skip over the statement that assigns the demarshaled value to the
 * todemarshal variable.
 *)
let gen_fetchmarshbuf_ptr (fdec: fundec)
    (todemarshal: lval)
    (todemarshal_typ: typ)
    (bufvar: varinfo)
    (currsize: varinfo) : stmt list =
  begin
    let demarshptr_func_name = "fetch_marshbuf_ptr" in
    let demarshptr_fundec = emptyFunction demarshptr_func_name in
    let demarshptr_func = expify_fundec demarshptr_fundec in

    let stripped_name = strip_stub_prefix fdec.svar.vname in
    (*let arg_current_id = integer (get_function_id stripped_name) in*)
    let arg_current_function = Const(CStr(stripped_name)) in

    let arg_src_buf = expify_lval (lvalify_varinfo bufvar) in
    let arg_currsize = mkAddrOf (lvalify_varinfo currsize) in
    let retval = mkAddrOf todemarshal in
    let cast_pointerpointer = TPtr (TPtr (TVoid ([]), []), []) in
    let arg_retval_cast = mkCast retval cast_pointerpointer in
    let arg_sizeof_targetobj =
      if is_iomem todemarshal_typ = false && is_usermem2 todemarshal_typ = false
      && is_untouched todemarshal_typ = false then
        let arg_sizeof_typ = deref_ptr_typ todemarshal_typ in
        SizeOf(arg_sizeof_typ)
      else (
          if is_usermem2 todemarshal_typ = true then
              integer (-2)
      else 
          if is_untouched todemarshal_typ = true then
              integer(-3)
          else
              integer (-1)
              );
        (* integer (-2) -- Changed by asim, we skip wrappers and let iomem call
         * readl/writel directly. *)
    in
    let argslist = [arg_current_function;
                    arg_src_buf;
                    arg_currsize;
                    arg_retval_cast;
                    arg_sizeof_targetobj] in
    let call_demarshptr_func = Call(None, demarshptr_func, argslist, locUnknown) in
    let stmt_demarshptr_call = mkStmt (Instr [call_demarshptr_func]) in

    (* What should we do if we have a const pointer? *)
    (*
      if (Marshannot_dri.is_const todemarshal_typ) = true
      then [stmt_demarshptr_call; stmt_incr_currsize]
      else [stmt_incr_currsize];
    *)

    (*
      Previously, we used two separate steps: fetch the pointer
      into the final memory location, and then translate it in place.
      This leads to a problem in the case of repeated marshaling and
      demarshaling during other kernel activity: we may fetch a user
      mode pointer into some kernel structure, and then, before translation
      takes place, the kernel tries to dereference that pointer, potentially
      in the context of another process.  Obviously this is bad.

      Therefore, we replace the previous two step process with a new
      single step one, in which the fetch function is expanded to
      accomodate the information necessary for the nooks_ot_lookup
      function to execute.

      Another option would to be to generate more temporary variables
      and perform the fetch into the temporary, and then assign
      the temporary to the final location after the translation.  This
      would work too, but would require even more marshaling code.
    *)

    (*
    (* The statements below initiate a pointer translation. If pointer
       translation fails, memory of size = sizeof_targetobj is allocated *)
    let xlateptr_func_name = !nooks_ptrlookup_fn in
    let xlateptr_fundec = (emptyFunction xlateptr_func_name) in
    let xlateptr_func = (expify_fundec xlateptr_fundec) in
    let ptr2xlate = (expify_lval todemarshal) in
    let xlate_retval = (mkCast (mkAddrOf todemarshal) voidPtrType) in
    let sizeof_targetobj = (SizeOf(deref_ptr_typ todemarshal_typ)) in
    let argslist = [ptr2xlate; xlate_retval; sizeof_targetobj] in
    let call_xlateptr_func = Call(None, xlateptr_func, argslist, locUnknown) in
    let stmt_xlateptr_call = (mkStmt (Instr [call_xlateptr_func])) in
    *)

    if isVoidType (deref_ptr_typ todemarshal_typ) = true
      && is_iomem todemarshal_typ = false
    then
      begin
        (addwarn ["Void type passed to nooks_ot_lookup: ";
                  fdec.svar.vname; (lval_tostring todemarshal);
                  "Fix this, or you'll have memory corruption at runtime"]);
      end;
    (*[stmt_demarshptr_call; stmt_xlateptr_call]; *)
    [stmt_demarshptr_call];
  end

(** gen_marshbuf_free:
 * Generate code to free the marshaling buffer. Inputs are the name of the
 * FREE function, and the varinfo of the buffer to be freed.
 *)
let gen_marshbuf_free (free_func_name: string)
    (marshbuf_lval: lval) : stmt =
  begin
    let free_fundec = emptyFunction free_func_name in
    let free_func = expify_fundec free_fundec in
    let free_argslist = [expify_lval marshbuf_lval] in
    let call_free_func = Call(None,free_func, free_argslist, locUnknown) in
    let stmt_call_free = mkStmt (Instr [call_free_func]) in
    let free_check =
      BinOp(Ne, (expify_lval marshbuf_lval), zero64Uexp, intType) in
    let free_trueblock = mkBlock [stmt_call_free] in
    let free_falseblock = mkBlock [] in
    let free_ifstmt = If(free_check, free_trueblock, free_falseblock, locUnknown) in
    mkStmt free_ifstmt;
  end

(** Generate an if-stmt that tests whether we're allowed to call into the kernel.
    Supply the true and false block.
*)
let gen_disp_kern_allowed
    (stub_func: fundec)
    (true_block : block)
    (false_block : block)
    : stmt list =
  begin
    let disp_kern_allowed_retval = get_local_variable stub_func (TInt (IInt, [])) "disp_kern_allowed_retval" in
    let disp_kern_allowed_retval_lval = lvalify_varinfo disp_kern_allowed_retval in
    let disp_kern_allowed_fundec = emptyFunction "should_disp_kern" in
    let disp_kern_allowed_func = expify_fundec disp_kern_allowed_fundec in
    let disp_kern_allowed_argslist = [] in
    let disp_kern_allowed_call = Call(Some (disp_kern_allowed_retval_lval),
                                      disp_kern_allowed_func,
                                      disp_kern_allowed_argslist,
                                      locUnknown) in
    let disp_kern_allowed_call_stmt = (mkStmt (Instr [disp_kern_allowed_call])) in
    let int_one = Const (CInt64(Int64.of_int 1, IUInt, None)) in
    let disp_kern_allowed_check =
      BinOp(Eq, (expify_lval (lvalify_varinfo disp_kern_allowed_retval)), int_one, intType) in
    let ifstmt = If(disp_kern_allowed_check, true_block, false_block, locUnknown) in
    [disp_kern_allowed_call_stmt; (mkStmt ifstmt)];
  end

let gen_buf_not_null
    (stub_func: fundec)
    (true_block : block)
    (false_block : block)
    : stmt list =
  begin
    let rqarg_varinfo = get_local_variable stub_func struct_reqargs_typ "rqarg" in
    let rqarg_lval = lvalify_varinfo rqarg_varinfo in
    let rqarg_lval_data = add_field_to_lval rqarg_lval reqargs_data in
    let rqarg_check = BinOp(Eq, (expify_lval rqarg_lval_data), zero64Uexp, intType) in
    let rqarg_ifstmt = If(rqarg_check, true_block, false_block, locUnknown) in
    [mkStmt rqarg_ifstmt];
  end
    
(** Figure out if we're allowed to invoke the kernel or not.
 * The issue is that with symoblic execution, we don't always want to
 * invoke the kernel.  This code will generate a return statement to
 * avoid invoking the kernel if necessary.
 *
 * It also generates the code necessary to make the return value
 * symbolic.
 *)
let gen_disp_kern_blocker
    (stub_func: fundec)
    (retvar_typ: typ)
    (retvar_lval: lval option)
    (stmts_buf_ok : stmt list)
    (stmts_buf_null : stmt list)
    : stmt list =
  begin
    let true_block =
      (match retvar_lval with
         | Some(ret_lval) ->
             let klee_fundec = emptyFunction "klee_make_symbolic" in
             let klee_sizeof = SizeOf (retvar_typ) in
             let klee_name = Const(CStr(stub_func.svar.vname)) in
             let klee_argslist = [mkAddrOf ret_lval; klee_sizeof; klee_name] in
             let klee_func = expify_fundec klee_fundec in
             let klee_call = Call(None,
                                  klee_func,
                                  klee_argslist,
                                  locUnknown) in
             let klee_stmt = mkStmt (Instr [klee_call]) in
             mkBlock ([klee_stmt] @ stmts_buf_null);
         | None ->
             mkBlock [];
      ) in

    let false_block = mkBlock stmts_buf_ok in
    (*gen_disp_kern_allowed stub_func true_block false_block;*)
    gen_buf_not_null stub_func true_block false_block;
  end

(* Example names include "call_interrupt_handlers"
   and "execute_completions" *)
let gen_call_empty
    (name: string)
    : stmt list =
  begin
    let call_emptyfundec = emptyFunction name in
    let call_emptyfunc = expify_fundec call_emptyfundec in
    let call_emptyargslist = [] in
    let call_emptycall = Call(None,
                              call_emptyfunc,
                              call_emptyargslist,
                              locUnknown) in
    [mkStmt (Instr [call_emptycall])];
  end

(** gen_disploop:
 * Code that generates the body of the dispatch loop. Takes as input
 * a pointer to struct req_args.
 * Output: Generate the dispatch loop.
 *)
let gen_disploop (gen_kern: bool)
    (marshwrap_funcs: fundec list) : fundec =
  begin
    let stmts = ref [] in
    let disp_funcname = (if gen_kern then "disp_kern" else "disp_user") in

    (* Create the disp_user/disp_kern function and set its return type *)
    let (disp_func, _, disp_arg2) = get_disp_userkern_fundec disp_funcname in

    (* Set up some other variables that we'll need for each of the conditionals *)
    let star_disp_arg2_lh = Mem((expify_lval (lvalify_varinfo disp_arg2))) in

    (* Initialize retval.buf = 0 *)
    let marshret_typ = TComp(struct_marshret_compinfo, []) in
    let locvar = makeLocalVar disp_func "retval" marshret_typ in
    let locvar_buf = lvalify_varinfo_field locvar marshret_buf in
    let set_locvar = Set(locvar_buf, zero64Uexp, locUnknown) in
    let stmt_set_locvar = mkStmt (Instr [set_locvar]) in
    stmts := List.append !stmts [stmt_set_locvar];

    (* Produce all the conditionals *)
    for i = 0 to (List.length marshwrap_funcs) - 1 do
      let ith = List.nth marshwrap_funcs i in
      let ith_stripped_name = strip_marshwrap_prefix ith.svar.vname in
      let funcid_lval = (star_disp_arg2_lh, Field(reqargs_funcid, NoOffset)) in
      let funcid_exp = expify_lval funcid_lval in
      let ith_id = get_function_id ith_stripped_name in
      let ith_idexp = integer ith_id in
      let disp_check = BinOp(Eq, funcid_exp, ith_idexp, intType) in
      let callstmt_fundec = emptyFunction ith.svar.vname in
      let callstmt_func = expify_fundec callstmt_fundec in

      (* Create first parameter:  arg->data *)
      let buf_lval = (star_disp_arg2_lh, Field(reqargs_data, NoOffset)) in
      let buf_exp = expify_lval buf_lval in

      (* Create second parameter: &retval *)
      let retval_exp = mkAddrOf (lvalify_varinfo locvar) in

      (* Make the call: __MARSH_WRAP__blah (arg->data, &retval); *)
      let argslist = [buf_exp; retval_exp] in
      let callstmt = Call(None, callstmt_func, argslist, locUnknown) in
      let stmt_callstmt = (mkStmt (Instr [callstmt])) in

      let set_arg_buf_rhs = (expify_lval (lvalify_varinfo_field locvar marshret_buf)) in
      let set_arg_buf_lhs = buf_lval in
      let set_arg_buf = Set(set_arg_buf_lhs, set_arg_buf_rhs, locUnknown) in
      let stmt_set_arg_buf = (mkStmt (Instr [set_arg_buf])) in
      let set_arg_len_rhs = (expify_lval (lvalify_varinfo_field locvar marshret_len)) in
      let len_lval = (star_disp_arg2_lh, Field(reqargs_length, NoOffset)) in
      let set_arg_len_lhs = len_lval in
      let set_arg_len = Set(set_arg_len_lhs, set_arg_len_rhs, locUnknown) in
      let stmt_set_arg_len = (mkStmt (Instr [set_arg_len])) in
      let stmt_retstmt = mkStmt (Return(Some (integer 0), locUnknown)) in
      let check_trueblock =
        (mkBlock [stmt_callstmt; stmt_set_arg_buf; stmt_set_arg_len; stmt_retstmt]) in
      let check_falseblock = (mkBlock []) in
      let check_ifstmt = If(disp_check,check_trueblock,check_falseblock,locUnknown) in
      let stmt_check = (mkStmt check_ifstmt) in
      stmts := (List.append !stmts [stmt_check]);
      (* TODO: This stmt on freeing the marshbuf has to be placed correctly *)
      (*
        let free_after_marsh = (gen_marshbuf_free "MARSHBUF_FREE" locvar) in
        stmts := (List.append !stmts [free_after_marsh]);
      *)
    done;
    (* If none of the above cases were taken, we have a bug. Might as well
     * cause a crash, making it easier to debug, rather than let the driver
     * fail silently *)
(*
    let bug_func_name = "BUG_DRFACT" in
    let bug_func = (emptyFunction bug_func_name) in
    let call_bug_func = Call(None, (expify_fundec bug_func), [], locUnknown) in
    let stmt_call_bug = (mkStmt (Instr [call_bug_func])) in
    let infloop = Loop(mkBlock([stmt_call_bug]), locUnknown, None, None) in
    let stmt_infloop = (mkStmt infloop) in
    stmts := (List.append !stmts [stmt_infloop]);
*)
    let return_stmt = mkStmt (Return(Some (integer 1), locUnknown)) in
    stmts := List.append !stmts [return_stmt];
    disp_func.sbody.bstmts <- !stmts;
    disp_func;
  end

let gen_empty_stub
    (stubname : string) (* Name of stub *)
    (stub_typ: typ)     (* Function signature *)
    (stub_category: int) : (* 1 if it's the main stub. e.g. "misc_device",
                              2 if it's the marsh stub, e.g. MARSHSTUB_miscdevice
                              3 if it's the demarsh stub
                           *)
    fundec =
  begin
    let new_fundec = emptyFunction stubname in
    new_fundec.svar.vglob <- true;
    new_fundec.svar.vdecl <- locUnknown;
    new_fundec.svar.vaddrof <- false;
    new_fundec.svar.vreferenced <- false;

    (* Make the new sub-functions static *)
    if (stub_category = 2) || (stub_category = 3) then (
       new_fundec.svar.vstorage <- Static; 
    );
    

    (* Create the formal parameters for this fundec *)
    (* First, create the buffer parameter *)
    if stub_category != 1 then
      ignore (makeFormalVar new_fundec "rqarg" (TPtr(struct_reqargs_typ, [])));

    (* Then copy all the parameters from the existing function *)
    let (rettyp,argsopt,is_vararg,_) = splitFunctionType stub_typ in
    match argsopt with
      | Some(al) ->
          (* Add the existing arguments *)
          for j = 0 to (List.length al) - 1 do
            let (jthstr,jthtyp,jthattr) = (List.nth al j) in
            let newformal = makeFormalVar new_fundec jthstr jthtyp in
            newformal.vattr <- jthattr;
          done;
      | None -> ();
    ;

    (* Finally set the return type *)
    let (new_rettyp, new_args, _, new_attrs) =
      splitFunctionType new_fundec.svar.vtype in
    let new_vararg = is_vararg in
    let new_typ =
      if stub_category = 2 then
        TFun (TVoid ([]), new_args, new_vararg, new_attrs)
      else
        TFun (rettyp, new_args, new_vararg, new_attrs)
    in
    setFunctionType new_fundec new_typ;
    new_fundec;
  end

let gen_copy_fromto_user
    (to_user: bool) (* True if to user, false otherwise *)
    (buf_to: exp)
    (off_to: exp)
    (buf_from: exp)
    (off_from: exp)
    (length: exp)
    : stmt list =
  let cftu_fn_name = if to_user then "copy_to_user" else "copy_from_user" in
  let cftu_fundec = emptyFunction cftu_fn_name in
  let cftu_exp = expify_fundec cftu_fundec in
  let cftu_char_star = TPtr (TInt (IChar, []), []) in

  (* Setup buf_to *)
  let cftu_cast_buf_to = mkCast buf_to cftu_char_star in
  let cftu_indexed_buf_to = BinOp (IndexPI,
                                  cftu_cast_buf_to,
                                  off_to,
                                  cftu_char_star) in
  let cftu_final_buf_to = mkCast cftu_indexed_buf_to (TPtr (TVoid ([]), [])) in

  (* Setup buf_from *)
  let cftu_cast_buf_from = mkCast buf_from cftu_char_star in
  let cftu_indexed_buf_from = BinOp (IndexPI,
                                    cftu_cast_buf_from,
                                    off_from,
                                    cftu_char_star) in
  let cftu_final_buf_from = mkCast cftu_indexed_buf_from (TPtr (TVoid ([]), [])) in
  
  (* Setup arguments to copy_to/from_user *)
  let cftu_arg1 = cftu_final_buf_to in
  let cftu_arg2 = cftu_final_buf_from in
  let cftu_arg3 = length in
  let cftu_instr = Call(None, cftu_exp, [cftu_arg1; cftu_arg2; cftu_arg3], locUnknown) in
  let cftu_stmt = mkStmt (Instr [cftu_instr]) in
  [cftu_stmt]

    























(*****************************************************************************)
(* Main marshaling functions                                                 *)
(*****************************************************************************)

(* Marshaling stack: keeps track of the types being marshaled *)
let marshstack = ref []

(* Are we currently in a recursive access? *)
let rec_access = ref false

let push_marshstack (s: string) =
  marshstack := List.append [s] !marshstack

let pop_marshstack () : unit =
  try
    marshstack := List.tl !marshstack;
  with Failure("tl") -> fatal ["Attempt to pop an empty stack"];;

let is_on_marshstack (s: string) = List.mem s !marshstack

let marshstack_len () : int = List.length !marshstack

let set_recaccess() = rec_access := true
let clear_recaccess() = rec_access := false
let is_recaccess() = !rec_access

(** gen_unified_m_dm: Generate marshaling or demarshaling code:
 *
 * Like the function gen_marshaling_code, this function generates code to
 * demarshal a value. The arguments passed to this function are:
 * (1) fdec: the fundec of the function that is currently being processed
 * (2) to_m_dm: the varinfo of the variable that will get its value after
 *     we demarshal / the varinfo that we're marshaling.
 * (3) field: this is an optional parameter that is passed, determining whether
 *     what we're demarshaling is a struct field.
 * (4) to_m_dm_typ: the type, possibly resolved of todemarshal,
 * (5) bufvar: the varinfo of the marshbuf, that contains the flattened DSes.
 * (6) currsize: the varinfo of the variable that stores the current offset
 *     into the buffer.
 * (7) fdec_m_dm: the fundec of the function for which we're generating
 *     demarshaling code. Equivalent to the fdec2 argument of unified_dfs.
 * (8) ptg: points-to-graph obtained after marshaling analysis.
 * (9) resfld: resolved fields information, for opaque pointers.
 * (10) resform: resolved formal parameter information, for opaque pointers.
 * (11) stkdpth: stack depth, used with recursive data structures
 * (12) marshaling:  true if we are generating marhshaling code, false
 *      otherwise
 * (13) gen_kern: true if generating code in the kernel half, false otherwise
 *      (user-half of the driver)
 *
 * As output, we generate code to marshal/demarshal. We use a
 * function fill_marshbuf/fetch_marshbuf, that takes 4 parameters,
 * one of which is a void * memory blob containing
 * the data that we asked for. The arguments are:
 * (1) a void * with the buffer containing the flattened data structure.
 * (2) the current offset into the buffer.
 * (3) the size of the data that we're requesting from the buffer.
 * (4) the return buffer.
 *)
let rec gen_unified_int
    (fdec: fundec)
    (to_m_dm: lval)
    (field: fieldinfo option)
    (to_m_dm_typ: typ)
    (bufvar: varinfo)
    (currsize: varinfo)
    (fdec_m_dm: fundec)
    (ptg: (string, (typ * string * string)) Hashtbl.t)
    (resfld: (string, (typ * fieldinfo * typ)) Hashtbl.t)
    (resform: (string, (int * varinfo * typ)) Hashtbl.t)
    (stkdpth: int)
    (marshaling: bool)
    (gen_kern: bool)

    (* See gen_unified_m_dm for remaining parameters *)
    (m_dm_func: exp)
    (the_buf: exp)
    (current_name: exp)
    (currsize_var: exp)
    : stmt list =
  begin
    let inserted_stmts = ref [] in
    (* other_buf is the opposite of the_buf:  if the_buf is the target, then
       other_buf is the source.  if the_buf is the source, then other_buf is
       the target *)
    if (is_iomem to_m_dm_typ) = false then
      begin
        let other_buf = ref
          (mkCast 
             (mkAddrOf (add_field_to_lval_wrapper to_m_dm field))
             (TPtr (TVoid ([]), []))
          )
        in
        (match field with
           | Some(f) ->
               (match f.fbitfield with
                  | Some(sz) -> (* Generate an assignment of the bitfield to an int *)
                      (* Just an integer temporary variable.  Will add just 4 bytes *)
                      let bitfldtmp = (get_temporary_variable fdec None intType "btfld") in
                      other_buf := mkCast
                        (mkAddrOf (lvalify_varinfo bitfldtmp))
                        (TPtr (TVoid ([]), []));

                      let (assign_bitfld_lhs, assign_bitfld_rhs) = 
                        if marshaling then
                          (lvalify_varinfo bitfldtmp,
                           expify_lval (add_field_to_lval_wrapper to_m_dm field))
                        else
                          (add_field_to_lval_wrapper to_m_dm field,
                           expify_lval (lvalify_varinfo bitfldtmp))
                      in
                      let assign_bitfld = Set(assign_bitfld_lhs, assign_bitfld_rhs, locUnknown) in
                      let stmt_assign_bitfld = mkStmt (Instr [assign_bitfld]) in
                      inserted_stmts := [stmt_assign_bitfld];
                  | None -> ();
               );
           | None -> ();
        );
        (* Get the value of the integer *)
        let sizeof_var = SizeOf(to_m_dm_typ) in
        let argslist =
          if marshaling then
            [current_name; mkAddrOf (lvalify_varinfo bufvar); currsize_var; !other_buf; sizeof_var]
          else
            [current_name; the_buf; currsize_var; sizeof_var; !other_buf]
        in
        let call_m_dm_func = Call(None, m_dm_func, argslist, locUnknown) in
        let stmt_m_dm_call = mkStmt (Instr [call_m_dm_func]) in
        (* We might have prepared code for demarshaling bit fields. These statements
         * must precede that statement *)
        inserted_stmts :=
          if marshaling then
            List.append !inserted_stmts [stmt_m_dm_call]
          else
            List.append [stmt_m_dm_call] !inserted_stmts;

        if is_usermem to_m_dm_typ then
          Printf.fprintf stderr "Found __user: %s %s\n" (typ_tostring to_m_dm_typ) fdec.svar.vname;
        (* Is this TInt ever a pointer? If so, translate and marshal/demarshal! *)
        if (Marshannot_dri.is_ispointer to_m_dm_typ) = true then
          begin
            let target_typ = Marshannot_dri.typeof_ptrtarget to_m_dm_typ in
            let pointer_type = TPtr(target_typ, []) in
            let ptrlv = add_field_to_lval_wrapper to_m_dm field in
            let m_dm_stmts = unified_dfs ptrlv pointer_type fdec
              fdec_m_dm ptg resfld resform bufvar currsize
              stkdpth marshaling gen_kern in
            inserted_stmts := List.append !inserted_stmts m_dm_stmts;
          end;
      end
    else
      begin
        (Printf.fprintf stderr "Not marshaling: %s in %s\n" (lval_tostring to_m_dm) fdec.svar.vname);
        inserted_stmts := [];
      end;
    !inserted_stmts;
  end
and gen_unified_array
    (fdec: fundec)
    (to_m_dm: lval)
    (field: fieldinfo option)
    (to_m_dm_typ: typ)
    (bufvar: varinfo)
    (currsize: varinfo)
    (fdec_m_dm: fundec)
    (ptg: (string, (typ * string * string)) Hashtbl.t)
    (resfld: (string, (typ * fieldinfo * typ)) Hashtbl.t)
    (resform: (string, (int * varinfo * typ)) Hashtbl.t)
    (stkdpth: int)
    (marshaling: bool)
    (gen_kern: bool)

    (* See gen_unified_m_dm for remaining parameters *)
    (m_dm_func: exp)
    (the_buf: exp)
    (current_name: exp)
    (currsize_var: exp)
    (basetype: typ)
    (lengthexp: exp option)
    : stmt list =
  (* Array type *)
  (* For an array, we first note the number of elements in the array. Here, the
     length of the array is available as an exp. Then we copy that many elements
     over from the array into the buffer. We DO NOT copy the base pointer of
     the array. That will be copied from the container struct. *)
  if (is_iomem to_m_dm_typ) = false then
    let array_length_exp = 
      match lengthexp with
        | Some(l) -> l;
        | None -> (* Read from annotations/provide manually. *)
            (* Initialize array length to "SUPPLYME" *)
            let unknown_prefix = if marshaling then
              "SUPPLYME"
            else
              "resolved_annot_"
            in
            let unknown_var = get_temporary_variable fdec None intType unknown_prefix in
            let unknown_lval = lvalify_varinfo unknown_var in
            let unknown_exp = expify_lval unknown_lval in
            unknown_exp;
    in
    (* Create an index variable to traverse the array *)
    let idx_var_name = "idx_arr" ^ (itoa stkdpth) in
    let idx_var = get_temporary_variable fdec None intType idx_var_name in
    let idx_lval = lvalify_varinfo idx_var in
    let idx_exp = expify_lval idx_lval in
    (* Create the offset into the array: i.e., (arr + idx), where arr is the
       name of the array declared as 'T arr[size]' *)
    let arrnm_lval = add_field_to_lval_wrapper to_m_dm field in
    let arrnm_exp = mkAddrOrStartOf arrnm_lval in
    let index = Index(idx_exp, NoOffset) in
    let ptrarith = addOffsetLval index arrnm_lval in
    if marshaling then
      begin
        let stmts_marshal = unified_dfs ptrarith basetype fdec fdec_m_dm ptg
          resfld resform bufvar currsize (stkdpth + 1)
          marshaling gen_kern
        in
        (* Create the loop that will traverse the array *)
        let stmts_loop =
          if List.length stmts_marshal > 0 then
            mkForIncr ~iter:(idx_var) ~first:(zero)
              ~stopat:(array_length_exp) ~incr:(one)
              ~body:(stmts_marshal)
          else
            []
        in
        stmts_loop;
      end
    else
      begin
        let stmts_rangestore = 
          (* If we have a RANGESTORE array, create a mapping for each element *)
          if (Marshannot_dri.is_rangestore to_m_dm_typ) = true then
            let argslist = [arrnm_exp ; idx_exp] in
            let new_stmt = gen_storeoffset_range_call argslist in
            [new_stmt]
          else
            []
        in
        (* Create the dereference lval and demarshal element*)
        let stmts_demarshal = unified_dfs ptrarith basetype fdec fdec_m_dm
                                 ptg resfld resform bufvar currsize (stkdpth + 1)
                                 marshaling gen_kern
        in
        let stmts_loopbody = stmts_rangestore @ stmts_demarshal in
        let stmts_loop =
          if List.length stmts_loopbody > 0 then
            (* Create the loop that will traverse the array *)
            mkForIncr ~iter:(idx_var) ~first:(zero)
              ~stopat:(array_length_exp) ~incr:(one)
              ~body:(stmts_loopbody)
          else
            []
        in
        stmts_loop;
      end
  else
    begin
      Printf.fprintf stderr "Not marshaling: %s in %s\n" (lval_tostring to_m_dm) fdec.svar.vname;
      [];
    end
and gen_unified_nullterm
    (fdec: fundec)
    (to_m_dm: lval)
    (field: fieldinfo option)
    (to_m_dm_typ: typ)
    (bufvar: varinfo)
    (currsize: varinfo)
    (fdec_m_dm: fundec)
    (ptg: (string, (typ * string * string)) Hashtbl.t)
    (resfld: (string, (typ * fieldinfo * typ)) Hashtbl.t)
    (resform: (string, (int * varinfo * typ)) Hashtbl.t)
    (stkdpth: int)
    (marshaling: bool)
    (gen_kern: bool)

    (* See gen_unified_m_dm for remaining parameters *)
    (m_dm_func: exp)
    (the_buf: exp)
    (current_name: exp)
    (currsize_var: exp)
    (dtyp: typ)
    : stmt list =
  begin
    (* If we are demarshaling, "other" means "target".  If we are marshaling,
       "other" means "src"
    *)
    let other_lval = (add_field_to_lval_wrapper to_m_dm field) in
    let other_buf = (mkCast
                       (expify_lval other_lval)
                       (TPtr (TVoid ([]), []))
                    ) in
    let m_dm_gen_fetch_fill =
      if marshaling then
        gen_fillmarshbuf_ptr
      else
        gen_fetchmarshbuf_ptr
    in
    (* 1. Marshal/demarshal the value of the pointer *)
    let ptr_m_dm = m_dm_gen_fetch_fill fdec
      (add_field_to_lval_wrapper to_m_dm field) to_m_dm_typ bufvar currsize in
    let deref_check = BinOp(Ne, (expify_lval other_lval), zero64Uexp, intType) in
    let true_block = ref [] in
    (* 2. Compute/Demarshal the string length *)
    let strlen_var = (get_temporary_variable fdec (Some(other_lval)) intType "strlen") in
    let strlen_lval = (lvalify_varinfo strlen_var) in
    let strlen_exp = (expify_lval strlen_lval) in

    if marshaling then
      begin
        let strlen_func_name = "strlen" in
        let strlen_fundec = (emptyFunction strlen_func_name) in
        let strlen_func = (expify_fundec strlen_fundec) in
        let retval = (Some(strlen_lval)) in
        let argslist = [other_buf] in
        let call_strlen = Call(retval, strlen_func, argslist, locUnknown) in
        let stmt_strlencall = (mkStmt (Instr [call_strlen])) in
        let incr_strlen_rhs = (BinOp(PlusA, strlen_exp, one, intType)) in
        let incr_strlen = Set(strlen_lval, incr_strlen_rhs, locUnknown) in
        let stmt_incr = (mkStmt (Instr [incr_strlen])) in
        true_block := (List.append !true_block [stmt_strlencall; stmt_incr]);
        (* Marshal the string length *)
        let argslist = [current_name;
                        mkAddrOf (lvalify_varinfo bufvar);
                        currsize_var;
                        (mkAddrOf strlen_lval); SizeOf(intType)
                       ]
        in
        let call_marshstrlen = Call(None, m_dm_func, argslist, locUnknown) in
        let stmt_call_marshstrlen = (mkStmt (Instr [call_marshstrlen])) in
        true_block := (List.append !true_block [stmt_call_marshstrlen]);
      end
    else
      begin
        let strlen_addrof = mkAddrOf strlen_lval in
        let argslist = [current_name; the_buf; currsize_var; SizeOf(intType); strlen_addrof] in
        let call_demarsh_strlen = Call(None, m_dm_func, argslist, locUnknown) in
        let stmt_demarsh_strlen = (mkStmt (Instr [call_demarsh_strlen])) in
        true_block := (List.append !true_block [stmt_demarsh_strlen]);
      end;

    if marshaling = false && (is_fixedarray to_m_dm_typ) = false then
      begin
        (* Realloc the target using nooks_ot_alloc_arraymem (this is a bug fix
         * Read that many bytes from the marshbuf
         * and store it in the target. Do so only if the target is a non-const.
         * If it is a const, then don't do the copy. *)
        let stmt_realloc_array =
          gen_nooks_arrayalloc_call
            other_lval
            strlen_exp
            dtyp
            false
        in
        true_block := (List.append !true_block [stmt_realloc_array]);
      end
    else
      ();

    (* 3. Read bytes into newly realloced target, or marshal the string itself *)
    let strlen_mult_size_exp = (BinOp(Mult,strlen_exp,SizeOf(dtyp),intType)) in
    let argslist =
      if marshaling then
        [current_name; mkAddrOf (lvalify_varinfo bufvar); currsize_var; other_buf; strlen_mult_size_exp]
      else
        [current_name; the_buf; currsize_var; strlen_mult_size_exp; other_buf]
    in
    let call_m_dm_func = Call(None, m_dm_func, argslist, locUnknown) in
    let stmt_m_dm_call = mkStmt (Instr [call_m_dm_func]) in
    true_block := List.append !true_block [stmt_m_dm_call];

    let check_trueblock = mkBlock !true_block in
    let check_falseblock = mkBlock [] in
    let check_ifstmt = If(deref_check, check_trueblock, check_falseblock ,locUnknown) in
    let stmt_check = mkStmt check_ifstmt in
    ptr_m_dm @ [stmt_check];
  end
and gen_unified_extraptr
    (fdec: fundec)
    (to_m_dm: lval)
    (field: fieldinfo option)
    (to_m_dm_typ: typ)
    (bufvar: varinfo)
    (currsize: varinfo)
    (fdec_m_dm: fundec)
    (ptg: (string, (typ * string * string)) Hashtbl.t)
    (resfld: (string, (typ * fieldinfo * typ)) Hashtbl.t)
    (resform: (string, (int * varinfo * typ)) Hashtbl.t)
    (stkdpth: int)
    (marshaling: bool)
    (gen_kern: bool)

    (* See gen_unified_m_dm for remaining parameters *)
    (m_dm_func: exp)
    (the_buf: exp)
    (current_name: exp)
    (currsize_var: exp)
    (dtyp: typ)
    : stmt list =
  (* If marshaling, extra means "src".  If demarshaling, "extra" means target *)
  let (extra_function_name, extra_desttype) = attr_extra_ptr to_m_dm_typ in
  let other_lval = add_field_to_lval_wrapper to_m_dm field in
  let extra_retval = get_temporary_variable fdec (Some (other_lval)) extra_desttype "extra" in
  let extra_retval_lval = lvalify_varinfo extra_retval in
  let extra_srctype = to_m_dm_typ in
  let extra_srclval = to_m_dm in
  let no_extra_annot = strip_typ_attribs to_m_dm_typ in
  let stmts_orig = unified_dfs
    to_m_dm no_extra_annot fdec fdec_m_dm
    ptg resfld resform bufvar currsize stkdpth
    marshaling gen_kern
  in
  let call_extra_stmts_pre = gen_extra_code
    fdec
    extra_retval
    extra_desttype
    extra_srclval
    extra_srctype
    extra_function_name
    marshaling
    gen_kern
    false
  in
  let stmts_extraptr = unified_dfs
    extra_retval_lval extra_desttype fdec fdec_m_dm
    ptg resfld resform bufvar currsize stkdpth
    marshaling gen_kern
  in
  let call_extra_stmts_post = gen_extra_code
    fdec
    extra_retval
    extra_desttype
    extra_srclval
    extra_srctype
    extra_function_name
    marshaling
    gen_kern
    true
  in
  stmts_orig @
    call_extra_stmts_pre @
    stmts_extraptr @
    call_extra_stmts_post;
and gen_unified_containerof
    (fdec: fundec)
    (to_m_dm: lval)
    (field: fieldinfo option)
    (to_m_dm_typ: typ)
    (bufvar: varinfo)
    (currsize: varinfo)
    (fdec_m_dm: fundec)
    (ptg: (string, (typ * string * string)) Hashtbl.t)
    (resfld: (string, (typ * fieldinfo * typ)) Hashtbl.t)
    (resform: (string, (int * varinfo * typ)) Hashtbl.t)
    (stkdpth: int)
    (marshaling: bool)
    (gen_kern: bool)

    (* See gen_unified_m_dm for remaining parameters *)
    (m_dm_func: exp)
    (the_buf: exp)
    (current_name: exp)
    (currsize_var: exp)
    (dtyp: typ)
    : stmt list =
  (* If marshaling, other means "src".  If demarshaling, "other" means target *)
  let other_lval = (add_field_to_lval_wrapper to_m_dm field) in
  let cont_typ = TPtr((typeof_container to_m_dm_typ), []) in
  let cont_var = (get_temporary_variable fdec (Some(other_lval)) cont_typ "cont") in
  let cont_fld = (fieldof_container to_m_dm_typ) in
  let cont_lv = (lvalify_varinfo cont_var) in
  if marshaling then
    let stmts_get_container =
      gen_container_code fdec other_lval cont_typ cont_lv cont_fld in
    let stmts_marshcont = unified_dfs cont_lv cont_typ fdec fdec_m_dm
      ptg resfld resform bufvar currsize stkdpth
      marshaling gen_kern in
    stmts_get_container @ stmts_marshcont;
  else
    begin
      let stmts_demarshcont = unified_dfs cont_lv cont_typ fdec fdec_m_dm
        ptg resfld resform bufvar currsize stkdpth
        marshaling gen_kern
      in
      (* Assign the demarshaled container's member field to the other_lval. Here,
       * check for compatibility of types *)
      if (isPointerType (typeOfLval cont_lv)) = false
      then
	fatal ["Non pointer type:"; (typ_tostring_noattr cont_typ);
	       (lval_tostring cont_lv); (fdec.svar.vname)];
      let cont_fld_lv = (add_field_to_lval_wrapper
                           (mkMem (expify_lval cont_lv) NoOffset) cont_fld) in
      let cont_fld_exp = ref (expify_lval cont_fld_lv) in
      (* If it's not a pointer, cast to a pointer *)
      if (isPointerType (typeOf !cont_fld_exp)) = false &&
        (isArrayType (typeOf !cont_fld_exp)) = false then
          begin
            (* We don't do a mkAddrOf in case of arrays: this is the last-member
             * is array of size 0 trick that is employed in the kernel *)
            cont_fld_exp := mkAddrOf cont_fld_lv;
          end;
      (* Check for compatibility of types *)
      let lhs_typ_str = (typ_tostring_noattr to_m_dm_typ) in
      let rhs_typ_str = (typ_tostring_noattr (typeOf !cont_fld_exp)) in
      if (String.compare lhs_typ_str rhs_typ_str) <> 0 then
        addwarn ["You're trying to generate an asg with incompatible types.";
                 "Here are the details:\n"; "\tLHS (var, type)";
                 (lval_tostring other_lval); lhs_typ_str;
                 "\n\t RHS (var, type)";
                 (exp_tostring !cont_fld_exp); rhs_typ_str];
      let set_other_lval = Set(other_lval, !cont_fld_exp, locUnknown) in
      let stmt_set_other_lval = (mkStmt (Instr [set_other_lval])) in
      stmts_demarshcont @ [stmt_set_other_lval];
    end

(* This function may be obsolete.

   We are currently doing all user-memory marshaling ONLY
   in copy_from_user/copy_to_user wrappers.
   We do NOT copy from/to user when transferring control
   between domains, so this extra marshaling code is
   never needed
*)
and gen_unified_usermem
    (fdec: fundec)
    (to_m_dm: lval)
    (field: fieldinfo option)
    (to_m_dm_typ: typ)
    (bufvar: varinfo)
    (currsize: varinfo)
    (fdec_m_dm: fundec)
    (ptg: (string, (typ * string * string)) Hashtbl.t)
    (resfld: (string, (typ * fieldinfo * typ)) Hashtbl.t)
    (resform: (string, (int * varinfo * typ)) Hashtbl.t)
    (stkdpth: int)
    (marshaling: bool)
    (gen_kern: bool)

    (* See gen_unified_m_dm for remaining parameters *)
    (m_dm_func: exp)
    (the_buf: exp)
    (current_name: exp)
    (currsize_var: exp)
    (dtyp: typ)
    : stmt list =
  if (gen_kern = false) then
    begin
      Printf.fprintf stderr "Not calling gen_unified_usermem on %s" fdec.svar.vname;
      []
    end
  else
    begin
      Printf.fprintf stderr "Calling gen_unified_usermem on %s" fdec.svar.vname;
      let var_suffix = fdec.svar.vname ^ (makevarname (lval_tostring to_m_dm)) in
      let bvname = "arraylen_" ^ var_suffix in
      let bdry_var = get_temporary_variable fdec (Some(to_m_dm)) intType bvname in
      let new_stmts =
        if marshaling then begin
          (* Marshaling - have to do copy_from_user *)
          (* Create the code to marshal the length of the array *)
          let (container_lv, offset) = removeOffsetLval to_m_dm in
          let option_container_lv =
            if offset = NoOffset then None
            else Some(container_lv)
          in
          let vars_to_search = fdec_m_dm.sformals @ fdec_m_dm.slocals @ fdec.sformals @ fdec.slocals in
          let stmt_len =
            if is_blob to_m_dm_typ then
              Marshannot_dri.get_array_length to_m_dm_typ option_container_lv
                vars_to_search bdry_var
            else
              begin
                addwarn ["usermem copy generated without blob annotation: "; typ_tostring to_m_dm_typ;
                         " in function: "; fdec.svar.vname; ". Default size = 1.  Add a blob annotation."];
                let init_bdry = Set(lvalify_varinfo bdry_var, integer 1, locUnknown) in
                let inst_len = Instr [init_bdry] in
                let stmt_len = mkStmt inst_len in
                stmt_len
              end
          in
          let stmts_marsh_bdry =
            gen_unified_m_dm fdec (lvalify_varinfo bdry_var) None intType
              bufvar currsize fdec_m_dm ptg resfld resform stkdpth
              marshaling gen_kern
          in
          (* Array size is calculated and marshaled.  Now do the copy_from_user *)
          let buf_to = the_buf in
          let off_to = expify_lval (lvalify_varinfo currsize) in
          let buf_from = expify_lval to_m_dm in
          let off_from = integer 0 in
          let length = expify_lval (lvalify_varinfo bdry_var) in
          let cfu_stmts = gen_copy_fromto_user false buf_to off_to buf_from off_from length in
          [stmt_len] @ stmts_marsh_bdry @ cfu_stmts;
        end
        else
          (* Demarshaling -- have to do copy_to_user *)
          (* First demarshal the length of the array *)
          let stmts_demarsh_bdry =
            gen_unified_m_dm fdec (lvalify_varinfo bdry_var)
              None intType bufvar currsize fdec_m_dm ptg resfld resform
              stkdpth marshaling gen_kern
          in
          (* Next do the copy_to_user *)
          let buf_to = expify_lval to_m_dm in
          let off_to = integer 0 in
          let buf_from = the_buf in
          let off_from = expify_lval (lvalify_varinfo currsize) in
          let length = expify_lval (lvalify_varinfo bdry_var) in
          let ctu_stmts = gen_copy_fromto_user true buf_to off_to buf_from off_from length in
          stmts_demarsh_bdry @ ctu_stmts
      in
      (* Increment buffer offset.  We normally do this with
         fetch/fill but we're not using those here.
      *)
      let add_off_lhs = lvalify_varinfo currsize in
      let add_off_rhs = BinOp (PlusA,
                               expify_lval add_off_lhs,
                               expify_lval (lvalify_varinfo bdry_var),
                               TInt (IInt, [])
                              )
      in
      let add_off_instr = Set (add_off_lhs, add_off_rhs, locUnknown) in
      let add_off_stmt = mkStmt (Instr [add_off_instr]) in
      new_stmts @ [add_off_stmt]
    end
and gen_unified_warning
    (fdec: fundec)
    (to_m_dm: lval)
    (field: fieldinfo option)
    (to_m_dm_typ: typ)
    (dtyp: typ) : stmt list =
  (* Print some helpful stuff: *)
  match dtyp with
    | TInt (kind, _) ->
        begin
          let function_info = Printf.sprintf "Function: %s" fdec.svar.vname in
          let structure_info = match field with
            | Some (fi) ->
                Printf.sprintf "Structure name: %s, Field name: %s"
                  fi.fcomp.cname fi.fname
            | None ->
                ""
          in
          let host_info = match to_m_dm with
            | (Var(vi), _) ->
                Printf.sprintf "Variable type: %s" (typ_tostring vi.vtype)
            | (Mem(ex), _) ->
                Printf.sprintf "Exp type: %s" (exp_tostring ex)
          in
          let offset_info = match to_m_dm with
            | (_, NoOffset) ->
                "No offset"
            | (_, Field(fi, offset)) ->
                Printf.sprintf "Structure name: %s, Field name: %s" fi.fcomp.cname fi.fname
            | (_, Index(ex, offset)) ->
                ""
          in
          let msg = Printf.sprintf "Assuming standard scalar pointer for %s\n"
            (lval_tostring to_m_dm) in
          let msg2 = Printf.sprintf "%s\n%s\n%s\n%s" function_info structure_info host_info offset_info in
          addwarn [msg; msg2];
          [];
        end
    | _ ->
        [];
          
(** Top-level function *)
and gen_unified_m_dm
    (fdec: fundec)
    (to_m_dm: lval)
    (field: fieldinfo option)
    (to_m_dm_typ: typ)
    (bufvar: varinfo)
    (currsize: varinfo)
    (fdec_m_dm: fundec)
    (ptg: (string, (typ * string * string)) Hashtbl.t)
    (resfld: (string, (typ * fieldinfo * typ)) Hashtbl.t)
    (resform: (string, (int * varinfo * typ)) Hashtbl.t)
    (stkdpth: int)
    (marshaling: bool)
    (gen_kern: bool)
    : stmt list =
  let m_dm_func_name = if marshaling then "fill_marshbuf" else "fetch_marshbuf" in
  let m_dm_fundec = emptyFunction m_dm_func_name in
  let m_dm_func = expify_fundec m_dm_fundec in
  let the_buf = expify_lval (lvalify_varinfo bufvar) in
  let stripped_name = strip_stub_prefix fdec.svar.vname in
  let current_name = Const(CStr(stripped_name)) in
  
  
    Printf.fprintf stderr "*****gen_unified_m_dm: Generating code %s %s typ:%s
    lval %s .\n" m_dm_func_name
    (exp_tostring current_name) (typ_tostring to_m_dm_typ) (lval_tostring
    to_m_dm);
  
  

  (* Buffer in/from which we marshal/demarsh data *)
  let currsize_var = mkAddrOf (lvalify_varinfo currsize) in
  match to_m_dm_typ with
    | TInt(_) ->
        gen_unified_int fdec to_m_dm field
          to_m_dm_typ bufvar currsize fdec_m_dm ptg
          resfld resform stkdpth marshaling gen_kern
          m_dm_func the_buf current_name currsize_var
    | TArray(basetype,lengthexp,_) ->
        gen_unified_array fdec to_m_dm field
          to_m_dm_typ bufvar currsize fdec_m_dm ptg
          resfld resform stkdpth marshaling gen_kern
          m_dm_func the_buf current_name currsize_var
          basetype lengthexp
    | TNamed(tinfo,_) ->
        (* A named type. We must marshal/demarshal it appropriately. Do not (de)marshal a
           compound type as that is handled by unified_dfs. *)
        if (isCompoundType to_m_dm_typ) = false then
          gen_unified_m_dm fdec to_m_dm field
            tinfo.ttype bufvar currsize fdec_m_dm ptg
            resfld resform stkdpth marshaling gen_kern
        else [];
    | TPtr(dtyp, al) ->
        (* Generic pointer type. We need to check for annotations here
         * Annotations handled here are: NULLTERM EXTRAPTR CONTAINER
         * ARITH/RECURSIVE annotations are handled in the unified_dfs algorithm. *)

        if (Marshannot_dri.is_nullterm to_m_dm_typ) = true then
          gen_unified_nullterm fdec to_m_dm field
            to_m_dm_typ bufvar currsize fdec_m_dm ptg
            resfld resform stkdpth marshaling gen_kern
            m_dm_func the_buf current_name currsize_var dtyp
        else if (Marshannot_dri.is_extraptr to_m_dm_typ) = true then
          gen_unified_extraptr fdec to_m_dm field
            to_m_dm_typ bufvar currsize fdec_m_dm ptg
            resfld resform stkdpth marshaling gen_kern
            m_dm_func the_buf current_name currsize_var dtyp
        else if (Marshannot_dri.is_container to_m_dm_typ) = true then
          gen_unified_containerof fdec to_m_dm field
            to_m_dm_typ bufvar currsize fdec_m_dm ptg
            resfld resform stkdpth marshaling gen_kern
            m_dm_func the_buf current_name currsize_var dtyp
            (*
        else if (Marshannot_dri.is_usermem to_m_dm_typ) = true then
          gen_unified_usermem fdec to_m_dm field
            to_m_dm_typ bufvar currsize fdec_m_dm ptg
            resfld resform stkdpth marshaling gen_kern
            m_dm_func the_buf current_name currsize_var dtyp
            *)
        else
          (* TODO: More cases to handle annotations to go here. Currently none. *)
          gen_unified_warning fdec to_m_dm field to_m_dm_typ dtyp;
    | TEnum(einfo,_) ->
        (* An enum type variable must have size of an integer *)
        gen_unified_m_dm fdec to_m_dm field
          (TInt(IInt,[])) bufvar currsize fdec_m_dm ptg
          resfld resform stkdpth marshaling gen_kern;
    | _ -> [];
and unified_nonvoid_ptr (lv: lval)
    (lvtyp: typ)
    (fdec: fundec)
    (fdec2: fundec)
    (ptg: (string, (typ * string * string)) Hashtbl.t)
    (resfld: (string, (typ * fieldinfo * typ)) Hashtbl.t)
    (resform: (string, (int * varinfo * typ)) Hashtbl.t)
    (m_dm_buf: varinfo)
    (m_dm_off: varinfo)
    (stkdpth: int)
    (marshaling: bool)
    (gen_kern: bool)
    : stmt list =
  begin
    (* Only call this function if we're marshaling a non-void pointer! *)

    (* List of statements for marshaling/demarshaling the value
       pointed to by the pointer. *)
    let stmts_unified_dfs = ref [] in

    (* v.vtype is a type that we do not support for marshaling/demarshaling,
     * and it is pointer type. We handle opaque pointers in a separate case. *)

    (* Are we marshaling or demarshaling? *)
    let m_dm_gen_fetch_fill =
      if marshaling then gen_fillmarshbuf_ptr else gen_fetchmarshbuf_ptr in
    (* Marshal/demarshal the value of the pointer *)
    let stmt_m_dm_ptr = m_dm_gen_fetch_fill fdec lv lvtyp m_dm_buf m_dm_off in

    if (is_iomem lvtyp) = false then
      begin
        (* Get the dereferenced type of the variable *)
        let dereftype = deref_ptr_typ lvtyp in
        (* Create a check to see that the pointer is not NULL *)
        let deref_check = BinOp(Ne, (expify_lval lv), zero64Uexp, intType) in

        (*At this point, we check to see if this is an arith or a recursive pointer *)
        if ((Marshannot_dri.is_blob lvtyp) = false) then
          begin
            (* Create the dereference lval *)
            let deref_lv = mkMem (expify_lval lv) NoOffset in
            (* Send the dereferenced pointer for marshaling/demarshaling *)
            stmts_unified_dfs := unified_dfs deref_lv dereftype fdec fdec2
              ptg resfld resform m_dm_buf m_dm_off (stkdpth + 1)
              marshaling gen_kern;
            (* If it is an arith pointer, we must copy until the end of the array *)
          end
        else if (Marshannot_dri.is_blob lvtyp) = true then
          begin
            let var_suffix = fdec.svar.vname ^ (makevarname (lval_tostring lv)) in
            let bvname = "arraylen_" ^ var_suffix in
            let bdry_var = get_temporary_variable fdec (Some(lv)) intType bvname in
            if marshaling then begin
              (* Marshaling *)
              (* 1. Create the code to marshal the length of the array *)
              let (container_lv, offset) = removeOffsetLval lv in
              let option_container_lv = (if offset = NoOffset then None
                                         else Some(container_lv)) in
              let vars_to_search = fdec2.sformals @ fdec2.slocals @ fdec.sformals @ fdec.slocals in
              let len_stmt = Marshannot_dri.get_array_length lvtyp option_container_lv
                vars_to_search bdry_var in
              stmts_unified_dfs := (List.append !stmts_unified_dfs [len_stmt]);
              let stmts_marsh_bdry =
                gen_unified_m_dm fdec (lvalify_varinfo bdry_var) None intType
                  m_dm_buf m_dm_off fdec2 ptg resfld resform stkdpth
                  marshaling gen_kern in
              stmts_unified_dfs := List.append !stmts_unified_dfs stmts_marsh_bdry;
            end
            else
              begin
                (* Demarshaling *)
                (* 1. Create the code to demarshal the length of the array *)
                let stmts_demarsh_bdry =
                  gen_unified_m_dm fdec (lvalify_varinfo bdry_var)
                    None intType m_dm_buf m_dm_off fdec2 ptg resfld resform
                    stkdpth marshaling gen_kern in
                stmts_unified_dfs := List.append !stmts_unified_dfs stmts_demarsh_bdry;

                (* 2. Once we've fetched the size of the array, we've to communicate its
                 * size to the nooks object tracker. *)

                (* TODO LOGIC IS COMPLEX *)
                (* This is the code that allocates memory for a variable-length
                   array
                *)
                (* Problems arise if we have not already allocated sufficient memory for it *)
                (* LOGIC COMPLEXITY TODO:
                   How do we marshal an sk_buff / skb when we are dealing with the interior of the
                   head/tail/end/data nonsense?  WE DO NOT want to re-allocate any of these pointers
                   under any circumstances.
                *)

                (*
                  Note on strings:  we're allocating all the fixed-length ones ourselves,
                  and adding them to the object tracker.  We still have to worry about
                  variable-length strings.  Search this file for gen_nooks_arrayalloc_call
                  for the location at which we allocate memory for variable-length strings
                *)

                let deref_type = deref_ptr_typ lvtyp in (* Type of individual elements *)
                (* TODO:  What if we are generating demarshaling code for an array
                   that conincidentally is located at the start of a structure which
                   is also in the object tracker?  In this case, the array and the structure
                   have the same address, but we don't want to allocate memory in this case,
                   since the array is already a part of the structure.  e.g. the name array
                   in the net_device structure.
                *)

                (*
                  TODO Note that leaving this code enabled CAUSES PROBLEMS
                *)
                if (Marshannot_dri.is_fixedarray lvtyp) = false then
                  begin
                    (* Only generate code for variable sized arrays.  We don't want
                       to allocate new memory if the array size is fixed *)
                    let stmt_nooks_arralloc =  gen_nooks_arrayalloc_call
                      lv
                      (expify_lval (lvalify_varinfo bdry_var))
                      deref_type
                      (Marshannot_dri.is_rangestore lvtyp)
                    in
                    stmts_unified_dfs := !stmts_unified_dfs @ [stmt_nooks_arralloc];
                  end;
              end;

            (* 3. Create temporary variable to perform offsets. Init it to 0 *)
            (* Each level of nesting means we must create a new variable name else
             * we'll be messing around with the values of index variables for outer
             * loops *)
            let idx_var_name = "idx" ^ (itoa stkdpth) in
            let idx_var = (get_temporary_variable fdec None intType idx_var_name) in
            (* 4. Create the dereference lval with an offset *)
            let idx_exp = (expify_lval (lvalify_varinfo idx_var)) in
            let ptrarith = BinOp(PlusPI, (expify_lval lv), idx_exp, lvtyp)  in
            let deref_lv = mkMem ptrarith NoOffset in
            let stmts_m_dm = (unified_dfs deref_lv dereftype fdec fdec2
                                ptg resfld resform m_dm_buf m_dm_off (stkdpth + 1)
                                marshaling gen_kern) in
            (* 5. Create the conditional/sentinel to break out of the loop/end
             * traveral of the array *)
            let bdry_exp = (expify_lval (lvalify_varinfo bdry_var)) in
            (* 6. Create the loop itself *)
            let stmts_loop = (mkForIncr ~iter:(idx_var) ~first:(zero)
                                ~stopat:(bdry_exp) ~incr:(one)
                                ~body:(stmts_m_dm)) in
            (* 7. Finally, append everything to the marshaled statements *)
            stmts_unified_dfs := (List.append !stmts_unified_dfs stmts_loop);
          end;
        
        (* Obtain the statements to be returned *)
        (* Put the whole thing within the IF check for non-NULL if we're not
         * dealing with a recursive pointer. *)
        let true_block = !stmts_unified_dfs in
        let check_trueblock = (mkBlock true_block) in
        let check_falseblock = (mkBlock []) in
        let check_ifstmt = If(deref_check,check_trueblock,check_falseblock,locUnknown) in
        let stmt_check = mkStmt check_ifstmt in
        (* Return value *)
        if (List.length true_block) > 0 then
          stmt_m_dm_ptr @ [stmt_check]
        else
          stmt_m_dm_ptr
      end
    else
      stmt_m_dm_ptr
  end
and unified_void_ptr (lv: lval)
    (lvtyp: typ)
    (fdec: fundec)
    (fdec2: fundec)
    (ptg: (string, (typ * string * string)) Hashtbl.t)
    (resfld: (string, (typ * fieldinfo * typ)) Hashtbl.t)
    (resform: (string, (int * varinfo * typ)) Hashtbl.t)
    (m_dm_buf: varinfo)
    (m_dm_off: varinfo)
    (stkdpth: int)
    (marshaling: bool)
    (gen_kern: bool)
    : stmt list =
  begin
    (* Only call this function if we're marshaling a void pointer! *)

    (* v.vtype is a type that we do not support for marshaling, and it is
     * an opaque pointer. In this case, we derefence the pointer, and marshal
     * the dereferenced structure. In case the opaque pointer resolved to more
     * than one type, we generate code for all resolutions, and throw a warning.
     * The user has to manually prune out the unwanted marshaling code. *)

    let ret_stmts = ref [] in
    (* Get the resolved type(s) of the variable *)
    let v = obtain_varinfo_from_lval lv in
    let restyp_list = resolve_opaque_formal v fdec2 resform in
    (* Are we marshaling or demarshaling? *)
    let m_dm_gen_fetch_fill =
      if marshaling then gen_fillmarshbuf_ptr else gen_fetchmarshbuf_ptr in
    (* If we don't resolve to anything, just (de)marsh the value of the pointer *)
    if (List.length restyp_list) = 0 then
      begin
        let stmt_m_dm_ptr = (m_dm_gen_fetch_fill fdec lv lvtyp m_dm_buf m_dm_off) in
        ret_stmts := (List.append !ret_stmts stmt_m_dm_ptr);
      end
    else
      begin
        if (List.length restyp_list) <> 1
        then fatal (["Resolved type list is size ";
                    Printf.sprintf "%d" (List.length restyp_list);
                    "Function ";
                    fdec.svar.vname;
                    " and ";
                    fdec2.svar.vname;
                    ".  Types include"]
                    @ (List.map (function param -> Printf.sprintf "\"%s\"" (typ_tostring param)) restyp_list)
                    @ [".  Affected variable is "; lval_tostring lv]
                   );
        
        for i = 0 to (List.length restyp_list) - 1
        do
          let ithres = (List.nth restyp_list i) in
          if (isPointerType ithres) = true then begin
            (* Fetch/Fill the value of the pointer *)
            let stmt_m_dm_ptr = (m_dm_gen_fetch_fill fdec lv ithres m_dm_buf m_dm_off) in

            if (is_iomem ithres) = false then
              begin
                (* Check that the pointer is not NULL, and
                   get the dereferenced type of the variable *)
                let dereftype = (deref_ptr_typ ithres) in
                (* TODO: Must add the code to check whether this is an arith pointer,
                 * and the appropriate marshaling code here. I'm not doing this right
                 * now because I've not encountered any opaque arith pointers *)
                (* Create a check to see that the pointer is not NULL *)
                let deref_check = BinOp(Ne, (expify_lval lv), zero64Uexp, intType) in
                (* Create the dereferenced lval, and
                 * send the pointer for marshaling/demarshaling *)
                let deref_lv = mkMem (mkCast (expify_lval lv) ithres) NoOffset in
                let stmts_unified_dfs =
                  (unified_dfs deref_lv dereftype fdec fdec2
                     ptg resfld resform m_dm_buf m_dm_off (stkdpth + 1)
                     marshaling gen_kern) in
                (* Put the whole thing within the IF check for non-NULL *)
                let true_block = stmts_unified_dfs in
                let check_trueblock = (mkBlock true_block) in
                let check_falseblock = (mkBlock []) in
                let check_ifstmt = If(deref_check,check_trueblock,check_falseblock,locUnknown) in
                let stmt_check = (mkStmt check_ifstmt) in
                (* Return value *)
                if (List.length true_block) > 0
                then ret_stmts := List.append !ret_stmts (List.append stmt_m_dm_ptr [stmt_check])
                else ret_stmts := List.append !ret_stmts stmt_m_dm_ptr;
              end
            else
              ret_stmts := List.append !ret_stmts stmt_m_dm_ptr;
          end
          else if (is_iomem ithres) = false then
            begin
              (* If the resolved type is not a pointer, try generating marshaling code
                 for it. If no code was generated, then raise a warning *)
              let m_dm_v_stmts =
                gen_unified_m_dm fdec lv None ithres m_dm_buf m_dm_off
                  fdec2 ptg resfld resform stkdpth marshaling gen_kern in
              if (List.length m_dm_v_stmts) <> 0
              then
                begin
                  ret_stmts := (List.append !ret_stmts m_dm_v_stmts);
                end
              else
                begin
                  let what_we_are_doing = (if marshaling then "marshaling" else "demarshaling") in
                  addwarn ["This case is currently not supported in"; what_we_are_doing; "/resolve:";
                           (lval_tostring lv); (typ_tostring_noattr ithres); fdec.svar.vname; fdec2.svar.vname];
                end;
            end
          else
            begin
              (* In this case, we have non-pointer annotated with noderef so
                 we don't marshal anything *)
            end
        done;
      end;
    !ret_stmts;
  end
and unified_compound (lv: lval)
    (lvtyp: typ)
    (fdec: fundec)
    (fdec2: fundec)
    (ptg: (string, (typ * string * string)) Hashtbl.t)
    (resfld: (string, (typ * fieldinfo * typ)) Hashtbl.t)
    (resform: (string, (int * varinfo * typ)) Hashtbl.t)
    (m_dm_buf: varinfo)
    (m_dm_off: varinfo)
    (stkdpth: int)
    (marshaling: bool)
    (gen_kern: bool)
    : stmt list =
  begin
    (* Only call this function for compound types! *)

    (* v.vtype is a type that we do not support for marshaling, and it is not
     * a pointer type. If it is a compound type, extract fields and marshal/demarshal
     * each of them *)
    let m_dm_stmts = ref [] in
    let ptg_recursive = ref false in
    (* Get the fields, and marshal/demarshal them iteratively. Ensure
       we don't have repeats in the field accesses. *)
    let ptg_query_result =
      get_fields_accessed (Hashtbl.find_all ptg (typ_tostring_noattr lvtyp)) in
    ptg_recursive := Marshannot_dri.is_recursive_access ptg_query_result;
    if (!ptg_recursive)
    then begin
      push_marshstack (typ_tostring_noattr lvtyp);
      (infomsg ["Pushing on stack:"; (lval_tostring lv);
        (typ_tostring_noattr lvtyp)]);
    end;

    for i = 0 to (List.length ptg_query_result) - 1 do
      let (ithtyp,ithfield,ithaccess) = (List.nth ptg_query_result i) in
      let ithcomp = (tcomp_compinfo ithtyp) in
        Printf.fprintf stderr "********Attempting to generate dm for %s %s
        %s.\n" (typ_tostring ithtyp) ithfield ithaccess;
      (* Whether we're generating code in the kernel or user is irrelevant. *)
      (* This conditional is very subtle, because there are 2^3 = 8 distinct
         places that marshaling/demarshaling code is generated, and we need to
         be sure they all match up.  That is, we need to be sure that whenever
         we marshal a value, we also demarshal it.  To reason through this,
         I used a short test program that contained all the possibilities.  *)
      (* Could be simplified if we removed the gen_kern term, but I think this
         probably makes it more clear *)
      if ((String.compare ithaccess "read ") <> 0) ||
        (*(    marshaling &&     gen_kern &&     (contains_marshwrap_prefix fdec.svar.vname)) ||*)
        (    marshaling &&     gen_kern && not (contains_marshwrap_prefix fdec.svar.vname)) ||
        (*(    marshaling && not gen_kern &&     (contains_marshwrap_prefix fdec.svar.vname)) ||*)
        (    marshaling && not gen_kern && not (contains_marshwrap_prefix fdec.svar.vname)) ||
        (not marshaling &&     gen_kern &&     (contains_marshwrap_prefix fdec.svar.vname)) ||

        (* Uncommented by asim 
        (not marshaling &&   gen_kern && not (contains_marshwrap_prefix fdec.svar.vname)) ||
        *)
        
        (not marshaling && not gen_kern &&     (contains_marshwrap_prefix fdec.svar.vname)) ||
        (*(not marshaling && not gen_kern && not (contains_marshwrap_prefix fdec.svar.vname)) ||*)
        false
      then
          Printf.fprintf stderr "######Crossed the ugly FOR";
        (match ithcomp with
           | Some(ithcompinfo) ->
               begin
                 (* This will not throw an exception because ithtyp is a TComp, and
                  * our marshaling analysis will only return fields that belong to the
                  * TComp. Check if the field is a void pointer, and if so, lookup its
                  * resolved type. *)
                 let ithfieldinfo = get_fieldinfo ithcompinfo ithfield in
                 let ithfieldtyp = ithfieldinfo.ftype in

                 if marshaling = false then begin
                   (* If the field is one whose address is taken, or it has a
                    * STOREOFFSET annotation, then generate code to store the offset
                    * of this field. We currently do so only if the lv is of the form
                    * *PTR. *)
                   if ((Marshal_dri.is_field_addr_taken ithfieldinfo) = true) ||
                     ((Marshannot_dri.is_storeoffset ithfieldtyp) = true)
                   then begin
                     let (lh, off) = lv in
                     (match lh with
                        | Mem(_) ->
                            let reg_offset_stmts = gen_storeoffset_call
                              lv ithfieldinfo fdec
                            in
                            m_dm_stmts := (List.append !m_dm_stmts reg_offset_stmts);
                        | Var(_) ->
                            (*
                              MJR: has been harmless in all observed cases.
                              However, don't rule out bug possibilities :)
                            *)
                              Printf.fprintf stderr "generating storeoffset in a
                              case I've never seen before %s %s %s
                              " (lval_tostring lv) fdec.svar.vname
                              fdec2.svar.vname; 

                              addwarn ["generating storeoffset in a case I've never seen before";
                                     "Check this case in the code that is generated. Info:";
                                     lval_tostring lv; fdec.svar.vname; fdec2.svar.vname];
                            
                            let reg_offset_stmts = gen_storeoffset_call
                              lv ithfieldinfo fdec
                            in
                            m_dm_stmts := (List.append !m_dm_stmts reg_offset_stmts);
                     );
                   end;
                 end;

                 if (isVoidPtrType ithfieldtyp) = true
                 then begin
                   let restyp_list = resolve_opaque_field fdec2 resfld ithtyp ithfieldinfo in
                   for j = 0 to (List.length restyp_list) - 1 do
                     (* Get the type we resolve the void * to *)
                     let jthres_noattrs = (List.nth restyp_list j) in

                     (* Add any attributes that were originally present.  May include
                        noderef for example
                     *)
                     let jthres = typeAddAttributes (typeAttrs ithfieldtyp) jthres_noattrs in
                     let m_dm_v_stmts =
                       gen_unified_m_dm fdec lv (Some(ithfieldinfo)) jthres
                         m_dm_buf m_dm_off fdec2 ptg resfld resform stkdpth
                         marshaling gen_kern
                     in
                     if (List.length m_dm_v_stmts) <> 0
                     then begin
                       m_dm_stmts := (List.append !m_dm_stmts m_dm_v_stmts);
                     end
                     else begin
                       (* Cannot marshal/demarshal this field type. *)
                       if marshaling then begin
                         (* Cannot marshal this field type. Simply generate an assignment, and
                          * call the marshaling function recursively. *)
                         (* In this case, we need to generate an assignment with a cast
                            statement resolving the field *)
                         let lhs_var = get_temporary_variable fdec None jthres ithfield in
                         let cast_lhs = lvalify_varinfo lhs_var in
                         let cast_rhs = expify_lval (add_field_to_lval lv ithfieldinfo) in
                         let cast_rhs' = mkCast cast_rhs jthres in
                         let cast_instr = Set(cast_lhs,cast_rhs',locUnknown) in
                         let stmt_cast = mkStmt (Instr [cast_instr]) in
                         let stmt_marsh_lhs =
                           unified_dfs cast_lhs jthres fdec fdec2
                             ptg resfld resform m_dm_buf m_dm_off (stkdpth + 1)
                             marshaling gen_kern
                         in
                         m_dm_stmts := List.append !m_dm_stmts ((*stmt_init::*) stmt_cast::stmt_marsh_lhs);
                       end
                       else begin
                         (* In this case, we generate
                          * a fetch of the pointer value. We then assign the fetched
                          * pointer value to a temp, and then recursively demarsh the
                          * temp *)
                         let temp_var = get_temporary_variable fdec None jthres ithfield in
                         let temp_lv = lvalify_varinfo temp_var in
                         let init_instr = Set(temp_lv, zero64Uexp, locUnknown) in
                         let stmt_init = mkStmt (Instr [init_instr]) in
                         let assgn_lhs = add_field_to_lval lv ithfieldinfo in
                         let assgn_rhs = expify_lval (lvalify_varinfo temp_var) in
                         let assgn_instr = Set(assgn_lhs, assgn_rhs, locUnknown) in
                         let stmt_assgn = mkStmt (Instr [assgn_instr]) in
                         let stmt_demarsh =
                           unified_dfs temp_lv jthres fdec fdec2
                             ptg resfld resform m_dm_buf m_dm_off (stkdpth + 1)
                             marshaling gen_kern
                         in
                         m_dm_stmts := List.append !m_dm_stmts
                           (List.append (stmt_init::stmt_demarsh) [stmt_assgn]);
                       end;
                     end;
                   done;
                 end
                 else
                   begin (* Not a void pointer type *)
                     let m_dm_v_stmts =
                       gen_unified_m_dm fdec lv (Some(ithfieldinfo))
                         ithfieldtyp m_dm_buf m_dm_off fdec2 ptg resfld resform
                         stkdpth marshaling gen_kern
                     in
                     if (List.length m_dm_v_stmts) <> 0
                     then
                       begin
                         m_dm_stmts := List.append !m_dm_stmts m_dm_v_stmts;
                       end
                     else
                       begin
                         (* Cannot marshal/demarshal this field type. Simply call marsh/demarsh
                          * function recursively, and generate an assignment. If the address
                          * of a field is taken, then we call it on another variable.*)
                         if ((Marshal_dri.is_field_addr_taken ithfieldinfo) = true) ||
                           ((Marshannot_dri.is_storeoffset ithfieldtyp) = true)
                         then
                           begin
                             let new_lval = add_field_to_lval lv ithfieldinfo in
                             let namehint = "STRUCTADDRX" ^ (makevarname (lval_tostring new_lval)) in
                             let var_type = TPtr (ithfieldtyp,[]) in
                             let addrof_lhs_var = get_local_variable fdec
                               var_type namehint in
                             let addrof_lhs = lvalify_varinfo addrof_lhs_var in
                             let addrof_rhs = mkAddrOf new_lval in
                             let addrof_set = Set(addrof_lhs, addrof_rhs, locUnknown) in
                             let stmt_setaddrof = mkStmt (Instr [addrof_set]) in
                             let star_addrof = mkMem (expify_lval addrof_lhs) NoOffset in
                             let stmt_m_dm =
                               unified_dfs star_addrof ithfieldtyp fdec fdec2
                                 ptg resfld resform m_dm_buf m_dm_off (stkdpth + 1)
                                 marshaling gen_kern in
                             if (List.length stmt_m_dm) <> 0 then
                               m_dm_stmts := !m_dm_stmts @ [stmt_setaddrof];
                             m_dm_stmts := !m_dm_stmts @ stmt_m_dm;
                           end
                         else
                           begin
                             let new_lval = add_field_to_lval lv ithfieldinfo in
                             let stmt_m_dm =
                               unified_dfs new_lval ithfieldtyp fdec fdec2
                                 ptg resfld resform m_dm_buf m_dm_off (stkdpth + 1)
                                 marshaling gen_kern
                             in
                             m_dm_stmts := List.append !m_dm_stmts stmt_m_dm;
                           end;
                       end
                   end                         
               end
           | None -> ()
        );
    done;
    
    if (!ptg_recursive = true) then begin
      pop_marshstack();
      (*(infomsg ["Popping from stack:"; (lval_tostring lv);
        (typ_tostring_noattr lvtyp)]);*)
    end;

    (*
    (* If we had a recursive field access, then we must embed the statements
     * that we just generated within a SENTINEL-guarded while loop *)
    if (!ptg_recursive = true) && (is_recaccess() = true)
    then begin
      (*if marshaling then (infomsg ["Found recursive access"; fdec.svar.vname; fdec2.svar.vname]);*)

      let ret_stmts = ref [] in
      (* 1. Generate a statement to save the state of the LV. I automatically
       * generate a save statement for the POINTER to the LV type, as this is
       * the common case. The validity of this needs to be checked on a per-case
       * basis. We do this only if we need to restore the state later *)
      let undereflvopt = (underef_lval lv) in
      let stsv_var = ref dummyFunDec.svar in
      (match undereflvopt with
         | Some(_) ->
             let stsvname = "svdst" ^ (makevarname (lval_tostring lv)) in
             let stsvtyp = (TPtr(lvtyp, [])) in
             let addroflv = (mkAddrOf lv) in
             stsv_var := (get_local_variable fdec stsvtyp stsvname);
             let stsv_set = Set((lvalify_varinfo !stsv_var), addroflv, locUnknown) in
             let stmt_stsv_set = (mkStmt (Instr [stsv_set])) in
             ret_stmts := (List.append !ret_stmts [stmt_stsv_set]);
         | None ->
             (* Even in this case, let's generate a statement to save state *)
             let stsvname = "svdst" ^ (makevarname (lval_tostring lv)) in
             let stsvtyp = (TPtr(lvtyp, [])) in
             let addroflv = (mkAddrOf lv) in
             stsv_var := (get_local_variable fdec stsvtyp stsvname);
             let stsv_set = Set((lvalify_varinfo !stsv_var), addroflv, locUnknown) in
             let stmt_stsv_set = (mkStmt (Instr [stsv_set])) in
             (addwarn ["You might need to replace"; (lval_tostring lv); "with"; !stsv_var.vname]);
             ret_stmts := (List.append !ret_stmts [stmt_stsv_set]);
      );
      (* 2. Generate the statements to marshal/demarshal the fields: Done above already. *)
      (* 3. Embed (2) in a loop. Also update the state (dummy inserted) *)
      (* 3.1. Create a dummy variable to represent the sentinel *)
      let sntname = "SENTINEL" ^ (makevarname (lval_tostring lv)) in
      (*(Printf.fprintf stderr "%s\n" sntname);*)
      let snt_var = get_local_variable fdec intType sntname in
      let snt_init = Set((lvalify_varinfo snt_var), (Cil.integer 1), locUnknown) in
      let stmt_snt_init = (mkStmt (Instr [snt_init])) in
      (* 3.2. Create the conditional to break out of the loop *)
      let snt_exp = expify_lval (lvalify_varinfo snt_var) in
      (* 3.3. The last statement in the loop should be the update of the
       * SENTINEL. We generate one that sets the SENTINEL to 0 *)
      let snt_reset = Set((lvalify_varinfo snt_var), zero, locUnknown) in
      let stmt_snt_reset = (mkStmt (Instr [snt_reset])) in
      (* 3.4. Create the loop itself. *)
      let loop_body = List.append !m_dm_stmts [stmt_snt_reset] in
      let stmts_loop = mkWhile ~guard:(snt_exp) ~body:(loop_body) in
      ret_stmts := List.append !ret_stmts [stmt_snt_init];
      ret_stmts := List.append !ret_stmts stmts_loop;
      (* 4. Follow it up with a statement to restore the state of the LV *)
      (match undereflvopt with
         | Some(undereflv) ->
             let stsv_exp = (expify_lval (lvalify_varinfo !stsv_var)) in
             let stsv_restore = Set(undereflv, stsv_exp, locUnknown) in
             let stmt_stsv_restore = (mkStmt (Instr [stsv_restore])) in
             ret_stmts := (List.append !ret_stmts [stmt_stsv_restore]);
         | None -> ((* No need to restore state in this case *));
      );
      (clear_recaccess());
      !ret_stmts;
    end
    else begin
      !m_dm_stmts;
    end;
    *)

    !m_dm_stmts; (* Eliminated SENTINEL-guarded while loops since
                    they've never once been used *)
  end
(*---------------------------------------------------------------------------*)
(** Insert marshaling/demarshaling code at the call-site stub. Return the list of
 * statements to be inserted before the function call.
 * (1) lv: this denotes the lval of the variable to be demarshed.
 * (2) lvtype: this denotes the type of the lval.
 * (3) fdec: This corresponds to the function within the generated
 *     (de)marshaling code will be placed.
 * (4) fdec2: this denotes the fundec of the function corresponding to which
 *     we're generating (de)marshaling code. Typically fdec2 and fdec will differ
 *     when we're generating (de)marshaling code for glue functions.
 *     fdec and fdec2 are the same if we're dealing with an internal kernel
 *     function, e.g. __MARSH_WRAP__register_netdev.  They differ if we're not,
 *     e.g. e1000_blah vs __MARSH_WRAP__e1000_blah.
 * (5) ptg: points-to-graph obtained after marshaling analysis.
 * (6) resfld: resolved fields information, for opaque pointers.
 * (7) resform: resolved formal parameter information, for opaque pointers.
 * (8) m_dm_buf: the field from which we're (de)marshaling data.
 * (9) m_dm_off: the offset into the buffer from which we're (de)marshaling data.
 * (10) stkdpth: the stack depth, used during recursion
 * (11) marshaling: true if we're generating marshaling code, false if
 *      generating demarshaling code
 * (12) gen_kern: true if generating code in the master, false if
 *      generating code in the slave.
 *)
and unified_dfs (lv: lval)
    (lvtyp: typ)
    (fdec: fundec)
    (fdec2: fundec)
    (ptg: (string, (typ * string * string)) Hashtbl.t)
    (resfld: (string, (typ * fieldinfo * typ)) Hashtbl.t)
    (resform: (string, (int * varinfo * typ)) Hashtbl.t)
    (m_dm_buf: varinfo)
    (m_dm_off: varinfo)
    (stkdpth: int)
    (marshaling: bool)
    (gen_kern: bool)
    : stmt list =
  begin
    (* Check if we are potentially in an infinite loop. If in infinite loop,
       nothing is returned *)
      
    Printf.fprintf stderr "unified_dfs: for fn1:%s fn2:%s field:%s type %s lval
    %s.\n"
    fdec.svar.vname fdec2.svar.vname m_dm_buf.vname (typ_tostring lvtyp) 
    (lval_tostring lv);

    if (stkdpth > !threshold_stackdepth) then
      begin
        addwarn ["Potentially an infinite loop in unified_dfs:";
                 fdec.svar.vname; " "; fdec2.svar.vname; " ";
                 (lval_tostring lv); " "; (typ_tostring_noattr lvtyp)];
        [];
      end
    else if (is_on_marshstack (typ_tostring_noattr lvtyp)) = true then
      begin
        (* We now check if we're (de)marshaling a recursive structure. We stop when
         * we see a node we've visited before. *)
        set_recaccess ();
        [];
      end
    else
      begin
        (* Type to be marshaled not on stack. Normal marshaling *)
        (* Is v a variable that we can generate (de)marshaling code for directly? *)
        let m_dm_v_stmts = gen_unified_m_dm fdec lv None lvtyp
          m_dm_buf m_dm_off fdec2 ptg resfld resform
          stkdpth marshaling gen_kern
        in
        if (List.length m_dm_v_stmts) <> 0 then
          (* We must have just generated some marshaling code, so that's it *)
          m_dm_v_stmts
        else if (isPointerType lvtyp) && (isVoidPtrType lvtyp) = false then  (

        Printf.fprintf stderr "Non-void ptr unified_dfs: for fn1:%s fn2:%s field:%s.\n"
        fdec.svar.vname fdec2.svar.vname m_dm_buf.vname;     
          unified_nonvoid_ptr lv lvtyp fdec
            fdec2 ptg resfld resform
            m_dm_buf m_dm_off stkdpth marshaling gen_kern;  )
        else if (isPointerType lvtyp) && (isVoidPtrType lvtyp) = true then (
        Printf.fprintf stderr "Void ptr unified_dfs: for fn1:%s fn2:%s field:%s.\n"
        fdec.svar.vname fdec2.svar.vname m_dm_buf.vname;     
          unified_void_ptr lv lvtyp fdec
          fdec2 ptg resfld resform ;)
            m_dm_buf m_dm_off stkdpth marshaling gen_kern
        else if (isCompoundType lvtyp) && (is_iomem lvtyp) = false then (
        Printf.fprintf stderr "isCompoundType unified_dfs: for fn1:%s fn2:%s field:%s.\n"
        fdec.svar.vname fdec2.svar.vname m_dm_buf.vname;     
          unified_compound lv lvtyp fdec
            fdec2 ptg resfld resform
            m_dm_buf m_dm_off stkdpth marshaling gen_kern; )
        else
          begin
            (* Example:  We may be marshaling a function pointer.  That's OK, and is supported. *)
            (*let what_we_are_doing = if marshaling then "marshaling" else "demarshaling" in
              addwarn ["This case is currently not supported in "; what_we_are_doing; ":";
                     (lval_tostring lv); (typ_tostring_noattr lvtyp); fdec.svar.vname;
                     fdec2.svar.vname];
            *)
            []
          end
      end (* Else case of infinite loop warning *)
  end
