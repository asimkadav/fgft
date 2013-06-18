(*===========================================================================*)
(*
 * CIL module to generate XDR specification automatically.
 * Matt Renzelmann <mjr@cs.wisc.edu> Oct. 11, 2008
 *)
(*===========================================================================*)

open Cil
open Scanf
open Utils_dri            (* General utilities *)
open Marshannot_dri       (* For handling array size annotation *)

(* Add this string to the end of all structs, enums.  Add it
   to end of any kernel functions that conflict with user-mode equivalents *)
let kernel_suffix : string = "_kernel";;

(* These conflict with functions already available in user mode *)
let redefined_functions : string list =
  [ "__cmsg_nxthdr";
    "outb";
    "inb";
    "outb_p";
    "inb_p";
    "outsb";
    "insb";
    "outw";
    "inw";
    "outw_p";
    "inw_p";
    "outsw";
    "insw";
    "outl";
    "inl";
    "outl_p";
    "inl_p";
    "outsl";
    "insl";
    "strcpy";
    "strncpy";
    "strcat";
    "strncat";
    "strcmp";
    "strncmp";
    "strchr";
    "strrchr";
    "strlen";
    "memchr";
    "sprintf";
    "vsprintf";
    "snprintf";
    "vsnprintf";
    "sscanf";
    "memmove";
    "strstr";
    "strpbrk";
    "strsep";
    "strspn";
    "strcspn";
    "in6addr_loopback";
    "sprintf";
    "vsprintf";
    "snprintf";
    "vsnprintf";
    "sscanf";
    "memmove";
    "strstr";
    "strpbrk";
    "strsep";
    "strspn";
    "strcspn";
  ];;


(*---------------------------------------------------------------------------*)
class analyze_headers = object (self)
  inherit nopCilVisitor  

  (* Return true if the global is a function or function prototype *)
  method initial_proto_filter (glob: global) : bool =
    match glob with
      | GType(t, _) -> false;
      | GCompTag(c, _) -> false;
      | GCompTagDecl(c, _) -> false;
      | GEnumTag(e, _) -> false;
      | GEnumTagDecl(e, _) -> false;
      | GVarDecl(v, _) ->
          (match v.vtype with
             | TFun(_,_,_,_) -> true;
             | _ -> false;
          );
      | GVar(v, i, _) -> false; 
      | GFun(f, _) -> true;
      | GAsm(s, _) -> false;
      | GPragma(a, _) -> false;
      | GText (t) -> false;

  (* Look at an instr, remove all typedefs *)
  method simplify_instr (ins : instr) : instr = 
    match ins with
      | Set (lv, ex, l) -> Set (self#simplify_lval lv, self#simplify_exp ex, l);
      | Call (lv_option, e1, e2, l) ->
          (match lv_option with
             | Some (lv) ->
                 Call (Some (self#simplify_lval lv), 
                       (self#simplify_exp e1),
                       (List.map self#simplify_exp e2),
                       l);
             | None ->
                 Call (None, 
                       (self#simplify_exp e1),
                       (List.map self#simplify_exp e2),
                       l);
          );
      | Asm (attrs, str_list, list1, list2, str_list2, l) ->
          let simplify_list1 (str1, str2, lv) =
            (str1, str2, self#simplify_lval lv) in
          let simplify_list2 (str1, str2, e) =
            (str1, str2, self#simplify_exp e) in
          Asm (attrs,
               str_list,
               List.map simplify_list1 list1,
               List.map simplify_list2 list2,
               str_list2,
               l);

  (* Remove all typedefs from a list of instrs *)
  method simplify_instrs (instr_list : instr list) : instr list =
    List.map self#simplify_instr instr_list;
    
  (* Remove all typedefs from a stmt *)
  method simplify_stmt (s: stmt) : stmt =
    s.skind <-
      (match s.skind with
         | Instr (list) ->
             Instr (self#simplify_instrs list);
         | Return (ret_option, l) ->
             (match ret_option with
                | Some (ret) -> Return (Some (self#simplify_exp ret), l);
                | None -> Return (None, l);
             );
         | If (ex, b1, b2, l) -> 
             If (self#simplify_exp ex, b1, b2, l);
         | Switch (ex, b, stmts, l) ->
             Switch (self#simplify_exp ex, b, List.map self#simplify_stmt stmts, l);
         | Loop (b, l, s1_option, s2_option) ->
             b.bstmts <- List.map self#simplify_stmt b.bstmts;
             (match s1_option with
                | Some (s1) -> 
                    (match s2_option with
                       | Some (s2) ->
                           Loop (b, l, Some (self#simplify_stmt s1), Some (self#simplify_stmt s2));
                       | None ->
                           Loop (b, l, Some (self#simplify_stmt s1), None);
                    );
                | None ->
                    (match s2_option with
                       | Some (s2) ->
                           Loop (b, l, None, Some (self#simplify_stmt s2));
                       | None ->
                           Loop (b, l, None, None);
                    );
             );
         | _ -> s.skind;
      );
    s;

  (* Rewrite functions into function prototypes, rename functions,
     adjust type of function if needed *)
  method create_protos (glob: global) : global =
    match glob with
      | GVarDecl(vi, attr) ->
          (match vi.vtype with
             | TFun(t,l,b,a) ->
                 (* Remove typedefs in the function definition *)
                 vi.vtype <- self#process_fun t l b a;

                 (* Rename the function if needed *)
                 if (List.mem vi.vname redefined_functions) then
                   vi.vname <- vi.vname ^ kernel_suffix;

                 (* Remove storage qualifiers *)
                 vi.vstorage <- NoStorage;
                 (* Remove inline *)
                 vi.vinline <- false;

                 (* Return the prototype *)
                 GVarDecl (vi, attr);
             | _ ->
                 Printf.fprintf stderr "! Error check this case\n";
                 glob;
          );
      | GFun (fd, attr) ->
          Cil.prepareCFG fd;

          (* false = per-function stmt numbering,
             true = global stmt numbering *)
          Cil.computeCFGInfo fd false;

          (* Simplify stmts in the function.  Looks at all stmts in the body. *)
          fd.sallstmts <- List.map self#simplify_stmt fd.sallstmts;

          let simplify_varinfo vi = vi.vtype <- self#simplify_typ vi.vtype in
          List.iter simplify_varinfo fd.slocals;
          (match fd.svar.vtype with
             | TFun(t,l,b,a) ->
                 (* Remove typedefs from the function definition *)
                 fd.svar.vtype <- self#process_fun t l b a;

                 (* Rename the function if needed *)
                 if (List.mem fd.svar.vname redefined_functions) then
                   fd.svar.vname <- fd.svar.vname ^ kernel_suffix;

                 (* Remove storage qualifiers *)
                 fd.svar.vstorage <- NoStorage;
                 (* Remove inline *)
                 fd.svar.vinline <- false;

                 (* Return the prototype.  Note we're converting a function
                    that includes a body into just a prototype *)
                 GVarDecl (fd.svar, attr);
                 (*GFun(fd, attr);*)
             | _ ->
                 Printf.fprintf stderr "! Error check this case\n";
                 glob;
          );
      | _ ->
          Printf.fprintf stderr "!! Error check this case\n";
          glob;

  (* Return all structure definitions, prototypes, enums, and enum prototypes *)
  method initial_forwarddecl_filter (glob: global) : bool =
    match glob with
      | GType(t, _) -> false;
      | GCompTag(c, _) -> true;
      | GCompTagDecl(c, _) -> true;
      | GEnumTag(e, _) -> true;
      | GEnumTagDecl(e, _) -> true;
      | GVarDecl(v, _) -> true;
      | GVar(v, i, _) -> true;
      | GFun(f, _) -> false;
      | GAsm(s, _) -> false;
      | GPragma(a, _) -> false;
      | GText (t) -> false;

  (* Simplify the structure/enum definition *)
  method create_forward_decl (glob: global) : global =
    let new_glob = ref glob in
    (match glob with
      | GCompTag(c, l) ->
          (* Look at all fields in the structure and remove
             all typedefs *)
          List.iter (function finfo ->
                       finfo.ftype <- (self#simplify_typ finfo.ftype)
                    ) c.cfields;
          (* Remove name conflicts *)
          c.cname <- c.cname ^ kernel_suffix;
          new_glob := GCompTag(c, l);
      | GCompTagDecl(c, _) -> new_glob := glob;
      | GEnumTag(e, l) ->
          (* Remove name conflicts *)
          e.ename <- e.ename ^ kernel_suffix;

          (* Rename all elements within the enum *)
          let xform ((s : string), (e : exp), (l : location)) =
            (s ^ kernel_suffix, e, l) in
          let new_items = List.map xform e.eitems in
          e.eitems <- new_items;
          new_glob := GEnumTag(e, l);
      | GEnumTagDecl(e, _) -> new_glob := glob;
      | GVarDecl(vi, l) ->
          vi.vtype <- self#simplify_typ vi.vtype;
          new_glob := GVarDecl (vi, l);
      | GVar(vi, init, l) ->
          vi.vtype <- self#simplify_typ vi.vtype;
          if vi.vstorage = Static then
            vi.vstorage <- Extern;
          new_glob := GVar (vi, init, l);
      | _ ->
          Printf.fprintf stderr "!!! Error check this case\n";
          new_glob := glob;
    );
    !new_glob;
    
  (* Remove all typedefs from an lval.  These come up
     in the case of casts, e.g. *((mytypedef * ) blah) = ... *)
  method simplify_lval (lv: lval) : lval =
    let (lhost, offset) = lv in
    (match lhost with
       | Var (vi) ->
           vi.vtype <- self#simplify_typ vi.vtype;
           (Var (vi), offset);
       | Mem (ex) ->
           (Mem (self#simplify_exp ex), offset);
    );

  (* Given an expression, remove all typedefs within it. *)
  method simplify_exp (orig_exp: exp) : exp =
    let new_exp = ref orig_exp in
    (match orig_exp with
       | Lval (lv) ->
           new_exp := Lval (self#simplify_lval lv);
       | SizeOf (t) ->
           new_exp := SizeOf (self#simplify_typ t);
       | SizeOfE (e) ->
           new_exp := SizeOfE (self#simplify_exp e);
       | UnOp (unop, e, t) ->
           new_exp := UnOp (unop, (self#simplify_exp e), (self#simplify_typ t));
       | BinOp (binop, e1, e2, t) ->
           new_exp := BinOp (binop, (self#simplify_exp e1), (self#simplify_exp e2), (self#simplify_typ t));
       | CastE (t, e) ->
           new_exp := CastE (self#simplify_typ t, self#simplify_exp e);
       | _ -> new_exp := orig_exp;
    );
    !new_exp;

  (* Given a typ, remove all typedefs by replacing them
     with their equivalent primitives.  Note this function
     strips all type attributes, including const, and
     __attribute__.  These should not be necessary in the
     output.  If they are, change the [] to the letter 'a'.
  *)
  method simplify_typ (orig_typ: typ) : typ =
    let new_typ = ref orig_typ in
    (match orig_typ with
      | TPtr(typ, a) ->
          new_typ := TPtr (self#simplify_typ typ, []);
      | TArray(typ, exp_option, a) ->
          (match exp_option with
             | Some (exp) ->
                 new_typ := TArray (self#simplify_typ typ, Some (self#simplify_exp exp), []);
             | None -> 
                 new_typ := TArray (self#simplify_typ typ, None, []);
          );
      | TFun (typ, list_opt, b, a) ->
          new_typ := self#process_fun typ list_opt b [];
      | TNamed(ti, a) ->
          new_typ := self#simplify_typ ti.ttype;
      | _ -> new_typ := Utils_dri.strip_typ_attribs orig_typ;
    );
    !new_typ;

  (* Given a function definition, see TFun, gets rid of all
     typedefs in the parameter list and return value *)
  method process_fun
    (t: typ)
    (list_opt: (string * typ * attributes) list option)
    (b: bool)
    (a: attributes) : typ =
    match list_opt with
      | Some(list) ->
          let xform (str, curtyp, attr) =
            (str, (self#simplify_typ curtyp), attr) in
          let new_params = List.map xform list in
          TFun ((self#simplify_typ t), Some (new_params), b, a);
      | None ->
          TFun ((self#simplify_typ t), None, b, a);
          
  (* Execute the main simplification program *)
  method do_gen_header (f: file) : unit  =
    let new_protos_yesno = List.filter self#initial_proto_filter f.globals in
    let new_protos = List.map self#create_protos new_protos_yesno in
    let new_forwarddecl_yesno = List.filter self#initial_forwarddecl_filter f.globals in
    let new_forwarddecl = List.map self#create_forward_decl new_forwarddecl_yesno in
    let final_globals = new_forwarddecl @ new_protos in
    f.globals <- final_globals;
end

(* Generate all global variables and put them in one file.
   These are all the globals referenced in the kernel headers. *)
class analyze_headers_variables = object (self)
  inherit nopCilVisitor

  (* Return true if the global is a variable *)
  method initial_filter (glob: global) : bool =
    let filter_vars vstorage vtype =
      (match vtype with
         | TVoid (_) -> false;
         | TInt(_,_) ->
             if vstorage = Register then (* Don't redefine register variables in the .C file *)
               false
             else
               true;
         | TFloat(_,_) -> true;
         | TPtr(_,_) -> true;
         | TArray(_,_,_) -> true;
         | TFun(_,_,_,_) -> false;
         | TNamed(_,_) -> false;
         | TComp(_,_) -> true;
         | TEnum(_,_) -> true;
         | TBuiltin_va_list(_) -> false;
      ) in
    match glob with
      | GType(t, _) -> false;
      | GCompTag(c, _) -> false;
      | GCompTagDecl(c, _) -> false;
      | GEnumTag(e, _) -> false;
      | GEnumTagDecl(e, _) -> false;
      | GVarDecl(v, _) -> (filter_vars v.vstorage v.vtype);
      | GVar(v, i, _) -> (filter_vars v.vstorage v.vtype);
      | GFun(f, _) -> false;
      | GAsm(s, _) -> false;
      | GPragma(a, _) -> false;
      | GText (t) -> false;
          
  (* True if the string contains kernel_suffix, false otherwise. *)
  method contains_kernel_suffix (s: string) : bool =
    let str = ".*" ^ kernel_suffix ^ "$" in
    let kernel_suffix_regexp = Str.regexp str in
    Str.string_match kernel_suffix_regexp s 0;

  (* Given a varinfo, change its storage to "nostorage" which is
     neither static, nor extern (i.e. normal) *)
  method fixvarinfo (vi: varinfo) : unit =
    match vi.vtype with
      | TVoid (_) ->
          Printf.fprintf stderr "Error 1! Check this case.\n";
      | TInt(_,_) ->
          vi.vstorage <- NoStorage;
      | TFloat(_,_) ->
          vi.vstorage <- NoStorage;
      | TPtr(_,_) ->
          vi.vstorage <- NoStorage;
      | TArray(t,_,_) ->
          vi.vstorage <- NoStorage;
      | TFun(_,_,_,_) ->
          Printf.fprintf stderr "Error 2! Check this case.\n";
      | TNamed(_,_) ->
          Printf.fprintf stderr "Error 3! Check this case.\n";
      | TComp(ci,_) ->
          (*if self#contains_kernel_suffix ci.cname = false then
            ci.cname <- ci.cname ^ kernel_suffix;*)
          vi.vstorage <- NoStorage;
      | TEnum(ei,_) ->
          (*if self#contains_kernel_suffix ei.ename = false then
            ei.ename <- ei.ename ^ kernel_suffix;*)
          vi.vstorage <- NoStorage;
      | TBuiltin_va_list(_) ->
          Printf.fprintf stderr "Error 4! Check this case.\n";

  (* Get rid of the extern specifiers on all the globals.
     This effectively converts them into normal globals.
  *)
  method convert_nonextern (glob: global) : global = 
    match glob with
      | GVarDecl(v, l) ->
          (* Convert VarDecl to Var.  The difference is that the GVar
             can be initialized. *)
          self#fixvarinfo v;
          let i : initinfo = {
            init = None;
          } in 
          GVar(v, i, l);
      | GVar(v, i, l) ->
          self#fixvarinfo v;
          GVar(v, i, l);
      | _ ->
          Printf.fprintf stderr "Error 5!! Check this case\n";
          glob;

  (* The point of the user mode list of globals is so we don't have
     to fix all the "extern"'s in the kernel header files in user mode.
     If we just remove "extern" on arrays, however, we get warnings
     because the array looks like it's "empty."  This code fixes the arrays
     so they have 1 element, thus supressing the error.  Note that if you
     ever try marshaling data into/out of these arrays, there will likely
     be problems since they're not the right size. *)
  method fix_arrays (glob: global) : global =
    match glob with
      | GVar(v, i, l) ->
          v.vtype <-
            (match v.vtype with
               | TArray(t, size_option, a) ->
                   (match size_option with
                      | None ->
                          TArray(t, Some (Cil.integer 1), a);
                      | _ ->
                          v.vtype;
                   );
               | _ -> v.vtype;
            );
          GVar(v, i, l);
      | _ -> glob;

  (* Scan the files and extract all globals. *)
  method do_gen_globs (f: file) : unit =
    let new_globs_yesno = List.filter self#initial_filter f.globals in
    let new_globs = List.map self#convert_nonextern new_globs_yesno in
    let fixed_globs = List.map self#fix_arrays new_globs in
    f.globals <- fixed_globs;
end

(* Generate all global variables and put them in one file.
   These are all the globals referenced in the kernel headers. *)
class analyze_headers_typedefs = object (self)
  inherit nopCilVisitor

  (* Return true if the global is a variable *)
  method initial_filter (glob: global) : bool =
    match glob with
      | GType(t, _) -> true;
      | GCompTag(c, _) -> false;
      | GCompTagDecl(c, _) -> false;
      | GEnumTag(e, _) -> false;
      | GEnumTagDecl(e, _) -> false;
      | GVarDecl(v, _) -> false;
      | GVar(v, i, _) -> false;
      | GFun(f, _) -> false;
      | GAsm(s, _) -> false;
      | GPragma(a, _) -> false;
      | GText (t) -> false;
          
  method do_gen_typedefs (f: file) : unit =
    let new_globs_yesno = List.filter self#initial_filter f.globals in
    f.globals <- new_globs_yesno;
end

(* Generates a header file intended to be included in the
   Jeannie driver source code.  Can safely be included in
   multiple Jeannie source files if we ever get that to work *)
let do_gen_header
    (f: file) : unit =
  let obj : analyze_headers = new analyze_headers in
  obj#do_gen_header f;;

(* Generates a C file that contains all the global variables
   ever mentioned in any linux header file.  Removes extern
   storage.  Intended to be compiled against user mode code *)
let do_gen_globs
    (f: file) : unit =
  Printf.fprintf stderr "Generating globals\n";
  let source_obj : analyze_headers_variables = new analyze_headers_variables in
  source_obj#do_gen_globs f;;

(* Generates a header file containing only typedefs *)
let do_gen_typedefs
    (f: file) : unit =
  let obj : analyze_headers_typedefs = new analyze_headers_typedefs in
  obj#do_gen_typedefs f;;

