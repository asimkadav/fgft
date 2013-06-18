(*===========================================================================*)
(*
 * CIL Module to implement the new locking protocol. Some transformations in
 * this module need to be implemented BEFORE splitting, and some need to be
 * implemented AFTER splitting. 
 *
 * We need to modify lock data structures to combo_lock data structures 
 * BEFORE splitting, so we correctly generate the marshaling code for this
 * data structure. We need to modify function names AFTER splitting (in fact,
 * this can be done before as well; we will only need to modify the splitter
 * to not complain if it doesn't find "user"/"kern" annotations for the 
 * renamed functions).
 *
 * Vinod Ganapathy <vg@cs.wisc.edu>, 19 June 2007.
 *)
(*===========================================================================*)

(* 
 * Ensure that the definitions of the spinlock wrapper functions have been
 * inserted BEFORE you proceed with marshaling. Otherwise, the corresponding
 * fields of the combolock structure will NOT be marshaled.
 *
 * BEFORE Xform.
 * 1. Insert a combo_lock declaration.
 * 2. Insert a combo_lock in place of a spin_lock in structure declarations.
 *
 * AFTER Xform.
 * 1. Replace function calls with calls to their wrappers.
 * 2. Check that the appropriate fields of the combolock structure are 
 *    being marshaled.
 *)

open Cil
open Str
open Scanf
open Utils_dri
open Marshannot_dri

(*---------------------------------------------------------------------------*)
(* Declare the C struct for the combo_lock data structure, which is used in 
 * the new locking protocol *)
let struct_combolock_spin = "spin"
let struct_combolock_semaphore = "sem"
let struct_combolock_unlocksem = "unlock_sem"
let spinlock_name = "spinlock_t"
let semaphore_name = "semaphore"
let combolock_name = "combo_lock"

(* spinlock_typ and semaphore_typ are currently declared as VOID types. They
 * will get initialized to their correct values when we walk the globals
 * searching for the semaphore type and the spinlock type *)
let spinlock_typ = ref voidType 
let semaphore_typ = ref voidType 

let func_cons_struct_combolock (c: compinfo) = 
  begin
    let field1 = (struct_combolock_spin, !spinlock_typ, None, [], locUnknown) in
    let field2 = (struct_combolock_semaphore, !semaphore_typ, None, [], locUnknown) in
    let field3 = (struct_combolock_unlocksem, intType, None, [], locUnknown) in
    [field1; field2; field3]
  end
    
let struct_combolock_compinfo () = 
  (mkCompInfo true combolock_name func_cons_struct_combolock [])

let struct_combolock_typ = (TComp(struct_combolock_compinfo(), []))

(*---------------------------------------------------------------------------*)
(* Synchronization functions that use a combolock *)
let combolock_functions : (string, bool) Hashtbl.t = (Hashtbl.create 117)
  (* Synchronization functions that do not use a combolock *)
let noncombolock_functions : (string, bool) Hashtbl.t = (Hashtbl.create 117)

(* The wrapper extension to be used *)
let wrapext = ref "_dummy"

(*---------------------------------------------------------------------------*)
(** Combo lock xformer: the AFTER transformation. *)
(* This object contains the implementation of methods that rename spinlock
 * functions into their wrappers *)
class combolock_rename_xformer = object (self)
  inherit nopCilVisitor
    (* Does the input string represent the name of a
       recognized combolock function? *)
  method is_str_combolock_function (fname: string) : bool = 
    begin
      try
        (ignore (Hashtbl.find combolock_functions fname));
        true;
      with Not_found -> (false);
    end

  (* Does the input string represent the name of a
     recognized non-combolock function? *)
  method is_str_noncombolock_function (fname: string) : bool = 
    begin
      try
        (ignore (Hashtbl.find noncombolock_functions fname));
        true;
      with Not_found -> (false);
    end

  method is_combolock_passed (expargl: Cil.exp list) : bool =
    begin
      let retval = ref false in
      let i = ref 0 in
      while !i < (List.length expargl) && !retval = false do
        let ith = (List.nth expargl !i) in
        let arg_type = typeOf (stripCasts ith) in
        let arg_details = (match arg_type with
                             | TPtr (thetype, _) ->
                                 (match thetype with
                                    | TComp (cinfo, _) -> cinfo.cname;
                                    | _ -> ""
                                 )
                             | _ -> ""
                          ) in
        let type_details = combolock_name in
        if arg_details = type_details then begin
          retval := true
        end else begin
          retval := false
        end;
        i := !i + 1
      done;
      !retval;
    end

  (* Pass in expargl as parameter.  Check type of each expression and see if it the type is a combolock.
     We can also check to see if it's a combolock by looking at type annotations.

     marshannot has a function called is_combolock:  searches for the combo lock attribute in the type
     attributes of the input type.

     Extract the type of the expression and pass it to is_combolock.
     Print the types of the parameters (use utils:  typ_tostring).  To actually print onto screen,
     use Printf.
  *)
  method print_types (vname: string)
    (expargl: Cil.exp list) : unit =
    begin 
      (Printf.fprintf stderr " %s\n%!" "MattStart");
      (Printf.fprintf stderr " %s\n%!" vname);
      for i = 0 to (List.length expargl) - 1 do
        let ith = (List.nth expargl i) in
        let arg_type = (typ_tostring (typeOf (stripCasts ith))) in
        let arg_name = (exp_tostring ith) in
        let arg_combolock = (is_combolock (typeOf ith)) in
        let arg_combolock2 = (self#is_combolock_passed expargl) in
        Printf.fprintf stderr " %s %s %B %B\n%!" arg_type arg_name arg_combolock arg_combolock2;
      done;
    end
      
  (* Does the input expression represent a recognized spin_lock function?
   * If yes, then return the type of the function.
   * The bool indicates whether we should replace the function.
   *)
  method is_exp_replaceable (expfn: exp)
    (expargl: Cil.exp list) :
    (bool * string * typ) = 
    begin
      (match expfn with
         | Lval (lh, off) ->
             (match lh with
                | Var(vinfo) ->
                    (*
                     * The first bool is whether it's a combolock function
                     * The second bool is whether it's a non-combolock function
                     * The third bool is whether a combolock is actually passed as a parameter
                     * to the combolock function.
                     * We replace the function with a wrapper if (booll && bool3) || bool2
                     *)
                    let is_combolock = self#is_str_combolock_function vinfo.vname in
                    let is_noncombolock = self#is_str_noncombolock_function vinfo.vname in
                    let is_replaceable = (is_combolock && (self#is_combolock_passed expargl)) || is_noncombolock in
                    (is_replaceable, vinfo.vname, vinfo.vtype);
                | _ -> (false, "", voidType);
             );
         | _ -> (false, "", voidType);
      );
    end

  (* Get the new function name from the old function name and the appropriate
   * extension *)
  method get_wrapped_lockfn_name (oldfname: string) : string = 
    begin
      oldfname ^ !wrapext;
    end

  (* The visitor that visits each instruction, and replaces calls to
   * spinlock related functions with their wrappers *)
  method vinst (i: instr) : instr list visitAction =
    begin
      (match i with
         | Call(lvopt, expfn, expargl, loc) ->
             let (replaceable, fnname, fntyp) = (self#is_exp_replaceable expfn expargl) in
             if replaceable
             then begin
               let lockwrap_func_name = (self#get_wrapped_lockfn_name fnname) in
               let lockwrap_fundec = (emptyFunction lockwrap_func_name) in
               let lockwrap_func = (expify_fundec lockwrap_fundec) in
               let call_lockwrap_func = Call(lvopt, lockwrap_func, expargl, loc) in
               let newilist = [call_lockwrap_func] in
               ChangeTo(newilist);  
             end 
             else begin
               DoChildren;
             end;
         | _ -> DoChildren;
      )
    end

  (* Top-level function for the combolock transformation *)
  method top_level (f: file) : unit =
    begin
      (visitCilFile (self :> cilVisitor) f);
    end
end

(*---------------------------------------------------------------------------*)
(** Populate the list of spinlock functions *)
let populate_wrapper_functions () : unit = 
  begin
    let add2combolockfn (fnm: string) = (add_if combolock_functions fnm true) in
    let add2noncombolockfn (fnm: string) = (add_if noncombolock_functions fnm true) in

    (* <Names of spin lock functions go here> *)
    let combolockfuncs = 
      "__spin_lock_init" ::
        "_spin_lock" ::
        "_spin_lock_bh" ::
        "_spin_lock_irqsave" ::
        "_spin_lock_irq" ::
        "_spin_unlock" ::
        "_spin_unlock_bh" ::
        "_spin_unlock_irqrestore" ::
        "_spin_unlock_irq" ::
        [] in

    let noncombolockfuncs =
      "__mutex_init" ::
        "mutex_lock" ::
        "mutex_unlock" ::
        "mutex_lock_interruptible" ::
        [] in
    
    (* Will we ever need the type signatures of these functions? *)
    for i = 0 to (List.length combolockfuncs) - 1 do
      let ith = (List.nth combolockfuncs i) in
      (add2combolockfn ith);
    done;

    for i = 0 to (List.length noncombolockfuncs) - 1 do
      let ith = (List.nth noncombolockfuncs i) in
      (add2noncombolockfn ith);
    done;
  end

(*---------------------------------------------------------------------------*)
(* Locking transformations that must be performed AFTER splitting. *)

(* do_lockxform_after_split: 
 * The main function. Here we replace all calls to the spinlock functions 
 * with calls to their wrappers instead *)
let do_lockxform_after_split (f: file) (dowhat: string) : unit = 
  begin
    (match dowhat with
       | "produce-user" -> wrapext := "_wrapper_user"; 
       | "produce-kern" -> wrapext := "_wrapper_kern";
       | _ -> (fatal ["Unrecognized command to do_lockxform"]);
    );
    (populate_wrapper_functions());
    let obj_lockxform : combolock_rename_xformer 
        = (new combolock_rename_xformer) in
    (obj_lockxform#top_level f);
  end

(*---------------------------------------------------------------------------*)
(* Locking transformations that must be performed BEFORE splitting. *)

(* Insert combolock declaration immediately following semaphore and spinlock
 * declarations. This function also fetches the definitions of the spinlock
 * and the semaphore types *)
let insert_combolock_declaration (gl: global list) : global list = 
  begin
    let found_spin_decl = ref false in
    let found_sema_decl = ref false in
    let declared_combolock = ref false in
    (* Is this a file with a combolock declaration? If so, don't redeclare *)
    for i = 0 to (List.length gl) - 1 do
      let ith = (List.nth gl i) in
      (match ith with
         | GCompTag(cinfo,loc) -> (* C Structs *)
             if (String.compare cinfo.cname combolock_name) = 0
             then declared_combolock := true;
         | _ -> ();
      );
    done;
    let retval = ref [] in
    for i = 0 to (List.length gl) - 1 do
      let ith = (List.nth gl i) in
      (match ith with
         | GType(tinfo, loc) -> (* Typeinfos *)
             if (String.compare tinfo.tname spinlock_name) = 0
             then begin
               found_spin_decl := true;
               spinlock_typ := TNamed(tinfo, []);
             end;
             if (String.compare tinfo.tname semaphore_name) = 0
             then begin
               found_sema_decl := true;
               semaphore_typ := TNamed(tinfo, []);
             end;
             retval := (List.append !retval [ith]);
         | GCompTag(cinfo,loc) -> (* C Structs *)
             if (String.compare cinfo.cname spinlock_name) = 0
             then begin
               found_spin_decl := true;
               spinlock_typ := TComp(cinfo, []);
             end;
             if (String.compare cinfo.cname semaphore_name) = 0
             then begin
               found_sema_decl := true;
               semaphore_typ := TComp(cinfo, []);
             end;
             retval := (List.append !retval [ith]);
         | _ -> retval := (List.append !retval [ith]);
      );
      if (!found_sema_decl = true) && 
        (!found_spin_decl = true) &&
        (!declared_combolock = false)
      then begin
        retval :=
          (List.append !retval [GCompTag(struct_combolock_compinfo(), locUnknown)]);
        declared_combolock := true;
      end;
    done;
    !retval;
  end

(* If the type is has a combolock annotation, replace with combolock type *)
(* NOTE: This must walk complex types searching for the COMBOLOCk annotation
 * I handle structs now (for embedded structures within a structure). If we 
 * encounter more complex cases, those must be handled here as well *)
let rec search_and_replace_combolock_typ (t: typ) : typ =
  begin
    (match t with
       | TComp(cinfo, attr) ->
           for j = 0 to (List.length cinfo.cfields) - 1 do
             let jth = (List.nth cinfo.cfields j) in 
             jth.ftype <- (search_and_replace_combolock_typ jth.ftype);
           done;
           TComp(cinfo, attr);
       | _ ->
           if (Marshannot_dri.is_combolock t) = true
           then struct_combolock_typ
           else t;
    );
  end

(* Walk list of globals: variable and structure definitions searching and
 * replacing COMBOLOCK annotations with appropriate structure definitions *)
(* We currently only examine variable and structure declarations *)
let search_and_replace_combolock (gl: global list) : global list = 
  begin
    let retval = ref [] in
    for i = 0 to (List.length gl) - 1 do
      let ith = (List.nth gl i) in
      (match ith with
         | GVarDecl(vinfo, loc) ->
             vinfo.vtype <- (search_and_replace_combolock_typ vinfo.vtype);
             retval := (List.append !retval [GVarDecl(vinfo, loc)]);
         | GCompTag(cinfo, loc) ->
             for j = 0 to (List.length cinfo.cfields) - 1 do
               let jth = (List.nth cinfo.cfields j) in
               jth.ftype <- (search_and_replace_combolock_typ jth.ftype);    
             done;
             retval := (List.append !retval [GCompTag(cinfo, loc)]);
         | _ -> retval := (List.append !retval [ith]);
      );
    done;
    !retval;
  end

(* do_lock_xform_before_split: 
 * The main function. Here, we use the user-supplied annotations to modify 
 * the spinlock data structure into a combo_lock data structure, and place 
 * the declaration of the data structure at an appropriate location *)
let do_lockxform_before_split (f: file) (dowhat: string) : unit = 
  begin
    (* 1. Insert combolock declaration *)
    f.globals <- (insert_combolock_declaration f.globals);

    (* 2. All variables that have been marked with the COMBOLOCK user 
     * annotation must be converted into the combolock type *)
    f.globals <- (search_and_replace_combolock f.globals);
  end

(*---------------------------------------------------------------------------*)
