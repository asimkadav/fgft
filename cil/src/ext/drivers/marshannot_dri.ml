(*===========================================================================*)
(*
 * Framework to express marshaling annotations.
 *
 * Vinod Ganapathy <vg@cs.wisc.edu>, March 12, 2007.
 *)
(*===========================================================================*)

open Cil
open Str
open Utils_dri

(* Look at the Wiki for updated information on the different marshaling
   annotation that are available. *)

(*---------------------------------------------------------------------------*)

(* These denote null terminated char strings *)
let nullterm_attrib = "nullterm"

(* These denote pointer data types PTR(t) that point to multiple instances
 * of T objects (e.g., because they are used as part of pointer arithmetic)
 *)
let arith_attrib = "arith"
let blob_attrib = "blob"

(* These denote recursive data structures. The field of the data structure
 * that induces recursion must be marked with this declaration in the structure
 * definition. We have a separate annotation for linked lists. Don't use these 
 * for linked lists. *)
let recursive_attrib = "recursive"
let linkedlist_attrib = "linkedlist"

(* These are opaques for which we manually supply the resolution *)
let opaque_attrib = "opaque"

(* These denote locks that must be modified to be combolocks *)
let combolock_attrib = "combolock"

(* These denote modifications to data structures *)
let modifies_function_prefix = "MICRODRIVERS__MODIF_"
let modifies_attrib = "modifies"

(* This denotes a pointer that is an offset - i.e., pointer to the middle 
 * of an array *)
let offsetptr_attrib = "offsetptr"

(* This denotes a rangestore pointer: i.e., it is an array and we must
 * store all the elements of the array in the table *)
let rangestore_attrib = "rangestore"

(* This denotes unsigned longs that are pointers *)
let ispointer_attrib = "ispointer"

(* This denotes fields for which we must store offsets *)
let storeoffset_attrib = "storeoffset"

(* This denotes the type of the container *)
let conttype_attrib = "conttype"
let contfield_attrib = "contfield"

(* Use this on pointers, e.g. parameters to functions that are pointers.
   This denotes a mostly-arbitrary function that returns a pointer.
   The pointer it returns is marshaled along with the actual pointer.
   The function takes as a parameter the pointer being marshaled,
   and returns a pointer of the designated type
*)
let extraptr_attrib = "extraptr"

(* This denotes the noderef annotation.  This is not unique to the splitter,
   indeed, driver developers are supposed to be using it anyway *)
(* The idea is to have these semantics when applied to each of these variables:
   - integer:  don't marshal it
   - ptr:  marshal the pointer but don't dereference it
   - structure:  don't marshal it
   - array:  don't marshal it
*)
let address_space_attrib = "address_space"
let noderef_attrib = "noderef"

(* This denotes the fixedarray annotation *)
let fixed_array_attrib = "fixedarray"

(* A list of all our type attributes *)
let all_annotations = 
  nullterm_attrib::
    arith_attrib::
    blob_attrib::
    recursive_attrib::
    linkedlist_attrib::
    opaque_attrib::
    combolock_attrib::
    offsetptr_attrib::
    rangestore_attrib::
    ispointer_attrib::
    storeoffset_attrib::
    conttype_attrib::
    contfield_attrib::
    noderef_attrib::
    fixed_array_attrib::
    []

(** search_for_attrib: Generic search for an attribute string within the
 * set of attributes of a particular data type *)
let find_attrib (t: typ) (search: string) : attribute option = 
  begin
    let test_obsolete current =
      if (String.compare search "exp") = 0 then
        fatal ["exp annotation obsolete"];
      if (String.compare search "expfld") = 0 then
        fatal ["expfld annotation obsolete"];
    in
    List.iter test_obsolete (typeAttrs t);
    let predicate current =
      begin
        let Attr(str,apl) = current in
        if (String.compare search str) = 0 then
          true
        else
          false
      end
    in
    try
      Some (List.find predicate (typeAttrs t));
    with Not_found ->
      None
  end

let attrib_exists (t: typ) (search: string) : bool = 
  let retval = find_attrib t search in
  match retval with | Some(r) -> true | None -> false

(*---------------------------------------------------------------------------*)
(* Basic queries *)

(** Is this a null-terminated-buffer type? *)
(* For a "char *varname", annotations must be provided as: 
 *  "char * __attribute__((nullterm)) varname". 
 *  The following are wrong ways to provide annotations:
 *  NOT "__attribute__((nullterm)) char *varname"
 *  NOT "char __attribute__((nullterm)) *varname" *)
let is_nullterm (t: typ) : bool = (attrib_exists t nullterm_attrib)  

(** Is this a array pointer? Array pointers are those pointers of type "PTR *"
 * that are involved in pointer arithmentic. Array pointers can have one of
 * three kinds of attributes: arith or blob *)
let is_arith (t: typ) : bool = fatal ["Do not use the arith annotation.  Support is gone"]

let is_blob (t: typ) : bool = (attrib_exists t blob_attrib)

(** Is this data type part of a recursive data structure access? 
 * Use this to mark fields that cause recursive access to data 
 * structures. *)
let is_linkedlist (t: typ) : bool = fatal ["Do not use is_linkedlist."]

(** Is this recurse? *)
let is_recursive (t: typ) : bool = (attrib_exists t recursive_attrib) 

(** Is this a const type? *)
let is_const (t: typ) : bool = (attrib_exists t "const")

(** Is this a resolved opaque? *)
let is_opaque (t: typ) : bool = (attrib_exists t opaque_attrib)

(** Is this a combolock? *)
let is_combolock (t: typ) : bool = (attrib_exists t combolock_attrib)

(** Is this a const varinfo? *)
let is_const_varinfo (v: varinfo) : bool = (is_const v.vtype)

(** Is this a rangestore pointer? *)
let is_rangestore (t: typ) : bool = (attrib_exists t rangestore_attrib) 

(** Is this an offsetptr pointer *)
let is_offsetptr (t: typ) : bool = (attrib_exists t offsetptr_attrib)

(** Is this a pointer? *)
let is_ispointer (t: typ) : bool = (attrib_exists t ispointer_attrib)

(** Is this a storeoffset? *)
let is_storeoffset (t: typ) : bool = (attrib_exists t storeoffset_attrib)

(** Is this decorated with a container pointer? *)
let is_container (t: typ) : bool = 
  (attrib_exists t conttype_attrib) || 
    (attrib_exists t contfield_attrib)

(** Is this an extra pointer annotation? *)
let is_extraptr (t: typ) : bool = (attrib_exists t extraptr_attrib)

(** Is this I/O memory/memory we should not marshal? *)
let is_noderef (t: typ) : bool = (attrib_exists t noderef_attrib)

(** Returns:
    true if we have a match (noderef / address_space matches)
    false if no match
*)
let rec is_fancy_noderef (t: typ) (address_space_num: int) : bool =
  begin
    let derefed_ptr_result =
      match t with 
        | TPtr(t_deref, _) ->
            is_fancy_noderef t_deref address_space_num;
        | _ ->
            false
    in
    let result_final =
      let result_basic = is_noderef t in
      let attrib = find_attrib t address_space_attrib in
      let (result_has_address_space, result_address_space_match) =
        match attrib with
          | Some(Attr(string, params)) -> 
              if List.length params = 1 then
                let address_space_param = List.nth params 0 in
                match address_space_param with
                  | AInt(i) ->
                      if (i = address_space_num) then
                        (true, true)
                      else
                        (true, false)
                  | _ -> (true, false)
              else
                (true, false)
          | None ->
              (false, false)
      in
      if result_basic && not result_has_address_space then
        fatal ["Failure:  noderef annotation not coupled with address_space: "; typ_tostring t]
      else if result_basic && result_address_space_match then
        true (* Noderef + address_space matches.  Good *)
      else if result_basic && result_has_address_space && not result_address_space_match then
        false (* Noderef + has address_space + but does not match *)
      else if not result_basic then
        false (* Does not have noderef *)
      else
        fatal ["Logical error in is_fancy_noderef function / marshannot_dri.ml"]
    in
    result_final || derefed_ptr_result
  end
    
(* # define __user         __attribute__((noderef, address_space(1))) *)
let is_usermem (t: typ) : bool = is_fancy_noderef t 1

(* # define __iomem        __attribute__((noderef, address_space(2))) *)
let is_iomem (t: typ) : bool = is_fancy_noderef t 2 || is_fancy_noderef t 1 ||
is_fancy_noderef t 3  || is_fancy_noderef t 4 
  
let is_usermem2 (t:typ) : bool = is_fancy_noderef t 3

let is_untouched (t:typ) : bool = is_fancy_noderef t 4


(** Is this a fixed array? *)
let is_fixedarray (t: typ) : bool = (attrib_exists t fixed_array_attrib)

(* Check if stub_prefix is indeed a prefix of the given string *)
let modifies_prefix_occurs (s: string) : bool =
  begin
    let modifies_prefix_regexp = (Str.regexp modifies_function_prefix) in
    (Str.string_match modifies_prefix_regexp s 0);
  end

(* Strip the modifies_prefix from an input string *)
let strip_modifies_prefix (s: string) : string =
  begin
    let modifies_prefix_len = (String.length modifies_function_prefix) in 
    let s_len = (String.length s) in 
    let modifies_prefix_regexp = (Str.regexp modifies_function_prefix) in
    if (Str.string_match modifies_prefix_regexp s 0) = false
    then fatal["Cannot strip modifies_prefix in "; s];
    try  
      (String.sub s modifies_prefix_len (s_len - modifies_prefix_len));
    with Invalid_argument(_) ->
      fatal["strip_modifies_prefix"];
  end

(* Is the input function a MODIFANNOT function *) 
let is_modifannot_function (fname: string) : bool = 
  begin
    (modifies_prefix_occurs fname);
  end


(*---------------------------------------------------------------------------*)
(* More complex queries *)

let attr_extra_ptr (t: typ) : (string * typ) =
  begin
    let attribs = typeAttrs t in
    if (List.length attribs) <> 1 then
      fatal ["More than one attribute in attr_extra_ptr"];

    let Attr(str, apl) = List.nth attribs 0 in
    if (String.compare str extraptr_attrib) = 0 then
      if (List.length apl) <> 2 then
        fatal ["extraptr attribute provided with wrong number of attributes"]
      else
        let desttype_attr = List.nth apl 0 in
        let function_attr = List.nth apl 1 in
        let desttype_typ =
          match desttype_attr with
            | ASizeOf(t) -> t
            | _ -> fatal ["attr_extra_ptr wrong first parameter"]
        in
        let function_string =
          match function_attr with
            | AStr(s) -> s
            | _ -> fatal ["attr_extra_ptr wrong second parameter"]
        in
        (function_string, desttype_typ)
    else
      fatal ["Calling attr_extra_ptr without the right extraptr attribute!"];
  end

(** Get the container type of a container pointer *)
let typeof_container (t: typ) : typ = 
  begin
    let retval = ref voidPtrType in
    for i = 0 to (List.length (typeAttrs t)) - 1 do
      let Attr(str,apl) = (List.nth (typeAttrs t) i) in
      if (String.compare str conttype_attrib) = 0 then
        for j = 0 to (List.length apl) - 1 do
          let jth = (List.nth apl j) in
          (match jth with
             | ASizeOf(restype) -> retval := restype;
             | _ -> (fatal ["Non sizeof in conttype annotation"]);
          );
        done;
    done;
    (infomsg ["Resolved conttyp type with annotation to"; (typ_tostring !retval)]);
    !retval;
  end

let fieldname_container (t: typ) : string = 
  begin
    let retval = ref "" in
    for i = 0 to (List.length (typeAttrs t)) - 1 do
      let Attr(str,apl) = (List.nth (typeAttrs t) i) in
      if (String.compare str contfield_attrib) = 0 then
        for j = 0 to (List.length apl) - 1 do
          let jth = (List.nth apl j) in
          (match jth with
             | AStr(s) ->
                 retval := s;
             | _ ->
                 fatal ["Field info not supplied as a string"];
          );
        done;
    done;
    (infomsg ["Field fetched was"; !retval]);
    !retval;
  end


(** Get the field of a container pointer *)
let fieldof_container (t: typ) : fieldinfo option = 
  begin
    if (is_container t) = false then
      fatal ["Failed to extract container information"];
    let conttyp = typeof_container t in
    let contfieldnm = fieldname_container t in
    let conttyp_compinfo = tcomp_compinfo conttyp in
    match conttyp_compinfo with
      | Some(cinfo) ->
          Some(get_fieldinfo cinfo contfieldnm);
      | None ->
          fatal ["fieldof_container: Not a struct type"];
  end


(** Return the attrparam of an opaque type. We use the "sizeof" to mention the
 * resolved type of the opaque. Return a fatal error if we cannot find a
 * resolved type of an annotated opaque variable. The annotation must be 
 * provided as: __attribute__((opaque(sizeof(TYPE)))) *)
let resolve_opaque_with_annot (t: typ) : typ = 
  begin
    let retval = ref voidPtrType in
    for i = 0 to (List.length (typeAttrs t)) - 1 do
      let Attr(str,apl) = (List.nth (typeAttrs t) i) in
      if (String.compare str opaque_attrib) = 0 then
        for j = 0 to (List.length apl) - 1 do
          let jth = (List.nth apl j) in
          (match jth with
             | ASizeOf(restype) -> retval := restype;
             | _ -> (fatal ["Non sizeof in opaque annotation"]);
          );
        done;
    done;
    (infomsg ["Resolved opaque type with annotation to"; (typ_tostring !retval)]);
    !retval;
  end


(** Return the attrparam of an ispointer type. Like resolve_opaque_with_annot. *)
let typeof_ptrtarget (t: typ) : typ = 
  begin
    let retval = ref voidPtrType in
    for i = 0 to (List.length (typeAttrs t)) - 1 do
      let Attr(str,apl) = (List.nth (typeAttrs t) i) in
      if (String.compare str ispointer_attrib) = 0 then
        for j = 0 to (List.length apl) - 1 do
          let jth = (List.nth apl j) in
          (match jth with
             | ASizeOf(restype) -> retval := restype;
             | _ -> (fatal ["Non sizeof in ispointer annotation"]);
          );
        done;
    done;
    (infomsg ["Resolved ispointer type with annotation to"; (typ_tostring !retval)]);
    !retval;
  end

(**
   The idea is to convert a string into an expression according to a few simple rules.
   If the string represents a variable in the current structure (ctxt), then we just use that
   variable.  The ctxt parameter is optional, in which case the search takes place only
   in the list of provided variables.

   If the string represents a variable in the list of variables provided (vlist), then we
   use that instead.  We only search this list if the provided ctxt is None.

   Note that there is a limitation here:  if the context and the variable list each
   contain a variable of the name "str", then we will always go with the one from the
   context rather than vlist.  Likewise, if a context is provided but the variable
   is not found, then we have an error.
*)
let string2exp (str: string) (vlist: varinfo list) (ctxt: lval option) : exp =
  match ctxt with
    | Some(lv) ->
        (* Here we have a context and so can do more *)
        let param = add_field_to_lval_str lv str in
        expify_lval param;
    | None ->
        (* No context, just pick it from the list *)
        begin
          let retval = ref (integer 0) in
          let found = ref false in
          for i = 0 to (List.length vlist) - 1 do
            let ith = (List.nth vlist i) in 
            if (String.compare str ith.vname) = 0 then
              begin
                retval := (expify_lval (lvalify_varinfo ith));
                found := true;
              end;
          done;
          if (!found = false) then
            fatal ["attrparam2exp_blob: No variable"; str; "in context"];
          !retval;
        end;;

(** Extract the expression corresponding to the length of the array from the
 * "blob" Attrparam. *)
let rec attrparam2exp_blob (ap: attrparam) (vlist: varinfo list) (ctxt: lval option) : exp = 
  match ap with
    | AInt(i) -> integer i;
    | AStr(s) -> 
        string2exp s vlist ctxt;
    | AUnOp(uop, ap1) -> 
        let ap1exp = attrparam2exp_blob ap1 vlist ctxt in
        UnOp(uop, ap1exp, (typeOf ap1exp));
    | ABinOp(bop, ap1, ap2) ->
        let ap1exp = attrparam2exp_blob ap1 vlist ctxt in
        let ap2exp = attrparam2exp_blob ap2 vlist ctxt in
        BinOp(bop, ap1exp, ap2exp, (typeOf ap1exp));
    | _ -> 
        (* Main difficulty here: how do I convert an arbitrary string 
         * into a local variable that stores the length of the array? *)
        fatal ["attrparam2exp_noctxt_blob: Currently unimplemented"];;

let remove_obfuscation (orig_str: string) : string =
  begin
    let orig_length = String.length orig_str in
    let obfuscation_str = "Obfuscate_" in
    let obfuscation_length = String.length obfuscation_str in
    let obfuscation_prefix_regexp = Str.regexp ("^" ^ obfuscation_str ^ ".*") in
    if (Str.string_match obfuscation_prefix_regexp orig_str 0) then
      begin
        let len = orig_length - obfuscation_length in
        let pos = obfuscation_length in
        try
          let substring = String.sub orig_str pos len in
          substring;
        with Invalid_argument(_) ->
          fatal["remove_obfuscation failure 1"];
      end
    else
      fatal["remove_obfuscation failure 2"];
  end
    
(** Used for blob attributes.  The point is to generate a statement
   assigning the length of the array to the variable specified.
   We need to deal with the assignment in this function (rather than
   just returning an expression) to handle arbitrary function calls
   provided by the ACons attribute.  ACons looks like a function call:
   __attribute__((blob(test("param1"))))
*)
let rec attrparam2stmt_blob (ap: attrparam) (vlist: varinfo list) (ctxt: lval option) (bdry_var: varinfo) : stmt =
  match ap with
    | ACons (fn_name, attr_list) ->
        Printf.fprintf stderr "blob annotation: %s\n" fn_name;
        let deobfuscated_name = remove_obfuscation fn_name in
        let convert_attr_to_str a =
          match a with
            | AStr(s) -> s
            | _ -> fatal ["attrparam2stmt_noctxt_blob: ACons failure.  Strings only!\n"];
        in
        (*let convert_str_to_exp str =
          let vi = makeVarinfo false str (TInt (ILong, [])) in
            expify_lval (lvalify_varinfo vi)
          in
        *)
        let convert_str_to_exp str =
          string2exp str vlist ctxt
        in
        let strings = List.map convert_attr_to_str attr_list in
        let exps = List.map convert_str_to_exp strings in
        let lengthfuncname = deobfuscated_name in
        let lengthfundec = emptyFunction lengthfuncname in
        let lengthfunc = expify_fundec lengthfundec in
        let call_lengthfunc = Call(Some (lvalify_varinfo bdry_var), lengthfunc, exps, locUnknown) in
        let stmt_call_lengthfunc = (mkStmt (Instr [call_lengthfunc])) in
        stmt_call_lengthfunc
    | _ -> 
        let exp_len = attrparam2exp_blob ap vlist ctxt in
        let init_bdry = Set(lvalify_varinfo bdry_var, exp_len, locUnknown) in
        let stmt_init_bdry = mkStmt (Instr [init_bdry]) in
        stmt_init_bdry;;
        
(** Extract the expression corresponding to the length of the array from the
 * "blob" AttrParam *)
(*
let rec attrparam2exp_ctxt_blob (ctxt: lval) (ap: attrparam) : exp =
  begin
    match ap with
      | AInt(i) -> (integer i);
      | AStr(s) -> 
          let param = add_field_to_lval_str ctxt s in
          expify_lval param;
      | ABinOp(bop, ap1, ap2) ->
          let ap1exp = attrparam2exp_ctxt_blob ctxt ap1 in
          let ap2exp = attrparam2exp_ctxt_blob ctxt ap2 in
          BinOp(bop, ap1exp, ap2exp, (typeOf ap1exp));
      | _ ->
          fatal ["Case not currently supported in attrparam2exp_ctxt_blob"];
  end
*)
(* Extract the attrparam supplied with a blob *)
let extract_blob_attrparam (t: typ) : attrparam = 
  begin
    let dummyattr = AInt(-1) in 
    let retval = ref dummyattr in
    for i = 0 to (List.length (typeAttrs t)) - 1 do  
      let Attr(str, apl) = (List.nth (typeAttrs t) i) in
      if (String.compare str blob_attrib) = 0 then begin
        if (List.length apl) <> 1 then 
          (fatal ["extract_blob_attrparam: <>1 attrparam passed to exp"]);
        retval := (List.nth apl 0);
      end;
    done;
    !retval;
  end

(** Used only in the XDR interface generation code.
    Gets us the length of a fixed length array.
    XDR does not support variable length arrays.
*)
let get_fixed_array_length (t: typ) : exp =
  let ap = extract_blob_attrparam t in
  attrparam2exp_blob ap [] None;;

(** Get me the length of the array.  Uses only the blob annotation.
 * Return value is the assignment containing
 * the array length, if we could find it. None otherwise. This also takes an
 * optional context as input, with which to specialize the length expression 
 * of the array. This also takes a list of varinfos, which may potentially
 * indicate contain variables that store lengths of arrays *)
let get_array_length (t : typ) 
    (ctxt : lval option) 
    (vlist : varinfo list) 
    (bdry_var : varinfo) : stmt =
  begin
    (* Check for obvious errors *)
    (*if (is_array t) = false then fatal ["Non array in get_array_length"];*)

    (* For ordinary arith pointers, we do not have any information on length *)
    (*if (is_arith t) = true then retval := None;*)

    (* For blob pointers, extract the length. *)
    if (is_blob t) = true then
      begin
        (* We do not have to put it in the context of an input lval here *)
        let ap = extract_blob_attrparam t in
        attrparam2stmt_blob ap vlist ctxt bdry_var;
      end
    else
      fatal ["get_array_length called with non-blob argument"; typ_tostring t];
  end

(** is_recursive_access: Take a list of triples (parstruct, field, access), 
 * denoting the type of a parent structure, a field of that structure, and
 * the way that field is accessed, and determine if any of the fields 
 * accessed has a recurse annotation on it *)
let is_recursive_access (fldlist: (typ * string * string) list) : bool = 
  begin
    let retval = ref false in
    for i = 0 to (List.length fldlist) - 1 do  
      let (ithtyp, ithfield, ithaccess) = (List.nth fldlist i) in
      let ithcomp = (tcomp_compinfo ithtyp) in
      (match ithcomp with
         | Some(ithcompinfo) ->
             begin
               let ithfieldinfo = (get_fieldinfo ithcompinfo ithfield) in
               let ithfieldtyp = ithfieldinfo.ftype in
               if (is_recursive ithfieldtyp) then retval := true;
               (* Keep dereferencing the current type and check for the RECURSIVE
                * keyword too. We need this because the keyword typically appears
                * as follows: PTRTYPE * RECURSIVE next *)
               let currtype = ref ithfieldtyp in
               let mustiterate = ref false in
               if (isPointerType !currtype) then mustiterate := true;
               while (!mustiterate) do
                 (* Deref the pointer, check, and repeat. *)
                 (match !currtype with
                    | TPtr(dtyp,[]) -> currtype := dtyp; mustiterate := true;
                        if (is_recursive !currtype) then retval := true;
                        (* (Printf.fprintf stderr "\tAlso came here\n"); *)
                    | _ -> mustiterate := false; 
                 );
               done;
             end;
         | None -> (fatal ["Non-compinfo passed to is_recursive_access"]);
      );
    done;
    !retval;
  end

(* TODO: This has to be implemented correctly if you want a linkedlist
 * annotation *)
(** is_linkedlist_access: Take a list of triples (parstruct, field, access), 
 * denoting the type of a parent structure, a field of that structure, and
 * the way that field is accessed, and determine if any of the fields 
 * accessed has a linkedlist annotation on it *)
let is_linkedlist_access (fldlist: (typ * string * string) list) : bool = 
  begin
    let retval = ref false in
    for i = 0 to (List.length fldlist) - 1 do  
      let (ithtyp, ithfield, ithaccess) = (List.nth fldlist i) in
      let ithcomp = (tcomp_compinfo ithtyp) in
      (match ithcomp with
         | Some(ithcompinfo) ->
             begin
               let ithfieldinfo = (get_fieldinfo ithcompinfo ithfield) in
               let ithfieldtyp = ithfieldinfo.ftype in
               if (is_linkedlist ithfieldtyp) then retval := true;
               (* Keep dereferencing the current type and check for the RECURSIVE
                * keyword too. We need this because the keyword typically appears
                * as follows: PTRTYPE * RECURSIVE next *)
               let currtype = ref ithfieldtyp in
               let mustiterate = ref false in
               if (isPointerType !currtype) then mustiterate := true;
               while (!mustiterate) do
                 (* Deref the pointer, check, and repeat. *)
                 (match !currtype with
                    | TPtr(dtyp,[]) -> currtype := dtyp; mustiterate := true;
                        if (is_linkedlist !currtype) then retval := true;
                        (* (Printf.fprintf stderr "\tAlso came here\n"); *)
                    | _ -> mustiterate := false; 
                 );
               done;
             end;
         | None -> (fatal ["Non-compinfo passed to is_linkedlist_access"]);
      );
    done;
    if !retval then
      (Printf.fprintf stderr "Linked list access");
    !retval;
  end


(** fields_accessed_is_simple_type:
 * A simple type is defined to be one that will not induce reads/writes of
 * complex data structures (such as structures and enums) and will therefore
 * not contribute to the "fields accessed" information in marshaling.
 *
 * Return true if the input type is a simple type WRT field access information
 * and return false otherwise.
 *)
let rec fields_accessed_is_simple_type (t: typ) : bool = 
  begin
    (match t with
       | TVoid(_) -> true;
       | TInt(_) -> true;
       | TFloat(_) -> true;
       | TPtr(t',_) -> (fields_accessed_is_simple_type t'); 
       | TBuiltin_va_list(_) -> true;
       | TNamed(tinfo,_) -> (fields_accessed_is_simple_type tinfo.ttype);
       | _ -> false;
    );
  end

(* Given the type of a function, we determine whether it needs modif
 * annotations. true => needs modif annotations. false => no. *)
let function_requires_modif_annots (t: typ) : bool =
  begin
    let retval = ref true in
    (match t with
       | TFun(rettyp, argslopt, _, _) ->
           retval := (fields_accessed_is_simple_type rettyp);
           (match argslopt with
              | Some(al) ->
                  for i = 0 to (List.length al) - 1 do
                    let (ithstr, ithtyp, ithattr) = (List.nth al i) in
                    retval := !retval && (fields_accessed_is_simple_type ithtyp);
                  done;
              | None -> ();
           );
           (* If retval is true at this point, it means we only have simple
            * types, so we have to return false. Otherwise, we have to return
            * true *)
           retval := (not !retval); 
       | _ -> 
           (* Not a function type. Should not be possible *)
           (fatal ["I was expecting a function type here."]);
    );
    !retval;
  end

(** get_fields_accessed_from_modifannot:
 * For a function that has been marked with a modifies annotation, get the
 * fields accessed. Note that the fields analysis hashtable (3rd arg) will
 * contain an entry for a function only if it modifies fields. So if we don't
 * find an entry for a modif function, we must check whether it has been 
 * defined before returning the NULL value. *)
let get_fields_accessed_from_modifannot (fnvinfo: varinfo)
    (vf: (varinfo, fundec) Hashtbl.t)
    (ptg: (string, (typ * fieldinfo * string)) Hashtbl.t)
    : (typ * fieldinfo * string) list option = 
  begin
    (* Doesn't really make sense to get the fields accessed for a modifannot
     * function. The modifannot function is provided as an annotation *)
    let fname = fnvinfo.vname in
    let ftyp = fnvinfo.vtype in
    if (is_modifannot_function fname) = true
    then begin
      (fatal ["Passing modifannot function: Doesn't make sense. Stop"]);
    end;
    let modifannot_fname = modifies_function_prefix ^ fname in
    (try
       (ignore (Hashtbl.find ptg modifannot_fname));
       (* We found information on fields accessed. Return it *)
       Some((Hashtbl.find_all ptg modifannot_fname)); 
     with Not_found -> (
       (* We did not find any information on fields accessed *)
       (* Check the type of the function. If we are not accessing any
        * complex types, return empty list and throw a warning. If we 
        * are accessing complex types, and are not provided an annotation,
        * ask for an annotation. If we are provided an annotation, that
        * means no fields were accessed (otherwise the Not_found case 
        * would not have been triggered). So we return empty list.*)
       if (function_requires_modif_annots ftyp) = true
       then begin
         let fvinfolist = (list_keys vf) in
         let funcfound = ref false in
         for i = 0 to (List.length fvinfolist) - 1 do
           let ith = (List.nth fvinfolist i) in
           if (String.compare ith.vname modifannot_fname) = 0
           then funcfound := true;
         done;
         if (!funcfound = true) then begin
           Some([]);
         end else begin
           (*(warning ["Supply modification annotation for"; fname]);*)
           None;
         end
       end else begin
         (* (warning ["I am assuming no annotations are needed for:"; fname]); *)
         Some([]);
       end;
     ));
  end

(** Apply offsetptr constraint: Order field accesses such that offsetptr
 * fields appear after the corresponding heads. If you give me a structure
 * with a cyclic constraint, i.e, x is OFFSETPTR(y) and y is OFFSETPTR(x)
 * this will go into an infinite loop (as is the expected behavior in this
 * case).
 * NOTE: XXX HACK XXX HACK XXX HACK XXX HACK XXX HACK XXX HACK XXX HACK XXX
 * Ideally, express constraints as a graph, and do DFS on the graph.
 * Currently we order as follows:
 *  - Scalars
 *  - Blob-annotated pointers
 *  - Any other pointers
 *  - Offset pointers.
 * NOTE: XXX HACK XXX HACK XXX HACK XXX HACK XXX HACK XXX HACK XXX HACK XXX
 *)
let apply_ordering_constraint 
    (before: (typ * fieldinfo * string) list) : 
    (typ * fieldinfo * string) list = 
  begin
    let scalar_list = ref [] in
    let ptr_blob_list = ref [] in
    let ptr_list = ref [] in
    let ptr_offset_list = ref [] in  
    (* collect all offsets *)
    for i = 0 to (List.length before) - 1 do
      let ith = List.nth before i in
      let (ithtyp, ithfinfo, ithacc) = ith in
      match ithfinfo.ftype with
        | TFloat(_, _) -> fatal ["Floating point in kernel?"]
        | TPtr(t, a) ->
            if is_offsetptr ithfinfo.ftype then ptr_offset_list := !ptr_offset_list @ [ith]
            else if is_blob ithfinfo.ftype then ptr_blob_list := !ptr_blob_list @ [ith]
            else ptr_list := !ptr_list @ [ith]
        | _ -> scalar_list := !scalar_list @ [ith];
    done;
    
    !scalar_list @ !ptr_blob_list @ !ptr_list @ !ptr_offset_list;
  end

(** Take as input a set of field accesses, and impose an ordering on them 
 * based upon annotations. The ordering constraints are currently:
 * 1. If you have an OFFSETPTR(head) pointer and if you have an access
 *    to head as well, then you must put 'head' before the OFFSETPTR(head)
 *    pointer.
 *)
let order_field_accesses (unordered: (typ * string * string) list) :
    (typ * string * string) list = 
  begin
    let unordered_fieldaccesses = ref [] in
    (* 1. Get fieldinfos first *)
    for i = 0 to (List.length unordered) - 1 do
      let (ithtyp, ithfield, ithaccess) = (List.nth unordered i) in
      let ithcomp = (tcomp_compinfo ithtyp) in 
      (match ithcomp with
         | Some(ithcompinfo) ->
             let ithfieldinfo = (get_fieldinfo ithcompinfo ithfield) in
             let newelem = (ithtyp, ithfieldinfo, ithaccess) in
             unordered_fieldaccesses :=
               (List.append !unordered_fieldaccesses [newelem]);
         | None -> (warning ["order_field_accesses: No comp found for"; 
                             (typ_tostring ithtyp)]);
      );
    done;
    
    (* 2. Apply offsetptr constraint *)
    let after_offsetptr_constraint = 
      apply_ordering_constraint !unordered_fieldaccesses in

    (* 3. Convert fieldinfos back into strings *)
    let ordered_fieldaccesses = after_offsetptr_constraint in
    let ordered = ref [] in
    for i = 0 to (List.length ordered_fieldaccesses) - 1 do
      let (ithtyp, ithfieldinfo, ithaccess) = 
        (List.nth ordered_fieldaccesses i) in
      let newelem = (ithtyp, ithfieldinfo.fname, ithaccess) in
      ordered := (List.append !ordered [newelem]);
    done;
    
    (* 4. Sanity check lengths of input and output! *)
    if (List.length unordered) <> (List.length !ordered) 
    then (fatal ["order_field_accesses: in and out of unequal len"]);
    
    !ordered;
  end


(*---------------------------------------------------------------------------*)
(** Cleanup code: Remove all the attributes that we introduced. *)
class cleanup_attributes = object (self)
  inherit nopCilVisitor
    
  method vattr (attr: attribute) : attribute list visitAction =
    begin
      let Attr(attr_name,_) = attr in
      (* Don't remove noderef attributes from result since these should
         be there anyway and they're handy to see *)
      if (List.mem attr_name all_annotations) && (attr_name <> noderef_attrib)
      then
	ChangeTo([])
      else 
	DoChildren;
    end
end


(*---------------------------------------------------------------------------*)
let do_perform_cleanup (f: Cil.file) : unit =
  begin
    let obj = new cleanup_attributes in
    visitCilFileSameGlobals (obj :> cilVisitor) f;
  end
