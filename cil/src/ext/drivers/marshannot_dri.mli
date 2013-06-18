
(*===========================================================================*)
(*
 * Framework to express marshaling annotations.
 *
 * Vinod Ganapathy <vg@cs.wisc.edu>, March 12, 2007.
*)
(*===========================================================================*)

(* Simple queries *)
val modifies_function_prefix: string

val is_nullterm: Cil.typ -> bool
val is_blob: Cil.typ -> bool
val is_recursive: Cil.typ -> bool
val is_linkedlist: Cil.typ -> bool
val is_const: Cil.typ -> bool
val is_opaque: Cil.typ -> bool
val is_combolock: Cil.typ -> bool
val is_const_varinfo: Cil.varinfo -> bool
val is_rangestore: Cil.typ -> bool
val is_offsetptr: Cil.typ -> bool
val is_ispointer: Cil.typ -> bool
val is_storeoffset: Cil.typ -> bool
val is_container: Cil.typ -> bool
val is_extraptr: Cil.typ -> bool
val is_iomem: Cil.typ -> bool
val is_usermem: Cil.typ -> bool
val is_usermem2: Cil.typ -> bool
val is_untouched: Cil.typ -> bool
val is_fixedarray: Cil.typ -> bool

(* Complex queries *)
val attr_extra_ptr: Cil.typ -> string * Cil.typ
val typeof_container: Cil.typ -> Cil.typ
val fieldof_container: Cil.typ -> Cil.fieldinfo option
val resolve_opaque_with_annot: Cil.typ -> Cil.typ
val typeof_ptrtarget: Cil.typ -> Cil.typ 
val get_fixed_array_length: Cil.typ -> Cil.exp
val get_array_length: Cil.typ -> Cil.lval option -> Cil.varinfo list -> Cil.varinfo -> Cil.stmt
val is_recursive_access: (Cil.typ * string * string) list -> bool
val is_linkedlist_access: (Cil.typ * string * string) list -> bool
val get_fields_accessed_from_modifannot: Cil.varinfo ->
  (Cil.varinfo, Cil.fundec) Hashtbl.t ->
  (string, (Cil.typ * Cil.fieldinfo * string)) Hashtbl.t ->
  (Cil.typ * Cil.fieldinfo * string) list option
val order_field_accesses: (Cil.typ * string * string) list ->
  (Cil.typ * string * string) list

val do_perform_cleanup: Cil.file -> unit
