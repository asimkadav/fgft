(*===========================================================================*)
(*
 * CIL Module to determine which arguments must be marshalled to user-space
 * (or symmetrically, kernel space). 
 *
 * Vinod Ganapathy <vg@cs.wisc.edu>, October 18, 2006.
*)
(*===========================================================================*)

val do_marshal_analysis: Cil.file -> string -> unit
val get_flattened_ptg: string -> bool -> (string, (Cil.typ * string * string)) Hashtbl.t
val get_globals_accessed : string -> Cil.varinfo list
val get_resolved_fields: unit -> (string, (Cil.typ * Cil.fieldinfo * Cil.typ)) Hashtbl.t
val get_resolved_formals: unit -> (string, (int * Cil.varinfo * Cil.typ)) Hashtbl.t  
val typ_field_tostring: (Cil.typ * Cil.fieldinfo) -> string
val is_field_addr_taken: Cil.fieldinfo -> bool
val do_be_conservative: bool ref
val do_simple_m_dm: bool ref
val do_void_ptr: bool ref
