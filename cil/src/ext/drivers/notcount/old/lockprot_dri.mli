(*===========================================================================*)
(*
 * CIL Module to implement the new locking protocol. Some transformations in
 * this module need to be implemented BEFORE splitting, and some need to be
 *
 * Vinod Ganapathy <vg@cs.wisc.edu>, 19 June 2007.
*)
(*===========================================================================*)

val do_lockxform_before_split : Cil.file -> string -> unit
val do_lockxform_after_split : Cil.file -> string -> unit
