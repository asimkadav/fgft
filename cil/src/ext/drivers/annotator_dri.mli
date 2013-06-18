(*===========================================================================*)
(*
 * CIL Module for annotating functions in the driver as belonging to user-space
 * or kernel space.
 *
 * Vinod Ganapathy <vg@cs.wisc.edu>, September 25, 2006.
*)
(*===========================================================================*)

val do_annotation: Cil.file -> string -> string -> string -> string -> unit
