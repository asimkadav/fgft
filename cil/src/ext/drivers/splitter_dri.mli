(*===========================================================================*)
(*
 * CIL Module for splitting device drivers into a user-component and a 
 * kernel-component. This assumes as input a completely colored call-graph.
 *
 * Vinod Ganapathy <vg@cs.wisc.edu>, September 20, 2006.
*)
(*===========================================================================*)

val do_splitting: Cil.file -> string -> string -> string -> string -> unit
