(*===========================================================================*)
(*
 * Utilities for splitting drivers.
 * Most usage occurs in splitter_dri
 *
 * Matt Renzelmann <mjr@cs.wisc.edu>, April 30, 2010
*)
(*===========================================================================*)
val strip_stub_prefix : string -> string
val strip_stub_prefix_pair : string -> (string * string)
val strip_marshwrap_prefix : string -> string
val get_disp_userkern_fundec : string -> (Cil.fundec * Cil.varinfo * Cil.varinfo)
val obtain_varinfo_from_lval : Cil.lval -> Cil.varinfo
val expify_varinfos : Cil.varinfo list -> Cil.exp list
val lvalify_varinfos : Cil.varinfo list -> Cil.lval list
val expify_lvals : Cil.lval list -> Cil.exp list
val ensure_formals_have_names : Cil.fundec -> unit
val contains_marshwrap_prefix : string -> bool
val kernfn_already_accounted_for : Cil.fundec list -> Cil.varinfo -> bool

val populate_entry_points:
  string ->
  (string, string) Hashtbl.t ->
  unit

val populate_nonstubbed_functions:
  (string, bool) Hashtbl.t ->
  (string, bool) Hashtbl.t ->
  (string, bool) Hashtbl.t ->
                 int ->
  unit

val populate_funcs_with_no_fundecs :
  (string, Cil.varinfo) Hashtbl.t ->
  Cil.varinfo list ->
  unit

val is_interface_function :
  (string, bool) Hashtbl.t ->
  string -> bool

val populate_interface_functions:
  Cil.file ->
  (string, string) Hashtbl.t ->
  (string, Cil.varinfo) Hashtbl.t ->
  (string, bool) Hashtbl.t ->
  (string, bool) Hashtbl.t ->
  unit

(** Annotations *)
val populate_annotations:
  (string, string) Hashtbl.t ->
  string ->
  unit

(** Function ID mapping *)
val populate_function_id_mapping:
  (string, string) Hashtbl.t ->
  (Cil.fundec list) ->
  (Cil.varinfo list) ->
  (Cil.varinfo list) ->
  unit

val populate_execution_mode_map :
  (string, string) Hashtbl.t ->
  (string, bool) Hashtbl.t ->
  unit

val is_comment:
  string ->
  bool

val create_array_typ : Cil.typ -> int -> Cil.typ
val get_function_id : string -> int
val get_lval_id : Cil.lval -> int

(** Variables *)
val get_temporary_variable :
  Cil.fundec ->
  Cil.lval option ->
  Cil.typ ->
  string ->
  Cil.varinfo

val get_formal_variable :
  Cil.fundec ->
  string ->
  Cil.varinfo

val remove_local_variable :
  Cil.fundec ->
  Cil.typ ->
  string ->
  unit

val get_local_variable :
  Cil.fundec ->
  Cil.typ ->
  string ->
  Cil.varinfo

val get_global_variable :
  Cil.typ ->
  string ->
  Cil.varinfo

(* Splitting *)
val should_split : (string, bool) Hashtbl.t -> string -> bool
val resolve_opaque_formal :
  Cil.varinfo ->
  Cil.fundec ->
  (string, (int * Cil.varinfo * Cil.typ)) Hashtbl.t ->
  Cil.typ list
val resolve_opaque_field :
  Cil.fundec ->
  (string, (Cil.typ * Cil.fieldinfo * Cil.typ)) Hashtbl.t ->
  Cil.typ ->
  Cil.fieldinfo ->
  Cil.typ list
val get_fields_accessed :
  (Cil.typ * string * string) list ->
  (Cil.typ * string * string) list
val underef_lval : Cil.lval -> Cil.lval option

(* Global manipulation *)
val get_globals_to_register : unit -> Cil.lval list
val populate_globals_to_register : Cil.global list -> Cil.varinfo list -> unit
val make_globals_nonextern : Cil.global list -> Cil.global list
val delete_nonstubs :
  Cil.global list ->
  (string, bool) Hashtbl.t ->
  (string, bool) Hashtbl.t ->
  Cil.global list

val make_params : Cil.fundec -> Cil.varinfo list
val gen_function_id_map : unit -> Cil.global list
