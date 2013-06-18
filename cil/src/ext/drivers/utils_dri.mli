(*===========================================================================*)
(*
 * Device-driver analysis: This file contains general utilities.
 *
 * Vinod Ganapathy <vg@cs.wisc.edu>, August 1, 2006.
*)
(*===========================================================================*)

(** Useful string function *)
val strip_whitespace: string -> string

(** Failures *)
val warning: string list -> unit
val fatal: string list -> 'a

(** Dereference a type *)
val deref_ptr_typ: Cil.typ -> Cil.typ

(** Functions to manipulate lists *)
val add_to_list: 'a -> 'a list -> 'a list
val remove_repeats: 'a list -> 'a list

(** Functions to manipulate hash tables *)
val add_if: ('a,'b) Hashtbl.t -> 'a -> 'b -> unit 
val add_if_binding: ('a,'b) Hashtbl.t -> 'a -> 'b -> unit
val list_keys: ('a,'b) Hashtbl.t -> 'a list
val list_bindings: ('a,'b) Hashtbl.t -> 'b list
val list_keybindings: ('a,'b) Hashtbl.t -> ('a * 'b) list
val are_keysets_same: (string,'a) Hashtbl.t -> (string,'b) Hashtbl.t -> bool 
val copy_htab_in_place: ('a,'b) Hashtbl.t -> ('a,'b) Hashtbl.t -> unit

(** Misc utilities *)
val itoa: int -> string
val get_attribs: Cil.typ -> Cil.attributes
val strip_typ_attribs: Cil.typ -> Cil.typ
val is_same_typ: Cil.typ -> Cil.typ -> bool
val tcomp_compinfo: Cil.typ -> Cil.compinfo option
val isCompoundType: Cil.typ -> bool
val get_fieldinfo: Cil.compinfo -> string -> Cil.fieldinfo 
val fieldinfo_name: Cil.fieldinfo -> string
val makevarname: string -> string

(** Prettyprint something *)
val typ_tostring: Cil.typ -> string
val instr_tostring: Cil.instr -> string
val typ_tostring_noattr: Cil.typ -> string
val typ_tostring_attronly: Cil.typ -> string
val lval_tostring: Cil.lval -> string
val exp_tostring: Cil.exp -> string
val stmtlist_tostring: Cil.stmt list -> string
val get_params_str_fundec: Cil.fundec -> string

(** Lval utilities *)
val expify_fundec: Cil.fundec -> Cil.exp
val expify_lval: Cil.lval -> Cil.exp
val lvalify_varinfo: Cil.varinfo -> Cil.lval
val lvalify_varinfo_field: Cil.varinfo -> Cil.fieldinfo -> Cil.lval
val lvalify_wrapper: Cil.varinfo -> Cil.fieldinfo option -> Cil.lval
val add_field_to_lval: Cil.lval -> Cil.fieldinfo -> Cil.lval
val add_field_to_lval_str: Cil.lval -> string -> Cil.lval
val add_field_to_lval_wrapper: Cil.lval -> Cil.fieldinfo option -> Cil.lval

(** Warnings *)
val addwarn: string list -> unit
val flushwarn: unit -> unit

(** Infomsgs *)
val announcemsg: string list -> unit
val infomsg: string list -> unit
