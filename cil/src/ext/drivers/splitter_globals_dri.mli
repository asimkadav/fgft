(* Exported globals *)
val feature_mode : string ref
val do_symdriver_test : bool ref

val nooks_ptrlookup_fn : string ref
val nooks_arrayalloc_fn : string ref
val nooks_registerfn_fn : string ref
val nooks_storeoffset_fn : string ref

val struct_marshret_buf : string
val struct_marshret_len : string
val struct_reqargs_data : string
val struct_reqargs_length : string
val struct_marshret_compinfo : Cil.compinfo

val marshret_buf : Cil.fieldinfo
val marshret_len : Cil.fieldinfo
val struct_reqargs_compinfo : Cil.compinfo
val struct_reqargs_typ : Cil.typ
val reqargs_funcid : Cil.fieldinfo
val reqargs_data : Cil.fieldinfo
val reqargs_length : Cil.fieldinfo

val threshold_stackdepth : int ref
val zero64 : int64
val zero64Uexp : Cil.exp
val zero64Sexp : Cil.exp
val minusone_exp : Cil.exp

val marshwrap_prefix : string
val stub_prefix : string
val stubmarsh_prefix : string
val stubdemarsh_prefix : string


