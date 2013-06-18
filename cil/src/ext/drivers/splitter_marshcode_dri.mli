(** Exported functions *)
val initialize_nooks : bool -> unit

val gen_dummy_bodies :
  (string, bool) Hashtbl.t ->
  Cil.fundec list ->
  Cil.varinfo list ->
  Cil.global list

val gen_registerfn : 
  Cil.fundec list ->
  Cil.varinfo list ->
  (string, bool) Hashtbl.t ->
  Cil.fundec

val gen_registerglob :
  Cil.lval list ->
  Cil.fundec

val gen_basic_call :
  string ->
  Cil.stmt

val gen_marshbuf_free :
  string ->
  Cil.lval ->
  Cil.stmt

val gen_disp_kern_allowed :
  Cil.fundec ->
  Cil.block ->
  Cil.block ->
  Cil.stmt list
    
val gen_disp_kern_blocker :
  Cil.fundec ->
  Cil.typ ->
  Cil.lval option ->
  Cil.stmt list ->
  Cil.stmt list ->
  Cil.stmt list
    
val gen_call_empty :
  string ->
  Cil.stmt list

val gen_disploop : 
  bool ->
  Cil.fundec list ->
  Cil.fundec

val gen_empty_stub :
  string ->
  Cil.typ ->
  int ->
  Cil.fundec

(*****************************************************************************)
(* Main marshaling functions                                                 *)
(*****************************************************************************)
val gen_unified_m_dm:
  Cil.fundec ->
  Cil.lval ->
  Cil.fieldinfo option ->
  Cil.typ ->
  Cil.varinfo ->
  Cil.varinfo ->
  Cil.fundec ->
  (string, (Cil.typ * string * string)) Hashtbl.t ->
  (string, (Cil.typ * Cil.fieldinfo * Cil.typ)) Hashtbl.t ->
  (string, (int * Cil.varinfo * Cil.typ)) Hashtbl.t ->
  int ->
  bool ->
  bool ->
  Cil.stmt list

val unified_dfs:
  Cil.lval ->
  Cil.typ ->
  Cil.fundec ->
  Cil.fundec ->
  (string, (Cil.typ * string * string)) Hashtbl.t ->
  (string, (Cil.typ * Cil.fieldinfo * Cil.typ)) Hashtbl.t ->
  (string, (int * Cil.varinfo * Cil.typ)) Hashtbl.t ->
  Cil.varinfo ->
  Cil.varinfo ->
  int ->
  bool ->
  bool -> 
  Cil.stmt list
