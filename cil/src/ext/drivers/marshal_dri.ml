(*===========================================================================*)
(*
 * CIL Module to determine which arguments must be marshalled to user-space
 * (or symmetrically, kernel space).
 *
 * The analysis described here will be useful not only in the splitting phase,
 * to determine the arguments to be copied, but also in the annotation phase,
 * to determine the cost of putting a certain function in the user-space.
 *
 * Vinod Ganapathy <vg@cs.wisc.edu>, October 26, 2006.
 *)
(*===========================================================================*)

(*
 * POTENTIAL IMPROVEMENTS:
 * - Differentiating between reads and writes to optimize how much is copied
 *   when the function is invoked and when it returns. Currently we merge both
 *   reads and writes and copy the information for both reads and writes
 *   during function invocation as well as return.
 *)

open Cil
open Utils_dri              (* General utilities *)
open Cgcomp_dri             (* Need call-graph for determining topsort order *)
open Marshannot_dri         (* Marshannot: support for marshaling annotations *)

(* Command line parameters, see drivers.ml *)
let do_be_conservative = ref true
let do_simple_m_dm = ref false
let feature_mode = ref ""
let do_void_ptr = ref false

(** Convenient defintions *)

(* Convert a type and fieldinfo into a string *)
let typ_field_tostring (tf: (typ * fieldinfo)) : string =
  begin
    let (t,f) = tf in
    let tname = (typ_tostring_noattr t) in tname ^ "#" ^ f.fname;
  end

(* Convert a type, fieldinfo and access type into a string *)
let typ_field_access_tostring (t: typ) (f: fieldinfo) (rw: string) : string =
  begin
    let tname = (typ_tostring_noattr t) in tname ^ "#" ^ f.fname ^ "#" ^ rw;
  end

module StringOrder =
struct
  type t = string
  let compare s1 s2 = (String.compare s1 s2)
end

module Stringset = Set.Make (StringOrder)

module VarinfoOrder =
struct
  type t = varinfo * string
  let compare va1 va2 =
    let (v1,a1) = va1 in
    let (v2,a2) = va2 in
    let name_cmp = (String.compare v1.vname v2.vname) in
    (if name_cmp = 0 then (String.compare a1 a2) else name_cmp);
end

module Varinfoset = Set.Make (VarinfoOrder)

(*---------------------------------------------------------------------------*)
(** Points-to graph: data structures related to the points to graph *)
(** Node of the points-to graph *)
type ptgnode_t =
  | Ptgnode of typ

(** Node ordering *)
module PtgnodeOrder =
struct
  type t = ptgnode_t
  let compare n1 n2 =
    begin
      let Ptgnode(t1) = n1 in
      let Ptgnode(t2) = n2 in
      let s1 = (typ_tostring_noattr t1) in
      let s2 = (typ_tostring_noattr t2) in
      (String.compare s1 s2);
    end
end

(** Set of nodes *)
module Ptgnodeset = Set.Make(PtgnodeOrder)

(** Edges: corresponding to the points to relation. Edge is of the form
    src_node, fieldname, tgt_node * reftype. You can get the fieldinfo
    using the fieldname and the parent compinfo. Reftype determines
    whether the edge corresponds to the type of access (read/write/array)
*)
type ptgedge_t =
  | Ptgedge of ptgnode_t * string * ptgnode_t * string

(** Edge ordering *)
module PtgedgeOrder =
struct
  type t = ptgedge_t
  let compare e1 e2 =
    begin
      let Ptgedge(s1,f1,t1,r1) = e1 in
      let Ptgedge(s2,f2,t2,r2) = e2 in
      let srccmp = (PtgnodeOrder.compare s1 s2) in
      let tgtcmp = (PtgnodeOrder.compare t1 t2) in
      let fldcmp = (String.compare f1 f2) in
      let refcmp = (String.compare r1 r2) in
      if (srccmp <> 0) then srccmp
      else if (tgtcmp <> 0) then tgtcmp
      else if (fldcmp <> 0) then fldcmp else refcmp;
    end
end

(** Set of edges *)
module Ptgedgeset = Set.Make(PtgedgeOrder)

(** Points-to Graph *)
type ptgraph_t = {nodes: Ptgnodeset.t; edges: Ptgedgeset.t}

(*---------------------------------------------------------------------------*)
(** Points-to graph: functions related to the points-to-graph *)
(** Return an empty graph *)
let ptg_empty_graph () : ptgraph_t =
  begin
    let retgraph : ptgraph_t = {
      nodes = (Ptgnodeset.empty);
      edges = (Ptgedgeset.empty);
    } in
    retgraph;
  end

(** Adds a node to a graph and returns the new graph *)
let ptg_add_node (n: ptgnode_t) (g: ptgraph_t) : ptgraph_t =
  begin
    let retgraph : ptgraph_t = {
      nodes = (Ptgnodeset.add n g.nodes);
      edges = g.edges;
    } in
    retgraph;
  end

(** Adds an edge to a graph and returns the new graph *)
let ptg_add_edge (e: ptgedge_t) (g: ptgraph_t) : ptgraph_t =
  begin
    let retgraph : ptgraph_t = {
      nodes = g.nodes;
      edges = (Ptgedgeset.add e g.edges);
    } in
    retgraph;
  end

(* List of edges in the graph *)
let ptg_edgelist (g: ptgraph_t) : ptgedge_t list =
  begin
    (Ptgedgeset.elements g.edges);
  end

(* Get incoming edges *)
let ptg_get_incoming_edges (n: ptgnode_t) (g: ptgraph_t) : ptgedge_t list =
  begin
    let retval = ref [] in
    let edgelist = (ptg_edgelist g) in
    for i = 0 to (List.length edgelist) - 1 do
      let ithedge = (List.nth edgelist i) in
      let Ptgedge(ndsrc,_,ndtgt,_) = ithedge in
      if (PtgnodeOrder.compare ndtgt n) = 0
      then retval := (List.append !retval [ithedge]);
    done;
    !retval;
  end

(* Get outgoing edges *)
let ptg_get_outgoing_edges (n: ptgnode_t) (g: ptgraph_t) : ptgedge_t list =
  begin
    let retval = ref [] in
    let edgelist = (ptg_edgelist g) in
    for i = 0 to (List.length edgelist) - 1 do
      let ithedge = (List.nth edgelist i) in
      let Ptgedge(ndsrc,_,ndtgt,_) = ithedge in
      if (PtgnodeOrder.compare ndsrc n) = 0
      then retval := (List.append !retval [ithedge]);
    done;
    !retval;
  end


(** Removes an edge from the graph if it exists and returns the new graph.
 * Otherwise, the graph is returned unchanged *)
let ptg_del_edge (e: ptgedge_t) (g: ptgraph_t) : ptgraph_t =
  begin
    let edgelist = (ptg_edgelist g) in
    let newedges = ref (Ptgedgeset.empty) in
    for i = 0 to (List.length edgelist) - 1 do
      let ithedge = (List.nth edgelist i) in
      if (PtgedgeOrder.compare e ithedge) <> 0
      then newedges := (Ptgedgeset.add ithedge !newedges);
    done;
    let retgraph : ptgraph_t = {
      nodes = g.nodes;
      edges = !newedges;
    } in
    retgraph;
  end

(** Merge two input graphs by combining (using union) the node set
    and the edge set of both graphs *)
let ptg_merge_graphs (g1: ptgraph_t) (g2: ptgraph_t) : ptgraph_t =
  begin
    let retgraph : ptgraph_t = {
      nodes = (Ptgnodeset.union g1.nodes g2.nodes);
      edges = (Ptgedgeset.union g1.edges g2.edges);
    } in
    retgraph;
  end

(* String form of a node *)
let ptgnode_tostring (n: ptgnode_t) : string =
  begin
    let Ptgnode(nodetype) = n in
    (typ_tostring_noattr nodetype);
  end

let ptgnode_totyp (n: ptgnode_t) : typ =
  begin
    let Ptgnode(nodetype) = n in
    nodetype
  end

(* String form of an edge. *)
let ptgedge_tostring (e: ptgedge_t) : string =
  begin
    let Ptgedge(src,fld,tgt,reftype) = e in
    reftype ^ "     " ^
      (ptgnode_tostring src) ^ "--- [" ^ fld ^ "] --->" ^
      (ptgnode_tostring tgt);
  end

(* Print out the graph.  s is the name of the current function. *)
let ptg_dump (g: ptgraph_t) (s: string) : unit =
  begin
    let edgelist = (Ptgedgeset.elements g.edges) in
    for i = 0 to (List.length edgelist) - 1 do
      let ith = (List.nth edgelist i) in
      (Printf.fprintf stderr "%s\t%s\n%!" s (ptgedge_tostring ith));
    done;
  end

(** Number of nodes *)
let ptg_numnodes (g: ptgraph_t) : int =
  begin
    (Ptgnodeset.cardinal g.nodes);
  end

(** Number of edges *)
let ptg_numedges (g: ptgraph_t) : int =
  begin
    (Ptgedgeset.cardinal g.edges);
  end

(* Print out statistics *)
let ptg_stats (g: ptgraph_t) : unit =
  begin
    let numedges = (ptg_numedges g) in
    let numnodes = (ptg_numnodes g) in
    (Printf.fprintf stderr "Nodes: %d, Edges: %d\n%!" numnodes numedges);
  end

(* Print out the nodes and their outgoing edges in the points-to graph *)
let ptg_types_and_fields (g: ptgraph_t) : (typ * string * string) list =
  begin
    let retval = ref [] in
    let edgelist = (Ptgedgeset.elements g.edges) in
    for i = 0 to (List.length edgelist) - 1 do
      let ith = (List.nth edgelist i) in
      let Ptgedge(src,fld,_,ref) = ith in
      let Ptgnode(srctype) = src in
      retval := (srctype,fld,ref)::!retval
    done;
    !retval;
  end

(*---------------------------------------------------------------------------*)
(* A global representing the points-to graphs computed for all the functions *)
let points_to_graphs : (string, ptgraph_t) Hashtbl.t ref = ref (Hashtbl.create 117)

(* A global representing the resolved fields of structures *)
let resolved_fields : (string, (typ * fieldinfo * typ)) Hashtbl.t ref
    = ref (Hashtbl.create 117)

(* A global representing the resolved formals of functions *)
let resolved_formals : (string, (int * varinfo * typ)) Hashtbl.t ref
    = ref (Hashtbl.create 117)

(* A global representing the global variables accessed in each function.
 * This is per procedure, and does not have summaries of globals accessed
 * by all callees. *)
let globals_accessed_intra : (string, (varinfo * string)) Hashtbl.t ref
    = ref (Hashtbl.create 117)

let summary_globals_accessed : (string, (varinfo * string)) Hashtbl.t ref
    = ref (Hashtbl.create 117)

(* Fields of structures whose addresses are ever taken. The key is obtained
 * by concatenating the name of the fieldinfo with the name of the struct
 * that contains the fieldinfo.
 *)
let fields_addrstaken : (string, fieldinfo) Hashtbl.t ref
    = ref (Hashtbl.create 117)

let dfs_visited : (string, string) Hashtbl.t = (Hashtbl.create 117)

(*---------------------------------------------------------------------------*)
(* Basic queries *)
(** Get the points to graph for a particular function *)
let get_ptg_func (funcname: string) : ptgraph_t =
  begin
    (try
       (Hashtbl.find !points_to_graphs funcname);
     with Not_found -> (
       (*(warning ["Points-to graph not found for function"; funcname]);*)
       (ptg_empty_graph());
     ));
  end

(** Flatten the points-to-graph into a list of "types"+"fields" and make it
 * edible for consumption by the marshaler *)
let rec get_flattened_ptg (funcname: string) (get_bigptg: bool):
    (string, (typ * string * string)) Hashtbl.t =
  if get_bigptg = false then
    begin 
      let rethash : (string, (typ * string * string)) Hashtbl.t =
        Hashtbl.create 11 in
      let ptg = get_ptg_func funcname in
      let ptg_l = ptg_types_and_fields ptg in
      for i = 0 to (List.length ptg_l) - 1 do
        let ith = List.nth ptg_l i in
        let (ithtyp,_,_) = ith in
        Hashtbl.add rethash (typ_tostring_noattr ithtyp) ith;
      done;
      Hashtbl.copy rethash;
    end
  else
    get_flattened_ptg "perform_full_sync" false;;

(** Flatten the points-to-graph into a form edible for consumption by the
 * marshaling annotation module *)
let get_flattened_ptg1 (funcname: string) :
    (string, (typ * fieldinfo * string)) Hashtbl.t =
  begin
    let rethash : (string, (typ * fieldinfo * string)) Hashtbl.t =
      (Hashtbl.create 11) in
    let ptg = (get_ptg_func funcname) in
    let ptg_l = (ptg_types_and_fields ptg) in
    for i = 0 to (List.length ptg_l) - 1 do
      let ith = (List.nth ptg_l i) in
      let (ithtyp,ithfldnm,ithref) = ith in
      let ithcomp = (tcomp_compinfo ithtyp) in
      (match ithcomp with
         | Some(ithcompinfo) ->
             let ithfldinfo = (get_fieldinfo ithcompinfo ithfldnm) in
             (Hashtbl.add rethash funcname (ithtyp,ithfldinfo,ithref));
         | None -> ();
      );
    done;
    (Hashtbl.copy rethash);
  end

(*---------------------------------------------------------------------------*)
(** Step 1: Pruning roots
 *
 * Prune the set of roots by gathering (per procedure) the memory locations
 * used/written. This procedure identifies the following information within
 * each procedure:
 * 1. The fields of a structure that are read from/written to in the proc.
 *    Information about whether an access is a read or a write is not fully
 *    accurate, though.
 * 2. The set of globals that are accessed in each procedure.
 * 3. Whether a pointer is involved in pointer arithmetic (indicating that
 *    the pointer may point to a block containing multiple instances of that
 *    object).
 * 4. The fields of a structure whose addresses are taken.
 *)
class prune_roots_analysis = object (self)
  inherit nopCilVisitor
    (* Hashtable of statements that have been analyzed *)
  val mutable visited_stmts : (stmt, bool) Hashtbl.t
    = (Hashtbl.create 117);

  (* Current set of field accesses. We gather these and then store them
     in the hashtable field_Accesses_1 so that that hashtable will not
     have repeats. *)
  val mutable curr_field_accesses : Stringset.t = Stringset.empty;

  (* Hashtable storing field accesses: This converts the type, field and
   * type of access into a string and stores it here with the funcname
   * i.e., hashtable contains: funcname::string(type,field,rw) *)
  val mutable field_accesses_1 : (string, string) Hashtbl.t
    = (Hashtbl.create 117);

  (* Hashtable storing the mapping of the string representing a type, field
   * and type of access into the actual data, i.e., hashtable contains the
   * data: string(type,field,rw)::type,field,rw *)
  val mutable field_accesses_2 : (string, (typ * fieldinfo * string)) Hashtbl.t
    = (Hashtbl.create 117);

  (* Current set of global accesses. We gather these as a set and store them
     in the hashtable glob_accesses, so as to prevent repeated accesses *)
  val mutable curr_glob_accesses : Varinfoset.t = Varinfoset.empty;

  (* Hashtable storing global accesses: This converts the set
   * curr_glob_accesses into a hashtable. The hashtable stores
   * funcname::varinfo,rw *)
  val mutable glob_accesses: (string, (varinfo * string)) Hashtbl.t
    = (Hashtbl.create 117);

  (* Method logaccess_field: Logs this access of the data structure field *)
  method logaccess_field (t: typ) (field: fieldinfo) (rw: string): unit =
    begin
      let str = (typ_field_access_tostring t field rw) in
      curr_field_accesses <- (Stringset.add str curr_field_accesses);
      (add_if field_accesses_2 str (t,field,rw));
    end

  (* log_all_fields: Logs accesses to all fields of a compinfo. If we have
   * any structs as fields, logs accesses to fields of those as well*)
  method log_all_fields (cinfo: compinfo) (rw: string) : unit =
    begin
      for i = 0 to (List.length cinfo.cfields) - 1 do
        let ith = (List.nth cinfo.cfields i) in
        let container_typ = (TComp(ith.fcomp,[])) in
        (self#logaccess_field container_typ ith "write");
        (self#logaccess_field container_typ ith "read ");
        (* warning ["log_all_fields:"; ith.fname; (typ_tostring container_typ)]; *)
        (match ith.ftype with
           | TComp(cinfo',_) -> (self#log_all_fields cinfo' rw);
           | _ -> ();
        );
      done;
    end

  (* Method logaccess_glob: Log access to a global. Only log access if it
     is not of function type. *)
  method logaccess_glob (v: varinfo) (rw: string) : unit =
    begin
      (match v.vtype with
         | TFun(_) -> ();
         | _ ->
             curr_glob_accesses <- (Varinfoset.add (v,rw) curr_glob_accesses);
      );
    end

  (* get the leaf field accessed *)
  method get_leaf_field_accessed_lval (lv: lval) : fieldinfo option =
    begin
      let (lh, off) = lv in
      (match off with
         | NoOffset -> None;
         | Field(finfo, NoOffset) -> Some(finfo);
         | Field(_, off') -> (self#get_leaf_field_accessed_lval (lh, off'));
         | Index(_, NoOffset) -> None;
         | Index(_, off') -> (self#get_leaf_field_accessed_lval (lh, off'));
      );
    end

  (* scan_and_log_storeoffset: This scans the fields of an lval of
   * compound type and logs accesses to all fields that are marked
   * with the STOREOFFSET annotation *)
  method scan_and_log_storeoffset (t: typ) (rw: string) : unit =
    begin
      (match t with
	 | TComp(cinfo,_) ->
	     for i = 0 to (List.length cinfo.cfields) - 1 do
	       let ith = (List.nth cinfo.cfields i) in
	       if (Marshannot_dri.is_storeoffset ith.ftype) = true then
		 begin (self#logaccess_field t ith rw); end;
	     done;
	 | TPtr(t',_) -> (self#scan_and_log_storeoffset t' rw);
	 | TNamed(tinfo,_) -> (self#scan_and_log_storeoffset tinfo.ttype rw);
	 | TArray(t',_,_) -> (self#scan_and_log_storeoffset t' rw);
	 | _ -> ();
      );
    end

  (* Analyze Offsets: This is where the action happens. Fields of data
     structures that are accessed are gathered here. *)
  method analysis_offset (off: offset) (rw: string) : unit =
    begin
      (match off with
         | NoOffset -> ();
         | Field(finfo,off') ->
             (*(Printf.fprintf stderr "Analyzing field %s %s\n" finfo.fname (typ_tostring finfo.ftype));*)

             (* MJR Was originally just (self#logaccess_field (TComp(finfo.fcomp,[])) finfo rw);
              * Added the match command to see if we're accessing a pointer.
              * If we are, log that as a WRITE to that field.  This is necessary because
              * once we read the pointer, we could do anything with it, including write
              * to things it points to.  This dilutes the meaning of WRITE because we're
              * not writing the pointer itself, rather, we're writing things it points to.
              *)
             (match finfo.ftype with
                | TPtr (_,_) -> (self#logaccess_field (TComp(finfo.fcomp,[])) finfo "write");
                | _ -> (self#logaccess_field (TComp(finfo.fcomp,[])) finfo rw);
             );
             (self#analysis_offset off' rw);
         | Index(_,off') ->
             ((* (warning["Check how to deal with array indexes"]) *));
             (self#analysis_offset off' rw);
      );
    end

  (* Analyze an Lval: This is another place where the action happens.
     Fields are also gathered here. We also check for globals here. *)
  method analysis_lval (lv: lval) (rw: string) : unit =
    begin
      (self#scan_and_log_storeoffset (typeOfLval lv) rw);
      let (lh,off) = lv in (self#analysis_offset off rw);
      (match lh with
         | Var(v) ->
	     (* The only place where we check for globals *)
             (if (v.vglob = true) then (self#logaccess_glob v rw));
	     (* Check the type of this varinfo. If it is a CONTAINER pointer,
		log access to the relevant type and field *)
	     if (is_container v.vtype) = true then
	       begin
		 let cont = (typeof_container v.vtype) in
		 let fldopt = (fieldof_container v.vtype) in
		 (match fldopt with
		    | Some(fld) -> (self#logaccess_field cont fld "read ");
		    | None -> (fatal ["No valid field found"]);
		 );
	       end;
         | Mem(e') ->
             (* (Printf.fprintf stderr "Mem Analyzing : %s\n" (lval_tostring lv)); *)
             (self#analysis_exp e' rw);
      );
    end

  (* Analyze an expression *)
  method analysis_exp (e: exp) (rw: string) : unit =
    begin
      (match e with
         | Const(_) -> ();
         | Lval(lv) ->
             (* (Printf.fprintf stderr "lval Analyzing : %s\n" (exp_tostring e));*)
             (self#analysis_lval lv rw);
         | SizeOf(_) -> ();
         | SizeOfE(e') -> (self#analysis_exp e' rw);
         | SizeOfStr(_) -> ();
         | AlignOf(_) -> ();
         | AlignOfE(e') -> (self#analysis_exp e' rw);
         | UnOp(_,e',_) -> (self#analysis_exp e' rw);
         | BinOp(_,e1,e2,t') ->
             (* MJR replaced with new code below the commented
                code to handle more possibilities. *)
             
             (*if (isPointerType (typeOf e1))
             then ((self#analysis_exp e1 "arith"); (self#analysis_exp e2 rw))
             else if (isPointerType (typeOf e2))
             then ((self#analysis_exp e1 rw); (self#analysis_exp e2 "arith"))
             else ((self#analysis_exp e1 rw); (self#analysis_exp e2 rw));*)

             let newrw1 = (if (isPointerType (typeOf e1)) then "arith" else rw) in
             let newrw2 = (if (isPointerType (typeOf e2)) then "arith" else rw) in
             (self#analysis_exp e1 newrw1);
             (self#analysis_exp e2 newrw2);
         | CastE(_,e') -> (self#analysis_exp e' rw);
         | AddrOf(lv) ->
             (* MJR:  was rw instead of WRITE.  The idea is that if we take
              * the address, then we could be doing all kinds of things with
              * it, including write to it.  If we pass this address around,
              * it may be written to.  Note, like above, this dilutes the
              * meaning of WRITE.  (search for "dilute")
              *)
             (self#analysis_lval lv "write");
             (match (self#get_leaf_field_accessed_lval lv) with
                | Some(finfo) ->
                    let finfo_name = (fieldinfo_name finfo) in
                    (add_if !fields_addrstaken finfo_name finfo);
                | None -> ();
             );
         | StartOf(lv) -> (self#analysis_lval lv rw);
      );
    end

  (* Analyze an instruction *)
  method analysis_instr (i: instr) : unit =
    begin
      (match i with
         | Set(lv,e,_) ->
             begin
               (self#analysis_lval lv "write");
               (self#analysis_exp e "read ");
               (* If the lv itself is a struct, that means all fields of the struct
                  are being written to *)
               (match (typeOfLval lv) with
                  | TComp(cinfo, _) ->
                      (self#log_all_fields cinfo "write");
                      (self#log_all_fields cinfo "read ");
                  | _ -> ();
               );
             end;
         | Call(lvopt,e,el,_) ->
             begin
               (match lvopt with
                  | Some(lv) -> (self#analysis_lval lv "write");
                  | None -> ();
               );
               (self#analysis_exp e "read ");
               (* MJR: The expressions below could also be a WRITE if it refers to
                * pointer data.  It will be a READ in most cases, but we'll err on
                * the side of being conservative.  Was READ originally.
                *)
               (* A Call with a pointer argument could potentially modify the argument *)
               for i = 0 to (List.length el) - 1 do
                 let ith = (List.nth el i) in
                 (self#analysis_exp ith "write");
               done;
             end;
         | Asm(_) ->
             ((*(warning ["Not handling inline assembly in copy analysis"])*));
      );
    end

  (* Analyze a statement *)
  method analysis_stmt (s: stmt) : unit =
    begin
      try
        (ignore (Hashtbl.find visited_stmts s));
      with Not_found -> (
        begin
          (Hashtbl.add visited_stmts s true);
          (match s.skind with
             | Instr(il) -> (ignore (List.map self#analysis_instr il));
             | Return(eopt,_) ->
                 (match eopt with
                    | Some(e) ->
                        (* XXX: READ or a WRITE?  MJR was READ originally *)
                        (self#analysis_exp e "write");
                    | None -> ();
                 );
             | Goto(_,_) -> ();
             | Break(_) -> ();
             | Continue(_) -> ();
             | If(e,tb,fb,_) ->
                 begin
                   (self#analysis_exp e "read ");
                   (self#analysis_block tb);
                   (self#analysis_block fb);
                 end;
             | Switch(e,b,sl,_) ->
                 begin
                   (self#analysis_exp e "read ");
                   (self#analysis_block b);
                   (ignore (List.map self#analysis_stmt sl));
                 end;
             | Loop(b,_,s1opt,s2opt) ->
                 begin
                   (self#analysis_block b);
                   (match s1opt with
                      | Some(s1) -> (self#analysis_stmt s1);
                      | None -> ();
                   );
                   (match s2opt with
                      | Some(s2) -> (self#analysis_stmt s2);
                      | None -> ();
                   );
                 end;
             | Block(b) -> (self#analysis_block b);
             | _ -> ((fatal ["Unhandled case"]));
          );
        end);
    end

  (* Analyze blocks *)
  method analysis_block (b: block) : unit =
    begin
      (ignore (List.map self#analysis_stmt b.bstmts));
    end

  (* Visitor performs an analysis on each statement in the function. Done
   * using a vfunc visitor because we'd like to summarize information on a
   * per-function basis *)
  method vfunc (f: fundec) : fundec visitAction =
    begin
      (Printf.fprintf stderr "currently analyzing: %s\n" f.svar.vname);
      (self#analysis_block f.sbody);

      let field_accesses_list = (Stringset.elements curr_field_accesses) in
      for i = 0 to (List.length field_accesses_list) - 1 do
        let ith = (List.nth field_accesses_list i) in
        (Hashtbl.add field_accesses_1 f.svar.vname ith);
      done;
      curr_field_accesses <- Stringset.empty;

      let glob_accesses_list = (Varinfoset.elements curr_glob_accesses) in
      for i = 0 to (List.length glob_accesses_list) - 1 do
        let ith = (List.nth glob_accesses_list i) in
        (Hashtbl.add glob_accesses f.svar.vname ith);
      done;
      curr_glob_accesses <- Varinfoset.empty;

      DoChildren;
    end

  (* Return the set of field accesses, without repeats, per function *)
  method get_field_accesses () : (string, (typ * fieldinfo * string)) Hashtbl.t =
    begin
      let retval : (string, (typ * fieldinfo * string)) Hashtbl.t
          = (Hashtbl.create 117) in
      let allfuncs = (remove_repeats (list_keys field_accesses_1)) in
      for i = 0 to (List.length allfuncs) - 1 do
        let ith = (List.nth allfuncs i) in
        let allaccesses = (Hashtbl.find_all field_accesses_1 ith) in
        for j = 0 to (List.length allaccesses) - 1 do
          let jth_str = (List.nth allaccesses j) in
          let jth_access = (Hashtbl.find field_accesses_2 jth_str) in
          (Hashtbl.add retval ith jth_access);
        done;
      done;
      (Hashtbl.copy retval);
    end

  (* Return the set of global accesses, without repeats, per function *)
  method get_global_accesses() : (string, (varinfo * string)) Hashtbl.t =
    begin
      (Hashtbl.copy glob_accesses);
    end

  (* Print out a single field.  Used in dump_info *)
  method dump_field (fld : typ * fieldinfo * string) : unit =
    begin
      let (t, f, rw) = fld in
      let tname = (Pretty.sprint 50 (d_typsig() (typeSig t))) in
      (Printf.fprintf stderr "\n  Type:    %s\n" tname);
      let ftname = (Pretty.sprint 50 (d_typsig() (typeSig f.ftype))) in
      (try
         (Printf.fprintf stderr "  Field:   %s (%d bytes) %s\n" f.fname
            ((bitsSizeOf f.ftype)/8) ftname);
       with Cil.SizeOfError(error, _) ->
         (Printf.fprintf stderr "  Field:   %s (unknown size, %s) %s\n"
            f.fname error ftname);
      );
      (Printf.fprintf stderr "  Access:  %s\n" rw);
    end

  method dump_global (glob: (varinfo * string)) : unit =
    begin
      let (gl,rw) = glob in
      (Printf.fprintf stderr "\t %s [%s]\n" gl.vname rw);
    end

  (* Dump analysis information *)
  method dump_info () : unit =
    begin
      let allfuncs = (remove_repeats (list_keys field_accesses_1)) in
      for i = 0 to (List.length allfuncs) - 1 do
        let ith = (List.nth allfuncs i) in
        let allaccesses = (Hashtbl.find_all field_accesses_1 ith) in
        if (List.length allaccesses) > 0 then
          (Printf.fprintf stderr "Summary for %s:" ith);
        for j = 0 to (List.length allaccesses) - 1 do
          let jth = (List.nth allaccesses j) in
          (self#dump_field (Hashtbl.find field_accesses_2 jth));
        done;
        let allglobals = (Hashtbl.find_all glob_accesses ith) in
        if (List.length allglobals) > 0 then
          (Printf.fprintf stderr "Globals for %s:\n" ith);
        for j = 0 to (List.length allglobals) - 1 do
          let jth = (List.nth allglobals j) in
          (self#dump_global jth);
        done;
      done;
    end

  (* Top-level method for dumping fields information *)
  method invoke_analysis (f: file) : unit =
    begin
      (visitCilFile (self :> cilVisitor) f);
      (*(self#dump_info ());*)
    end
end


(*---------------------------------------------------------------------------*)
(** Step 2: Resolving opaque pointers using casts.
 * Determine the type of each opaque pointer by inspecting its use in the
 * driver. We consider two kinds of opaque pointers: fields of structs, and
 * parameters to functions. For function parameters, we inspect, in addition
 * to the use of the parameter, how the function is invoked.
 *
 * This analysis only inspects casts to determine the types of opaque pointers.
 * Once some opaque pointers are resolved, more opaque pointers can be resolved
 * using these newly-resolved opaques. Step (3) handles this transitive closure
 * that propagates opaques.
 *)
class resolve_opaques_using_casts = object(self)
  inherit nopCilVisitor
    (* Current function being analyzed *)
  val mutable curr_fundec = dummyFunDec;

  (* Hashtable storing the set of opaque pointers that appear as fields
     of types. The hashtable stores 'string(type,field)::type,field' *)
  val mutable opaque_fields : (string, (typ * fieldinfo)) Hashtbl.t
    = (Hashtbl.create 11);

  (* Hashtable storing the set of opaque function formal parameters. The
     hashtable stores the mapping 'function-name::varinfo of formal'.
     Also store the parameter number (i.e., first, second etc.) that was
     determined to be void *)
  val mutable opaque_formals : (string, (int * varinfo)) Hashtbl.t
    = (Hashtbl.create 11);

  (* Hashtable that stores the resolved opaque fields. The hashtable
   * stores 'string(type_of_struct,field)::(type_of_struct,fieldinfo,resolved_type_of_field)'.
   * Note that there can be multiple bindings if the field is cast in
   * multiple ways. *)
  val mutable resolved_opaque_fields : (string, (typ * fieldinfo * typ)) Hashtbl.t
    = (Hashtbl.create 11);

  (* Those opaque pointers that we could not resolve. *)
  val mutable unresolved_opaque_fields : (typ * fieldinfo) list = [];

  (* Hashtable that stores the resolved type of function paramters. The
   * table stores '(funcname,varinfo),type_of_param'. Multiple bindings
   * can exist if the parameter is cast in multiple ways *)
  val mutable resolved_opaque_formals : (string, (int * varinfo * typ)) Hashtbl.t
    = (Hashtbl.create 11);

  (* Those opaque pointers that we could not resolve *)
  val mutable unresolved_opaque_formals : (string * (int * varinfo)) list = [];

  (* Accessor functions *)
  method get_resolved_fields() = (Hashtbl.copy resolved_opaque_fields);
  method get_resolved_formals() = (Hashtbl.copy resolved_opaque_formals);
  method get_unresolved_fields() = unresolved_opaque_fields;
  method get_unresolved_formals() = unresolved_opaque_formals;

  (* Gather the set of opaque pointers and populate the hashtables
   * opaque_fields and opaque_formals. Input is a set of field accesses
   * grouped by function, i.e., funcname::typ,field,accesskind. The
   * type here denotes the TComp and the fieldinfo denotes the field
   * of this TComp that is accessed. *)
  method gather_opaque_pointers
    (f: file)
    (field_accesses: (string, (typ * fieldinfo * string)) Hashtbl.t) : unit =
    begin
      (* Go through field accesses and identify the void * field accesses *)
      let allaccesses = (list_bindings field_accesses) in
      for i = 0 to (List.length allaccesses) - 1 do
        let ith = (List.nth allaccesses i) in
        let (t,f,_) = ith in
        if (isVoidPtrType f.ftype) then
          begin
            let str = (typ_field_access_tostring t f "") in
            (add_if opaque_fields str (t,f));
          end
      done;
      (* Go through function parameters and identify void * parameters *)
      let allfuncs = ref [] in
      for i = 0 to (List.length f.globals) - 1 do
        let ith = (List.nth f.globals i) in
        (match ith with
           | GFun(func,_) -> allfuncs := func::!allfuncs;
           | _ -> ();
        );
      done;
      for i = 0 to (List.length !allfuncs) - 1 do
        let ith = (List.nth !allfuncs i) in
        for j = 0 to (List.length ith.sformals) - 1 do
          let jth = (List.nth ith.sformals j) in
          if (isVoidPtrType jth.vtype) then
            begin
              (add_if opaque_formals ith.svar.vname (j,jth));
            end
        done;
      done;
    end

  (* Analysis of an offset. This helps us resolve opaque fields. *)
  method analysis_offset (off: offset) (t: typ) : unit =
    begin
      (match off with
         | NoOffset -> ();
         | Field(finfo,off') ->
             let allopaquefields = (list_bindings opaque_fields) in
             for i = 0 to (List.length allopaquefields) - 1 do
               let (ithtyp,ithfield) = (List.nth allopaquefields i) in
               if ((String.compare ithfield.fname finfo.fname) = 0) &&
                 ((String.compare (typ_tostring_noattr ithtyp)
                     (typ_tostring_noattr (TComp(finfo.fcomp,[])))) = 0) &&
                 ((isVoidType t) = false) &&
                 ((isVoidPtrType t) = false)
               then
                 begin
                   (add_if_binding resolved_opaque_fields
                      (typ_field_tostring (ithtyp,finfo)) (ithtyp,finfo,t));
                 end;
             done;
             (self#analysis_offset off' t);
         | Index(_,off') -> (self#analysis_offset off' t);
      );
    end

  (* Analysis of an lval to determine uses of opaque pointers *)
  (* For formal paramters, we match the name of the parameter against uses of
     variables in the source code of the function. For field names, we match
     irrespective of the current function we're analyzing *)
  method analysis_lval (lv: lval) (t: typ) : unit =
    begin
      let (lh,off) = lv in
      (* Match variable names here. For formal parameters, we only match them in
         the body of the current function. *)
      (match lh with
         | Mem(e') -> (self#analysis_exp e');
         | Var(v) ->
             let curr_opaqueformalsandlocs =
               (Hashtbl.find_all opaque_formals curr_fundec.svar.vname) in
             for i = 0 to (List.length curr_opaqueformalsandlocs) - 1 do
               let (ithloc,ithvinfo) = (List.nth curr_opaqueformalsandlocs i) in
               if (String.compare v.vname ithvinfo.vname) = 0 &&
                 ((isVoidPtrType t) = false) &&
                 ((isVoidType t) = false)
               then begin
                 (add_if_binding resolved_opaque_formals
                    curr_fundec.svar.vname (ithloc,v,t));
               end;
             done;
      );
      (* Analyze offsets here to check for field names *)
      (self#analysis_offset off t);
    end;

  (* Analysis of an exp to determine uses of opaque pointers. Key observation:
   * if you're going to use an opaque pointer, you MUST cast it, thus, we can
   * analyze casts to resolve the type of an opaque pointer. *)
  method analysis_exp (e: exp) : unit =
    begin
      (match e with
         | CastE(t,e') ->
             (match e' with
                | Lval(lv) -> (self#analysis_lval lv t);
                | _ -> (self#analysis_exp e');
             );
         | Lval(lv) -> (self#analysis_lval lv voidType);
         | SizeOfE(e') -> (self#analysis_exp e');
         | UnOp(_,e',_) -> (self#analysis_exp e');
         | BinOp(_,e1,e2,_) -> (self#analysis_exp e1); (self#analysis_exp e2);
         | AddrOf(lv) -> (self#analysis_lval lv voidType);
         | StartOf(lv) -> (self#analysis_lval lv voidType);
         | _ -> ();
      );
    end

  (* Analysis of an instruction to determine uses of opaque pointers *)
  method analysis_instr (i: instr) : unit =
    begin
      (match i with
         | Set(lv,e,loc) ->
             if (isVoidPtrType (typeOfLval lv)) = true
             then begin
               (* If you see an assignment a = (cast) b, and a is void, you know
                  that a should have the type of b. Otherwise, just analyze the
                  lval a with voidType *)
               (match e with
                  | CastE(t,e') ->
                      (self#analysis_lval lv (typeOf e')); (self#analysis_exp e);
                  | _ -> (self#analysis_lval lv voidType); (self#analysis_exp e);
               );
             end else begin
               (self#analysis_lval lv voidType); (self#analysis_exp e);
             end;
	     (* Check for Ints that may be assigned pointers! If so, we must
	      * insert an ISPOINTER annotation for these. NOTE: I currently do
	      * not do a full-fledged resolution analysis on these pointers. I
	      * just emit a warning, that's all *)
	     (* CHECK 1: integer_lval = (cast_into_integer) pointer_exp *)
	     if (isIntegralType (typeOfLval lv)) = true
	     then begin
	       (match e with
		  | CastE(t, e') ->
		      if (isPointerType (typeOf e')) = true then
			begin
                          let warn = Printf.sprintf
                            "\nInt used as pointer:\nChange to the appropriate pointer type! (2)\n%s\n%s\n%s"
                            (lval_tostring lv) (typ_tostring (typeOf e)) (instr_tostring i)
                          in
			  addwarn [warn];
			end;
		  | _ -> ();
	       );
	     end;
	     (* CHECK 2: pointer_lval = (cast_into_pointer) integer_exp *)
	     if (isPointerType (typeOfLval lv)) = true
	     then begin
	       (match e with
		  | CastE(t, e') ->
		      if (isIntegralType (typeOf e')) = true then
			begin
                          let warn = Printf.sprintf
                            "\nInt used as pointer:\nChange to the appropriate pointer type! (2)\n%s\n%s\n%s"
                            (lval_tostring lv) (typ_tostring t) (instr_tostring i)
                          in
			  addwarn [warn];
			end;
		  | _ -> ();
	       );
	     end;
         | Call(lvopt,e,el,_) ->
             (* Handling a Call is tricky. This is because for function formal
              * parameters, we need to (i) see how the formal parameter is used in
              * the body of the function, and (ii) track how the function that has a
              * void * is called in the driver. For (i), we will need to see if an
              * opaque formal parameter is ever used as an actual parameter with a
              * call to another function, so we need to analyze the expression list.
              * For (ii), we will need to see whether the function being called is
              * one with an opaque formal. In that case, we will need to see how the
              * function is being called to see whether we can resolve the opaque
              * argument *)
             (* This handles case (i), to check for opaque formal parameters used in
              * calls to other functions *)
             (ignore (List.map self#analysis_exp el));
             (* This handles case (ii), to check if the function being called is a
              * function with an opaque formal, and if so, to determine whether we
              * can resolve the type of the formal based on actuals passed to it *)
             (match e with
                | Lval(Var(fname),_) ->
                    (let funcswithopaqueparams = (list_keys opaque_formals) in
                     if (List.mem fname.vname funcswithopaqueparams) = true
                     then begin
                       let opaquelocs = (Hashtbl.find_all opaque_formals fname.vname) in
                       for k = 0 to (List.length opaquelocs) - 1 do
                         let (kthloc,kthvinfo) = (List.nth opaquelocs k) in
                         (try
                            let kth_actual = (List.nth el kthloc) in
                            (match kth_actual with
                               | CastE(casttyp,e') ->
                                   (* This handles cases where the function foo with an opaque
                                    * param is invoked as foo((void_star) actual). In this case,
                                    * we can examine the type of 'actual' *)
                                   (if (isVoidPtrType casttyp) && ((isVoidPtrType (typeOf e')) = false)
                                    then begin
                                      (add_if_binding resolved_opaque_formals
                                         fname.vname (kthloc,kthvinfo,(typeOf e')));
                                      (*(Printf.fprintf stderr
                                        "Resolved param %d:(%s) as %s based on call\n"
                                        kthloc kthvinfo.vname (typ_tostring_noattr (typeOf e')));*)
                                    end);
                               | _ -> ();
                            );
                          with Failure("nth") -> ());
                       done;
                     end);
                | _ -> (); (* Don't currently support cases where the call is via a funcptr *)
             );
         | _ -> ();
      );
    end

  (* Analysis of a statement to determine uses of opaque pointers *)
  method analysis_stmt (s: stmt) : unit =
    begin
      (match s.skind with
         | Instr(il) -> (ignore (List.map self#analysis_instr il));
         | Return(eopt,_) ->
             (match eopt with
                | Some(e) -> (self#analysis_exp e);
                | _ -> ();
             );
         | If(e,tb,fb,_) ->
             begin
               (self#analysis_exp e);
               (self#analysis_block tb);
               (self#analysis_block fb);
             end;
         | Block(b) -> (self#analysis_block b);
         | Switch(e,b,sl,_) ->
             begin
               (self#analysis_exp e);
               (self#analysis_block b);
               (ignore (List.map self#analysis_stmt sl));
             end;
         | Loop(b,_,_,_) -> (self#analysis_block b);
         | _ -> ();
      );
    end

  (* Analysis of a block to determine uses of opaque pointers *)
  method analysis_block (b: block) : unit =
    begin
      (ignore (List.map self#analysis_stmt b.bstmts));
    end

  (* Visitor to examine each statement in the program and identify the uses
     of opaque pointers *)
  method vfunc (fdec: fundec) : fundec visitAction =
    begin
      curr_fundec <- fdec;
      (self#analysis_block fdec.sbody);
      DoChildren;
    end

  (* Find unresolved opaque pointers *)
  method find_unresolved_opaque_pointers () : unit =
    begin
      let allopaquefields = (list_bindings opaque_fields) in
      let resolvedopaques = (list_keys resolved_opaque_fields) in
      for i = 0 to (List.length allopaquefields) - 1 do
        let ith = (List.nth allopaquefields i) in
        let ith_tostring = (typ_field_tostring ith) in
        if (List.mem ith_tostring resolvedopaques) = false
        then begin
          unresolved_opaque_fields <- ith::unresolved_opaque_fields;
        end
      done;
      let allopaqueformals = (list_keybindings opaque_formals) in
      for i = 0 to (List.length allopaqueformals) - 1 do
        let ith = (List.nth allopaqueformals i) in
        let (ithfun,(ithloc,ithvinfo)) = ith in
        try
          (ignore (Hashtbl.find resolved_opaque_formals ithfun));
          let allresolved = (Hashtbl.find_all resolved_opaque_formals ithfun) in
          (* List the parameters that were resolved *)
          let allresolved_param_names = ref [] in
          for j = 0 to (List.length allresolved) - 1 do
            let (locinfo,vinfo,t) = (List.nth allresolved j) in
            allresolved_param_names := vinfo.vname::!allresolved_param_names;
          done;
          if (List.mem ithvinfo.vname !allresolved_param_names) = false
          then unresolved_opaque_formals <- ith::unresolved_opaque_formals;
        with Not_found -> (
          (* This means that none the function's parameters were resolved *)
          unresolved_opaque_formals <- ith::unresolved_opaque_formals;
        );
      done;
    end

  (* Dump information on resolved opaque pointers *)
  method dump_resolved_opaque_pointers () : unit =
    begin
      (Printf.fprintf stderr "Resolved opaque pointers:\n%!");
      (* Resolved struct fields *)
      let all_resolved_fields = (list_keybindings resolved_opaque_fields) in
      for i = 0 to (List.length all_resolved_fields) - 1 do
        let ith = (List.nth all_resolved_fields i) in
        let (_,(structtyp,field,restyp)) = ith in
        (Printf.fprintf stderr "\tStruct:%s Field:%s resolved to %s\n"
           (typ_tostring_noattr structtyp) field.fname (typ_tostring_noattr restyp));
      done;
      (* Resolved function parameters *)
      let all_resolved_formals = (list_keybindings resolved_opaque_formals) in
      for i = 0 to (List.length all_resolved_formals) - 1 do
        let ith = (List.nth all_resolved_formals i) in
        let (fname,(locinfo,vinfo,restyp)) = ith in
        (Printf.fprintf stderr "\tFunction:%s Parameter:%s resolved to %s\n"
           fname vinfo.vname (typ_tostring_noattr restyp));
      done;
    end

  (* Dump information on unresolved opaque pointers *)
  method dump_unresolved_opaque_pointers () : unit =
    begin
      (Printf.fprintf stderr "Could not resolve these opaque pointers:\n%!");
      (* Unresolved struct fields *)
      for i = 0 to (List.length unresolved_opaque_fields) - 1 do
        let (ithcomp,ithfield) = (List.nth unresolved_opaque_fields i) in
        (Printf.fprintf stderr "\tStruct:%s Field:%s\n"
           (typ_tostring_noattr ithcomp) ithfield.fname);
      done;
      (* Unresolved function parameters *)
      for i = 0 to (List.length unresolved_opaque_formals) - 1 do
        let (ithfun,(ithloc,ithvinfo)) = (List.nth unresolved_opaque_formals i) in
        (Printf.fprintf stderr "\tFunction:%s Parameter:%s\n" ithfun ithvinfo.vname);
      done;
    end

  (* Main method to invoke the analysis *)
  method invoke_analysis (f: file)
    (field_accesses: (string, (typ * fieldinfo * string)) Hashtbl.t)
    : unit =
    begin
      (self#gather_opaque_pointers f field_accesses);
      (* Find types of opaque pointers by examining their uses *)
      (visitCilFile (self :> cilVisitor) f);
      (* Find the opaque pointers that were unresolved *)
      (self#find_unresolved_opaque_pointers());
      (*
      (* Dump information about resolved opaque pointers *)
          (self#dump_resolved_opaque_pointers());
      *)
      (* Dump information about unresolved opaque pointers *)
          (self#dump_unresolved_opaque_pointers());
      
    end
end


(*---------------------------------------------------------------------------*)
(** Step 3: Propagate resolved pointers. The idea is that step 2 resolves a set
 * of "seed" opaques by examining casts. This step propagates these seeds by
 * examining assignment statements in the program. THIS STEP DOES NOT EXAMINE
 * CASTS ONCE AGAIN.
 *)
class resolve_opaques_using_prop = object(self)
  inherit nopCilVisitor
    (* Current function being analyzed *)
  val mutable curr_fundec = dummyFunDec;

  (* Resolved opaque fields *)
  val mutable resolved_fields : (string, (typ * fieldinfo * typ)) Hashtbl.t
    = (Hashtbl.create 11);

  val mutable newly_resolved_fields : (string, (typ * fieldinfo * typ)) Hashtbl.t
    = (Hashtbl.create 11);

  (* Resolved opaque formals *)
  val mutable resolved_formals : (string, (int * varinfo * typ)) Hashtbl.t
    = (Hashtbl.create 11);

  val mutable newly_resolved_formals : (string, (int * varinfo * typ)) Hashtbl.t
    = (Hashtbl.create 11);

  (* Unresolved opaque fields *)
  val mutable unresolved_fields : (typ * fieldinfo) list = [];

  (* Unresolved opaque formals *)
  val mutable unresolved_formals : (string * (int * varinfo)) list = [];

  (* Accessor fucntions *)
  method get_resolved_fields() = (Hashtbl.copy resolved_fields);
  method get_resolved_formals() = (Hashtbl.copy resolved_formals);
  method get_unresolved_fields() = unresolved_fields;
  method get_unresolved_formals() = unresolved_formals;

  (* Return the resolved type of an offset *)
  method resolved_type_of_offset (off: offset) : typ list =
    begin
      (match off with
         | NoOffset -> [];
         | Field(finfo,off') ->
             (match off' with
                | NoOffset ->
                    let retval = ref [] in
                    let resfields = (list_bindings resolved_fields) in
                    for i = 0 to (List.length resfields) - 1 do
                      let (ithtyp1,ithfield,ithtyp2) = (List.nth resfields i) in
                      if (String.compare ithfield.fname finfo.fname) = 0 &&
                        (String.compare (typ_tostring_noattr ithtyp1)
                           (typ_tostring_noattr (TComp(finfo.fcomp,[]))) = 0)
                      then retval := ithtyp2::!retval;
                    done;
                    !retval;
                | _ -> (self#resolved_type_of_offset off');
             );
         | Index(_,off') -> (self#resolved_type_of_offset off');
      );
    end

  (* Returns the resolved type of an lval *)
  method resolved_type_of_lval (lv: lval) : typ list =
    begin
      let (lh,off) = lv in
      let resolvedoffsettype = (self#resolved_type_of_offset off) in
      if (List.length resolvedoffsettype) <> 0
      then resolvedoffsettype
      else begin
        (match lh with
           | Var(v) ->
               let retval = ref [] in
               let resforms = (list_keybindings resolved_formals) in
               for i = 0 to (List.length resforms) - 1 do
                 let (ithfunc,(ithloc,ithvinfo,ithtyp)) = (List.nth resforms i) in
                 if (String.compare ithfunc curr_fundec.svar.vname) = 0 &&
                   (String.compare v.vname ithvinfo.vname) = 0
                 then retval := ithtyp::!retval;
               done;
               !retval;
           | _ -> [];
               (* XXX: Not handling this case Mem(e) right now. This corresponds
                  to an assignment of the form a = *b, where a is a void *. This
                  requires b to be a pointer to a pointer to a type. Can safely
                  ignore these cases right now and worry about them if the need
                  arises. Is safe: if there is such a case, it will show up as
                  an unresolved opaque pointer in our reports. *)
        );
      end
    end

  (* Return the resolved type of an exp. If we cannot resolve it, return the
   * empty list. Only handling a few common cases right now. *)
  method resolved_type_of_exp (e: exp) : typ list =
    begin
      (match e with
         | Lval(lv) -> (self#resolved_type_of_lval lv);
         | CastE(_,e') -> (self#resolved_type_of_exp e');
         | _ -> [];
      );
    end

  (* Analyze an offset and store resolutions *)
  method analysis_offset (off: offset) (t: typ) : unit =
    begin
      (match off with
         | NoOffset -> ();
         | Field(finfo,off') ->
             for i = 0 to (List.length unresolved_fields) - 1 do
               let (ithtyp,ithfield) = (List.nth unresolved_fields i) in
               if ((String.compare ithfield.fname finfo.fname) = 0) &&
                 ((isVoidPtrType t) = false) &&
                 ((isVoidType t) = false)
               then begin
                 (add_if_binding newly_resolved_fields
                    (typ_field_tostring (ithtyp,finfo)) (ithtyp,finfo,t));
               end;
             done;
             (self#analysis_offset off' t);
         | Index(_,off') -> (self#analysis_offset off' t);
      );
    end

  (* Analyze an lval, potentially associating the resolved exp typ with it *)
  method analysis_lval (lv: lval) (t: typ) : unit =
    begin
      let (lh, off) = lv in
      (match lh with
         | Var(v) ->
             for i = 0 to (List.length unresolved_formals) - 1 do
               let (ithfunc,(ithloc,ithvinfo)) = (List.nth unresolved_formals i) in
               if (String.compare ithfunc curr_fundec.svar.vname) = 0 &&
                 (String.compare v.vname ithvinfo.vname) = 0 &&
                 ((isVoidPtrType t) = false) &&
                 ((isVoidType t) = false)
               then begin
                 (add_if_binding newly_resolved_formals
                    curr_fundec.svar.vname (ithloc,v,t));
               end;
             done;
         | _ -> ();
      );
      (* Analyze offsets to check for unresolved field names *)
      (self#analysis_offset off t);
    end

  (* Analyze instructions. This is the key place where all the action happens.
     For each assigment of the form a = b (including cases where a is a formal
     and b is an actual), and a is opaque, we see if we know the type of b. If
     so, we propagate that type to the type of a, and remove a from the set of
     opaques *)
  method analysis_instr (i: instr) : unit =
    begin
      (match i with
         | Set(lv,e,_) ->
             if (isVoidPtrType (typeOfLval lv)) = true
             then begin
               let restyp = (self#resolved_type_of_exp e) in
               for i = 0 to (List.length restyp) - 1 do
                 let ith = (List.nth restyp i) in
                 (self#analysis_lval lv ith);
               done;
             end;
         | Call(lvopt,e,el,_) ->
             (* 1. First check if this is a function with an unresolved param.
                2. If so, find the param that is unresolved, and resolve it using
                the types of the actual *)
             (match e with
                | Lval(Var(fname),_) ->
                    for j = 0 to (List.length unresolved_formals) - 1 do
                      let (jthfunc,(jthloc,jthvinfo)) = (List.nth unresolved_formals j) in
                      if (String.compare jthfunc fname.vname) = 0 (* Match found *)
                      then begin
                        (try
                           let jthloc_actual = (List.nth el jthloc) in
                           let res_jthloc_actual = (self#resolved_type_of_exp jthloc_actual) in
                           for k = 0 to (List.length res_jthloc_actual) - 1 do
                             let kth = (List.nth res_jthloc_actual k) in
                             (add_if_binding newly_resolved_formals fname.vname (jthloc,jthvinfo,kth));
                           done;
                         with Failure("nth") -> ());
                      end;
                    done;
                | _ -> ();
             );
         | _ -> ();
      );
    end

  (* Analyze statement. We only need to analyze instructions *)
  method analysis_stmt (s: stmt) : unit =
    begin
      (match s.skind with
         | Instr(il) -> (ignore (List.map self#analysis_instr il));
         | If(_,tb,fb,_) ->
             (self#analysis_block tb); (self#analysis_block fb);
         | Block(b) -> (self#analysis_block b);
         | Switch(_,b,sl,_) ->
             (self#analysis_block b); (ignore (List.map self#analysis_stmt sl));
         | Loop(b,_,_,_) -> (self#analysis_block b);
         | _ -> ();
      );
    end

  (* Analyze block *)
  method analysis_block (b: block) : unit =
    (ignore (List.map self#analysis_stmt b.bstmts));

  (* Visitor to examine each statement in the program and propagate casts *)
  method vfunc (fdec: fundec) : fundec visitAction =
    begin
      curr_fundec <- fdec;
      (self#analysis_block fdec.sbody);
      DoChildren;
    end

  (* The iterative analysis that resolves opaques *)
  method perform_iterative_analysis (f: file) : unit =
    begin
      (Hashtbl.clear newly_resolved_formals);
      (Hashtbl.clear newly_resolved_fields);
      (visitCilFile (self :> cilVisitor) f);

      (* Remove newly resolved opaques from from unresolved opaques and put them
         in resolved opaques *)
      let kb_formals = (list_keybindings newly_resolved_formals) in
      for i = 0 to (List.length kb_formals) - 1 do
        let (ithfun,(ithloc,ithvinfo,ithtyp)) = (List.nth kb_formals i) in
        (* Add to resolved formals *)
        (add_if_binding resolved_formals ithfun (ithloc,ithvinfo,ithtyp));
        (* Delete from unresolved formals *)
        let newlist = ref [] in
        for j = 0 to (List.length unresolved_formals) - 1 do
          let (jthfun,(jthloc,jthvinfo)) = (List.nth unresolved_formals j) in
          if (String.compare jthfun ithfun) <> 0 || (jthloc <> ithloc)
          then newlist := (jthfun,(jthloc,jthvinfo))::!newlist;
        done;
        unresolved_formals <- !newlist;
      done;

      let kb_fields = (list_keybindings newly_resolved_fields) in
      for i = 0 to (List.length kb_fields) - 1 do
        let (ithfun,(ithtyp1,ithfield,ithtyp2)) = (List.nth kb_fields i) in
        (* Add to resolved fields *)
        (add_if_binding resolved_fields ithfun (ithtyp1,ithfield,ithtyp2));
        (* Delete unresolved fields *)
        let newlist = ref [] in
        for j = 0 to (List.length unresolved_fields) - 1 do
          let (jthtyp,jthfield) = (List.nth unresolved_fields j) in
          let ithstring = (typ_field_tostring (ithtyp1,ithfield)) in
          let jthstring = (typ_field_tostring (jthtyp,jthfield)) in
          if (String.compare ithstring jthstring) <> 0
          then newlist := (jthtyp,jthfield)::!newlist;
        done;
        unresolved_fields <- !newlist;
      done;

      if (Hashtbl.length newly_resolved_fields) <> 0 ||
        (Hashtbl.length newly_resolved_formals) <> 0
      then begin
        (self#perform_iterative_analysis f);
      end
    end

  (* Populate the resolved pointers *)
  method populate_resolved (fields: (string, (typ * fieldinfo * typ)) Hashtbl.t)
    (formals: (string, (int * varinfo * typ)) Hashtbl.t)
    : unit =
    begin
      resolved_fields <- (Hashtbl.copy fields);
      resolved_formals <- (Hashtbl.copy formals);
    end

  (* Populate the unresolved pointers *)
  method populate_unresolved (fields: (typ * fieldinfo) list)
    (formals: (string * (int * varinfo)) list) : unit =
    begin
      unresolved_fields <- fields;
      unresolved_formals <- formals;
    end

  (* Dump information on resolved opaque pointers *)
  method dump_resolved_opaque_pointers () : unit =
    begin
      (Printf.fprintf stderr "Resolved opaque pointers:\n%!");
      (* Resolved struct fields *)
      let all_resolved_fields = (list_keybindings resolved_fields) in
      for i = 0 to (List.length all_resolved_fields) - 1 do
        let ith = (List.nth all_resolved_fields i) in
        let (_,(structtyp,field,restyp)) = ith in
        (Printf.fprintf stderr "\tStruct:%s Field:%s resolved to %s\n"
           (typ_tostring_noattr structtyp) field.fname (typ_tostring_noattr restyp));
      done;
      (* Resolved function parameters *)
      let all_resolved_formals = (list_keybindings resolved_formals) in
      for i = 0 to (List.length all_resolved_formals) - 1 do
        let ith = (List.nth all_resolved_formals i) in
        let (fname,(locinfo,vinfo,restyp)) = ith in
        (Printf.fprintf stderr "\tFunction:%s Parameter:%s resolved to %s\n"
           fname vinfo.vname (typ_tostring_noattr restyp));
      done;
    end

  (* Dump information on unresolved opaque pointers *)
  method dump_unresolved_opaque_pointers () : unit =
    begin
      (Printf.fprintf stderr "Could not resolve these opaque pointers:\n%!");
      (* Unresolved struct fields *)
      for i = 0 to (List.length unresolved_fields) - 1 do
        let (ithcomp,ithfield) = (List.nth unresolved_fields i) in
        (Printf.fprintf stderr "\tStruct:%s Field:%s\n"
           (typ_tostring_noattr ithcomp) ithfield.fname);
      done;
      (* Unresolved function parameters *)
      for i = 0 to (List.length unresolved_formals) - 1 do
        let (ithfun,(ithloc,ithvinfo)) = (List.nth unresolved_formals i) in
        (Printf.fprintf stderr "\tFunction:%s Parameter:%s\n" ithfun ithvinfo.vname);
      done;
    end

  (* Main method to invoke the analysis *)
  method invoke_analysis (f: file)
    (rfields: (string, (typ * fieldinfo * typ)) Hashtbl.t)
    (rformals: (string, (int * varinfo * typ)) Hashtbl.t)
    (ufields: (typ * fieldinfo) list)
    (uformals: (string * (int * varinfo)) list) : unit =
    begin
      (self#populate_resolved rfields rformals);
      (self#populate_unresolved ufields uformals);
      (* Propagate iteratively *)
      (self#perform_iterative_analysis f);
      (* Finally, dump information on resolved and unresolved opaques *)
      (self#dump_resolved_opaque_pointers());
      (self#dump_unresolved_opaque_pointers());
    end
end


(*---------------------------------------------------------------------------*)
(** Step 4: Compute_field_accesses_for_nofundecs.
 *
 * This step computes the field accesses for leaf elements for which fundecs
 * do not exist (and so, we have null field accesses for these functions).
 * We compute the field accesses for these elements by computing the union of
 * all field accesses observed in the points to graph. For functions that have
 * annotations, we use the annotations instead.
 *)
let compute_field_accesses_for_nofundecs (tsort: callnode list)
    (vf: (varinfo, fundec) Hashtbl.t)
    (fa: (string, (typ * fieldinfo * string)) Hashtbl.t)
    (ptgs: (string, ptgraph_t) Hashtbl.t)
    : (string, (typ * fieldinfo * string)) Hashtbl.t =
  begin
    let retval : (string, (typ * fieldinfo * string)) Hashtbl.t =
      (Hashtbl.create 117) in
    let allfas: (string, (typ * fieldinfo * string)) Hashtbl.t =
      (Hashtbl.create 117) in

    (* 1. Compute the union of all field accesses. Remove repeats by inserting
     * into a hashtable*)
    let allaccesses = (list_bindings fa) in
    for i = 0 to (List.length allaccesses) - 1 do
      let ith = (List.nth allaccesses i) in
      let (ithtyp, ithfinfo, ithacc) = ith in
      let ith_str = (typ_tostring_noattr ithtyp) ^ ithfinfo.fname ^ ithacc in
      (add_if allfas ith_str ith);
    done;

    (* 2. Compute the field accesses for functions with no fundecs *)
    for i = 0 to (List.length tsort) - 1 do
      let ith = (List.nth tsort i) in
      let found_fundec = ref false in
      let curr_fundec = ref Cil.dummyFunDec in
      (try
         curr_fundec := (Hashtbl.find vf ith.cnInfo);
         found_fundec := true;
       with Not_found -> (found_fundec := false));
      (* This function has no fundec: a candidate for our analysis *)
      if (!found_fundec = false)  then
        begin
          (* Check if we have modif annotation for this function and use
           * that information, if supplied. Else be conservative and
           * initialize field accesses of this function to be the union
           * of all local field accesses. *)
          let modif_fname =
            (Marshannot_dri.modifies_function_prefix ^ ith.cnInfo.vname) in
          let ptgflat = (get_flattened_ptg1 modif_fname) in
          let annot_fas = (Marshannot_dri.get_fields_accessed_from_modifannot
                             ith.cnInfo vf ptgflat) in
          (match annot_fas with
             | Some(fa_list) ->
                 (infomsg ["modif: Using supplied modif for internal kernel fn: "; ith.cnInfo.vname]);
                 for j = 0 to (List.length fa_list) - 1 do
                   let jth = (List.nth fa_list j) in
                   (add_if_binding retval ith.cnInfo.vname jth);
                 done;
             | None ->
		 if (!do_be_conservative = true) then
		   begin
                     (infomsg ["modif: Using conservative analysis for kernel fn: "; ith.cnInfo.vname]);
        	     let all_field_accesses = (list_bindings allfas) in
        	     for j = 0 to (List.length all_field_accesses) - 1 do
          	       let jth = (List.nth all_field_accesses j) in
          	       (add_if_binding retval ith.cnInfo.vname jth);
        	     done;
		   end
		 else begin
                   (infomsg ["modif: Using non-conservative analysis for kernel fn: "; ith.cnInfo.vname]);
(*
        	   let all_field_accesses = [] in
        	   for j = 0 to (List.length all_field_accesses) - 1 do
          	     let jth = (List.nth all_field_accesses j) in
          	     (add_if_binding retval ith.cnInfo.vname jth);
        	   done;
*)
		 end;
          );
        end else begin
          (* Fundec found for this function. The FAs for this function
           * are as computed before *)
          let fas_curr = (Hashtbl.find_all fa ith.cnInfo.vname) in
          for j = 0 to (List.length fas_curr) - 1 do
            let jth = (List.nth fas_curr j) in
            (add_if_binding retval ith.cnInfo.vname jth);
          done;
        end;
    done;

    retval;
  end


(*---------------------------------------------------------------------------*)
(** Step 5: Build_points_to_graph:
 *
 * This builds our approximation of the points-to graph. It uses types for
 * nodes and edges are labeled with fields. The points to-graph for each
 * procedure is grown separately, and they are merged together based upon the
 * call-graph (i.e., simply merge the graphs for all functions and add
 * information obtained from the current function).
 *
 * Graph creation algorithm: Say we see structure FOO, field BAR being
 * accessed.
 * 1. Create a node of type FOO
 * 2. If type of BAR is a pointer(T), create a node of type T, or else
 *    if BAR is of type T, create a node of type T.
 * 3. Add an edge labeled BAR appropriately to the newly created node.
 *)
let build_points_to_graph (tsort: callnode list)
    (fa: (string, (typ * fieldinfo * string)) Hashtbl.t)
    (rfields: (string, (typ * fieldinfo * typ)) Hashtbl.t)
    : (string, ptgraph_t) Hashtbl.t =
  begin
    let retval : (string, ptgraph_t) Hashtbl.t = (Hashtbl.create 117) in
    (* Iterate through the functions in the driver, in topsort order,
       and build the points-to graph for each function *)
    for i = 0 to (List.length tsort) - 1 do
      let ith = (List.nth tsort i) in
      let fas_curr = (Hashtbl.find_all fa ith.cnInfo.vname) in

      (* 1. Initialize the graph to be the empty graph *)
      let curr_ptg = ref (ptg_empty_graph()) in

      (* 2. Construct the graph of local accesses within this function by
         iterating through structure member accesses in current function
         and building the graph *)
      for j = 0 to (List.length fas_curr) - 1 do
        let jth = (List.nth fas_curr j) in
        let (srctyp,fld,rw) = jth in
        let srcnode = (Ptgnode(srctyp)) in
        curr_ptg := (ptg_add_node srcnode !curr_ptg);
        let fldtyp = fld.ftype in
        (match fldtyp with
           | TPtr(tgttyp,_) ->
               let tgtnode = (Ptgnode(tgttyp)) in
               curr_ptg := (ptg_add_node tgtnode !curr_ptg);
               let newedge = (Ptgedge(srcnode, fld.fname, tgtnode, rw)) in
               curr_ptg := (ptg_add_edge newedge !curr_ptg);
           | _ ->
               let tgtnode = (Ptgnode(fldtyp)) in
               curr_ptg := (ptg_add_node tgtnode !curr_ptg);
               let newedge = (Ptgedge(srcnode, fld.fname, tgtnode, rw)) in
               curr_ptg := (ptg_add_edge newedge !curr_ptg);
        );
      done;

      (* 3. Patch the local accesses graph by making any pointer nodes
         point to the relevant nodes after dereference. Do this after
         figuring out precisely what nodes go into this graph. *)
      (*  TODO: Do we need to do this anymore? *)

      (* 4. Draw graph edges for resolved opaque pointers. Currently only
         patching edges for resolved opaque struct fields. Have not felt the need
         to patch edges for resolved function parameters *)
      (* Suppose that a field 'f' of a 'struct s' has been resolved to
         a 'type p', then we add an edge  s----f---->p [resolved_direct].
         Else, if it was resolved to 'type *p', we add an edge of the form
         s----f---->p [resolved_deref]. *)
      let curr_ptg_edges = (ptg_edgelist !curr_ptg) in
      for j = 0 to (List.length curr_ptg_edges) - 1 do
        let jth = (List.nth curr_ptg_edges j) in
        let Ptgedge(srcnode,fieldname,tgtnode,rw) = jth in
        let Ptgnode(tgttype) = tgtnode in
        let Ptgnode(srctype) = srcnode in
        if (isVoidType tgttype) then
          begin
            let was_resolved = ref false in
            let resolved_fields = (list_keybindings rfields) in
            (* Add edges for resolved fields *)
            for k = 0 to (List.length resolved_fields) - 1 do
              let kth = (List.nth resolved_fields k) in
              let (_,(structtyp,fieldinfo',restyp)) = kth in
              if (String.compare fieldname fieldinfo'.fname) = 0 &&
                (is_same_typ srctype structtyp) (* Source types match? *)
              then begin
                was_resolved := true;
                (match restyp with
                   | TPtr(deref_restyp,_) ->
                       let resolved_tgtnode = (Ptgnode(deref_restyp)) in
                       curr_ptg := (ptg_add_node resolved_tgtnode !curr_ptg);
                       let newedge = (Ptgedge(srcnode,fieldname,resolved_tgtnode,"res_deref")) in
                       curr_ptg := (ptg_add_edge newedge !curr_ptg);
                   | _ ->
                       let resolved_tgtnode = (Ptgnode(restyp)) in
                       curr_ptg := (ptg_add_node resolved_tgtnode !curr_ptg);
                       let newedge = (Ptgedge(srcnode,fieldname,resolved_tgtnode,"res_direct")) in
                       curr_ptg := (ptg_add_edge newedge !curr_ptg);
                )
              end;
            done;
            (* For any field that was resolved, remove the edge to "void" *)
            if (!was_resolved = true)
            then begin
              curr_ptg := (ptg_del_edge jth !curr_ptg);
            end
          end
      done;

      (* 5. Extend the graph interprocedurally, by considering all the graphs
         of callee functions. *)
      let callees = (list_keys ith.cnCallees) in
      for j = 0 to (List.length callees) - 1 do
        let jth = (List.nth callees j) in
        try
          let ptg_jth = (Hashtbl.find retval jth) in
          curr_ptg := (ptg_merge_graphs ptg_jth !curr_ptg);
        with Not_found -> ((* PTG not found for this function *));
      done;

      (* 5. Insert the graph of the current function into the hash table. *)
      if (ptg_numedges !curr_ptg) <> 0
      then (Hashtbl.add retval ith.cnInfo.vname !curr_ptg);

      (* 6. Do the same for the set of globals accessed as well *)
      (* Add the list of globals of the current function *)
      let ithglobs = (Hashtbl.find_all !globals_accessed_intra ith.cnInfo.vname) in
      for j = 0 to (List.length ithglobs) - 1 do
        let jth = (List.nth ithglobs j) in
        Hashtbl.add !summary_globals_accessed ith.cnInfo.vname jth;
      done;
      (* Put the globals of all callees as well *)
      for j = 0 to (List.length callees) - 1 do
        let jth = (List.nth callees j) in
        try
          let globs_jth = Hashtbl.find_all !globals_accessed_intra jth in
          for k = 0 to (List.length globs_jth) - 1 do
            let kth = List.nth globs_jth k in
            Hashtbl.add !summary_globals_accessed ith.cnInfo.vname kth;
          done;
        with Not_found -> ((* Globals not found for this function *))
      done;
    done;

    (Hashtbl.copy retval);
  end


(*---------------------------------------------------------------------------*)
(** Print out the points-to graph *)
let dump_points_to_graph (ptgs: (string, ptgraph_t) Hashtbl.t) =
  begin
    let allfuncs = (list_keys ptgs) in
    for i = 0 to (List.length allfuncs) - 1 do
      let ith = (List.nth allfuncs i) in
      try
        let ptg_ith = (Hashtbl.find ptgs ith) in
        (Printf.fprintf stderr "Function: %s\n%!" ith);
        (ptg_dump ptg_ith ith);
        (Printf.fprintf stderr "\n%!");
      with Not_found -> ();
    done;
  end

(*---------------------------------------------------------------------------*)
let build_combined_points_to_graph (ptgs: (string, ptgraph_t) Hashtbl.t) : ptgraph_t =
  begin
    let retgraph = ref (ptg_empty_graph()) in
    let allptgs = (list_bindings ptgs) in
    for i = 0 to (List.length allptgs) - 1 do
      let ith = (List.nth allptgs i) in
      let edgelist = (Ptgedgeset.elements ith.edges) in
      for j = 0 to (List.length edgelist) - 1 do
        let jthedge = (List.nth edgelist j) in
        let Ptgedge(ndsrc,fld,ndtgt,acc) = jthedge in
        retgraph := (ptg_add_node ndsrc !retgraph);
        retgraph := (ptg_add_node ndtgt !retgraph);
        retgraph := (ptg_add_edge jthedge !retgraph);
      done;
    done;
    !retgraph;
  end

(*---------------------------------------------------------------------------*)
(** Check if element is on stack. If yes, true, if no false *)
let cycle_printed = ref []

let emit_cycle_from_stack (n: string) (s: string Stack.t) : unit =
  begin
    let cycle = ref [] in
    let stackrev = ref [] in
    (try
       while true do
         let top = (Stack.pop s) in
         stackrev := (List.append [top] !stackrev);
         let n_regexp = Str.regexp n in
         if (Str.string_match n_regexp top 0) then cycle := !stackrev;
       done;
     with Stack.Empty -> ());
    for i = 0 to (List.length !stackrev) - 1 do
      let ith = (List.nth !stackrev i) in
      (Stack.push ith s);
    done;
    if (List.length !cycle) <> 0 then
      begin
        let cycle_str = ref "recursive annotation: " in
        for i = 0 to (List.length !cycle) - 1 do
          (* Two extra things we can print if we want:
             - We can optionally print nodes that are already annotated with "recursive"
             - We can optionally print nodes that we've already printed

             In the past we printed everything so you could see the full cycle
             but this results in pretty busy output.
             
             The new approach prints only nodes that are not yet annotated
             and that we've not already printed.
          *)
          let ith = (List.nth !cycle i) in
          let recursive_regexp = Str.regexp ".*(recursive)$" in
          if List.mem ith !cycle_printed = false
            && Str.string_match recursive_regexp ith 0 = false
          then
            begin
              (*Printf.fprintf stderr "Cycle String Added: %s\n" ith;*)
              cycle_printed := !cycle_printed @ [ith];
              cycle_str := !cycle_str ^ " [" ^ ith ^ "]"
            end
              (*
          else
            cycle_str := !cycle_str ^ " [" ^ ith ^ " (already listed)]"
              *)
        done;
        addwarn [!cycle_str ^ "\n"];
      end;
  end
    
(** Detect cycles in the points-to graph *)
let rec do_dfs (n: ptgnode_t) (ptg: ptgraph_t) (s: string Stack.t) : unit =
  begin
    let ntos = (ptgnode_tostring n) in
    (*(Printf.fprintf stderr "%s" ntos);*)
    (try
       let color = (Hashtbl.find dfs_visited ntos) in
       if (String.compare color "gray") = 0 then
         begin
           (emit_cycle_from_stack ntos s);
         end
       else if (String.compare color "black") = 0 then
	 begin
	   (); (* don't do anything *)
	 end
       else begin
         (try
            Hashtbl.replace dfs_visited ntos "gray";
            (*Stack.push ntos s;*)
            (*Printf.fprintf stderr "pushing on stack: %s\n" ntos;*)
            let outgoing_edges = (ptg_get_outgoing_edges n ptg) in
            for i = 0 to (List.length outgoing_edges) - 1 do
              let ithedge = List.nth outgoing_edges i in
              let Ptgedge(start,fld,tgt,_) = ithedge in
              (*Printf.fprintf stderr "pushing on stack2: %s\n" (ntos ^ fld);*)
              let ci_opt = tcomp_compinfo (ptgnode_totyp start) in
              begin
                match ci_opt with
                  | Some(ci) ->
                      begin
                        let fi = get_fieldinfo ci fld in
                        let start_typ = fi.ftype in
                        if (is_recursive start_typ) = true then
                          Stack.push (ntos ^ " -> " ^ fld ^ " (recursive)") s
                        else
                          Stack.push (ntos ^ " -> " ^ fld) s;
                      end
                  | None ->
                      fatal ["marshal cycle detection"];
              end;
              do_dfs tgt ptg s;
              ignore (Stack.pop s);
            done;
            Hashtbl.replace dfs_visited ntos "black";
            (*ignore (Stack.pop s);*)
            (*Printf.fprintf stderr "popping from stack\n";*)
          with Stack.Empty -> ());
       end;
     with Not_found -> ((fatal ["no dfscolor found."])));
  end

(*---------------------------------------------------------------------------*)
let detect_cycles (ptg: ptgraph_t) : unit =
  begin
    let numedges = itoa (List.length (Ptgedgeset.elements ptg.edges)) in
    let numnodes = itoa (List.length (Ptgnodeset.elements ptg.nodes)) in
    addwarn ["CYCLE DETECTION (edges, nodes): "; numedges; numnodes];
    (* (ptg_dump ptg "BIG"); *)

    let nodelist = (Ptgnodeset.elements ptg.nodes) in

    (* Mark all nodes as not visited *)
    for j = 0 to (List.length nodelist) - 1 do
      let jthnode = (List.nth nodelist j) in
      let thestr = (ptgnode_tostring jthnode) in
      (*let thestr2 = (ptgnode_tostring jthnode) in*)
      (Hashtbl.replace dfs_visited thestr "white");
      (*(Printf.fprintf stderr "Type1: %s\nType2: %s\n" thestr thestr2);*)
    done;

    (* Do DFS with a white node as root *)
    let all_nodes_visited = ref false in
    begin
      try
        while (!all_nodes_visited = false) do
    	  (* Get a node that is white in color *)
    	  let rootnode = ref (List.nth nodelist 0) in
	  all_nodes_visited := true;
    	  for i = 0 to (List.length nodelist) - 1 do
      	    let ithnode = (List.nth nodelist i) in
    	    (try
	       let color = (Hashtbl.find dfs_visited (ptgnode_tostring ithnode)) in
	       if (String.compare color "white") = 0 then begin
	         rootnode := ithnode;
	         all_nodes_visited := false;
	       end;
	     with Not_found -> ());
    	  done;
    	  let stk = (Stack.create()) in
    	  (do_dfs !rootnode ptg stk);
        done;
      with Failure("nth") -> ((*Failure of List.nth nodelist 0*));
    end;
    Printf.fprintf stderr "CYCLE DETECTION ";
    (flushwarn ());  
    end
      
(*---------------------------------------------------------------------------*)
(** The main marshalling analysis routine. This calls the other routines     *)
(* Main tasks: resolving the remaining void * pointers *)
(* Determining cycles *)
let marshal_analysis (f: file) : unit =
  begin
    (* Step 1: For each function, determine the roots of the function, pruned
       by usage. Note that we're skipping the main step of determining all the
       initial roots. Also initialize the set of intraprocedural global accesses.
       This will be summarized when we build the points-to graph *)
    infomsg ["prune_roots_analysis starting"];
    let obj_prune : prune_roots_analysis = new prune_roots_analysis in
    obj_prune#invoke_analysis f;
    let field_accesses = obj_prune#get_field_accesses() in
    globals_accessed_intra := Hashtbl.copy (obj_prune#get_global_accesses());

    (* We must now determine the size of the memory block pointed to by each
       of the pointers in the roots *)
    (* Step 2: Resolve opaque pointers using casts contained in the driver code. *)
    infomsg ["resolve_opaques_using_casts starting"];
    let obj_opaque : resolve_opaques_using_casts = new resolve_opaques_using_casts in
    if !do_void_ptr then
      obj_opaque#invoke_analysis f field_accesses;
    let rfields = obj_opaque#get_resolved_fields() in
    let rformals = obj_opaque#get_resolved_formals() in
    let ufields = obj_opaque#get_unresolved_fields() in
    let uformals = obj_opaque#get_unresolved_formals() in

    (* Step 3: Propagate resolved opaques obtained in step 2 *)
    infomsg ["resolve_opaques_using_prop starting"];
    let obj_opaque_prop : resolve_opaques_using_prop = new resolve_opaques_using_prop in
    if !do_void_ptr then
      obj_opaque_prop#invoke_analysis f rfields rformals ufields uformals;
    let prop_rfields = obj_opaque_prop#get_resolved_fields() in
    let prop_rformals = obj_opaque_prop#get_resolved_formals() in
    (*let prop_ufields = (obj_opaque_prop#get_unresolved_fields()) in
      let prop_uformals = (obj_opaque_prop#get_unresolved_formals()) in *)
    (* Set the globals resolved_fields and resolved_formals *)
    resolved_fields := Hashtbl.copy prop_rfields;
    resolved_formals := Hashtbl.copy prop_rformals;

    (* Step 4: Build the points-to graph for each function at the granularity
       of data types. After proagation, get field info for nofundec functions
       and recompute the points-to-graph. *)
    let cgbuild = Cgcomp_dri.callgraph_build f true in
    let tsort = cgbuild.topsort_norec in
    let varinfo_fundec = cgbuild.varinfo_fundec in
    let ptgs = build_points_to_graph tsort field_accesses prop_rfields in
    points_to_graphs := Hashtbl.copy ptgs;
    (* Now compute the field info for no fundec functions and recompute the
     * points to graph *)
    let new_field_accesses = compute_field_accesses_for_nofundecs
                                tsort varinfo_fundec field_accesses ptgs in
    let ptgs = build_points_to_graph tsort new_field_accesses prop_rfields in
    points_to_graphs := Hashtbl.copy ptgs;

    (* Store a copy of the big points to graph to allow us to generate
       the "perform_full_sync" function *)
    let big_ptg = build_combined_points_to_graph !points_to_graphs in
    Hashtbl.add !points_to_graphs "perform_full_sync" big_ptg;

    (* Show the points to graph in the output *)
    (* dump_points_to_graph !points_to_graphs; *)

    (* Step 5: Detect cycles.
     * We currently use this information to determine where to place RECURSE
     * annotations *)
    detect_cycles big_ptg;
    
    (* Optionally, replace all this fancy stuff with the big_ptg.
       Thus, every function will use the same graph for marshaling
       code generation.  Otherwise, keep the specialized graphs.  *)
    if (!do_simple_m_dm = true) then
      begin
        let iterfun (key : 'a) (data : 'b) : unit =
          begin
            Printf.fprintf stderr "Simple marshaling: %s\n" key;
            Hashtbl.replace !points_to_graphs key big_ptg;
          end
        in
        Hashtbl.iter iterfun !points_to_graphs;
      end
  end


(*---------------------------------------------------------------------------*)
(* Main function and other query functions *)
(*---------------------------------------------------------------------------*)

(** Get the set of globals accessed by this function. Also summarizes globals
 * accessed by callee functions. *)
let get_globals_accessed (funcname: string) : varinfo list =
  begin
    (*if String.compare !feature_mode "normal" = 0 then*)
      begin
        let retval = ref [] in
        let globs = (Hashtbl.find_all !summary_globals_accessed funcname) in
        for i = 0 to (List.length globs) - 1 do
          let (ithvinfo, ithrw) = (List.nth globs i) in
          (* Printf.fprintf stderr "global_accessed: %s\n" ithvinfo.vname; *)
          retval := !retval @ [ithvinfo];
          (*(Printf.fprintf stderr "Global accessed is: \t%s (in %s)\n"
            ithvinfo.vname funcname);*)
        done;
        !retval;
      end
   (*    else if String.compare !feature_mode "sym" = 0 then
      []
    else
      fatal ["Unknown feature mode "; !feature_mode];
   *)
  end

(** Get resolved fields: Return the resolved type of each field. If there is
 * more than one resolved type for a field, throw a warning. *)
(* TODO: Sanity checking that fields are resolved only to one type *)
let get_resolved_fields () : (string, (typ * fieldinfo * typ)) Hashtbl.t =
  begin
    (Hashtbl.copy !resolved_fields);
  end

(** Get resolved formals: Return the resolved types of each formal variable *)
(* If there is more than one type resolved for a formal, throw a warning *)
(* TODO: Sanity checking that fields are resolved only to one type *)
let get_resolved_formals () : (string, (int * varinfo * typ)) Hashtbl.t =
  begin
    (Hashtbl.copy !resolved_formals);
  end

(** is_field_addr_taken: Is the field address of the input fieldinfo ever
 * taken in the program? *)
let is_field_addr_taken (f: fieldinfo) : bool =
  begin
    (try
       let f_str = (fieldinfo_name f) in
       (ignore (Hashtbl.find !fields_addrstaken f_str));
       true;
     with Not_found -> (false));
  end

(** Main function to call the marshalling analysis *)
let do_marshal_analysis (f: file) (features : string) : unit =
  begin
    feature_mode := features;
    marshal_analysis f;
  end
