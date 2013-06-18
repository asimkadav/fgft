(*===========================================================================*)
(*
 * CIL Module for annotating functions in the driver as belonging to 
 * user-space or kernel-space.
 * 
 * This file contains functions to manipulate the cost graph, that we will 
 * use to determine how to annotate functions as kernel or user space
 * functions. The cost graph will be derived from the call-graph but will
 * have associated costs on edges and vertices as well as additional edges
 * 
 * Vinod Ganapathy <vg@cs.wisc.edu>, September 26, 2006.
*)
(*===========================================================================*)

open Cil
open Utils_dri          (* General utilities *)
module E = Errormsg
module H = Hashtbl

(** Nodes: weighted nodes *)
type node_t = 
  | Node of string * int 

(** Node ordering. Don't care about weights *)
module NodeOrder = 
struct
  type t = node_t
  let compare n1 n2 = 
  begin
    let Node(ns1,_) = n1 in
    let Node(ns2,_) = n2 in
    (String.compare ns1 ns2);
  end
end

(** Set of nodes *)
module Nodeset = Set.Make (NodeOrder)

(** Set of nodes: this is exported *)
type nodeset_t = Nodeset.t ref

(** Edges: weighed edges *)
type edge_t = 
  | Edge of node_t * int * node_t

(** Edge ordering. Don't care about weights *)
module EdgeOrder = 
struct
  type t = edge_t 
  let compare e1 e2 =
  begin
    let Edge(src_e1,_,tgt_e1) = e1 in
    let Edge(src_e2,_,tgt_e2) = e2 in
    let srccmp = (NodeOrder.compare src_e1 src_e2) in
    let tgtcmp = (NodeOrder.compare tgt_e1 tgt_e2) in
    if srccmp <> 0 then srccmp else tgtcmp;
  end
end

(** Set of edges *)
module Edgeset = Set.Make (EdgeOrder)

(** Set of edges. Exported *)
type edgeset_t = Edgeset.t ref

(** Cost-Graph *)
type costgraph_t = {nodes: nodeset_t; edges: edgeset_t}


(*-------------------------- <Node queries> ------------------------------*)
(** Adds a node to the graph *)
let add_node (n: node_t) (g: costgraph_t) : unit =
begin
  let curr_nodes = g.nodes in
  g.nodes := (Nodeset.add n !curr_nodes);
end

(** Delete a node from the graph *)
let del_node (n: node_t) (g: costgraph_t) : unit = 
begin
  let curr_nodes = g.nodes in
  g.nodes := (Nodeset.remove n !curr_nodes);
end

(** Is this node present in the graph? Will ignore weights *)
let is_node_present (n: node_t) (g: costgraph_t) : bool =
begin
  let curr_nodes = g.nodes in
  (Nodeset.mem n !curr_nodes);
end

(** Modify the weight of the node *)
let modify_node_weight (n: node_t) (w: int) (g: costgraph_t) : unit =
begin
  if (is_node_present n g) = true then (del_node n g);  
  let Node(ns,_) = n in
  let newnode = Node(ns,w) in
  (add_node newnode g);
end

(** Get the successors of a node in the graph *)
let get_successors_set (n: node_t) (g: costgraph_t) : nodeset_t =
begin
	let Node(n_nm,_) = n in
	let retval = ref (Nodeset.empty) in
	let es = g.edges in 
	let esl = (Edgeset.elements !es) in
	for i = 0 to (List.length esl) - 1 do
		let Edge(src,w,tgt) = (List.nth esl i) in
		let Node(src_nm,_) = src in
		if (String.compare src_nm n_nm) = 0 
		then retval := (Nodeset.add tgt !retval);
	done;
	retval;
end

(** Get the successors of a node in the graph as a list *)
let get_successors_list (n: node_t) (g: costgraph_t) : node_t list =
begin
	let succs_set = (get_successors_set n g) in
	(Nodeset.elements !succs_set);
end
(*-------------------------- </Node queries> ------------------------------*)


(*--------------------------- <Edge queries> ------------------------------*)
(** Adds an edge to the graph *)
let add_edge (e: edge_t) (g: costgraph_t) : unit = 
begin
  let curr_edges = g.edges in 
  g.edges := (Edgeset.add e !curr_edges);
end

(** Delete an edge from the graph *)
let del_edge (e: edge_t) (g: costgraph_t) : unit = 
begin
  let curr_edges = g.edges in
  g.edges := (Edgeset.remove e !curr_edges);
end

(** Is this edge present in the graph? Will ignore weights *)
let is_edge_present (e: edge_t) (g: costgraph_t) : bool = 
begin
  let curr_edges = g.edges in
  (Edgeset.mem e !curr_edges);
end

(** Modify the weight of the edge *)
let modify_edge_weight (e: edge_t) (w: int) (g: costgraph_t) : unit =
begin
  if (is_edge_present e g) = true then (del_edge e g);
  let Edge(src,_,tgt) = e in
  let newedge = Edge(src,w,tgt) in
  if (is_node_present src g) = false then (add_node src g);
  if (is_node_present tgt g) = false then (add_node tgt g);
  (add_edge newedge g);
end

(*-------------------------- </Edge queries> ------------------------------*)


(*-------------------------- <Graph queries> ------------------------------*)
(** Create and empty graph *)
let create_empty_graph () : costgraph_t =
begin
  let ns = ref Nodeset.empty in
  let es = ref Edgeset.empty in
  let g : costgraph_t = {
    nodes = ns;
    edges = es;
  } in g;
end

(** Get all nodes in the graph *)
let get_all_nodes (g: costgraph_t) : node_t list =
begin
	let ns = g.nodes in (Nodeset.elements !ns);
end

(** Get all edges in the graph *)
let get_all_edges (g: costgraph_t) : edge_t list =
begin
	let es = g.edges in (Edgeset.elements !es);
end

(** Print statistics *)
let print_stats (g: costgraph_t) : unit =
begin
  let ns = g.nodes in
  (Printf.fprintf stderr "Costgraph has %d nodes\n%!" (Nodeset.cardinal !ns));
    
  let es = g.edges in
  (Printf.fprintf stderr "Costgraph has %d edges\n%!" (Edgeset.cardinal !es));
end
(*------------------------- </Graph queries> -----------------------------*)


(*------------------------- <Graph algorithms> ---------------------------*)
(* This is where the implementation of the min-cut algorithm should go *)
(*------------------------ </Graph algorithms> ---------------------------*)
