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

(** Nodes: weighted nodes *)
type node_t = 
  | Node of string * int 

(** Edges: weighed edges *)
type edge_t = 
  | Edge of node_t * int * node_t

(** Set of nodes *)
type nodeset_t 

(** Set of edges *)
type edgeset_t

(** Cost-Graph *)
type costgraph_t = {nodes: nodeset_t; edges: edgeset_t}

(** Node queries *)
val add_node: node_t -> costgraph_t -> unit
val del_node: node_t -> costgraph_t -> unit
val is_node_present: node_t -> costgraph_t -> bool
val modify_node_weight: node_t -> int -> costgraph_t -> unit
val get_successors_set: node_t -> costgraph_t -> nodeset_t
val get_successors_list: node_t -> costgraph_t -> node_t list

(** Edge queries *)
val add_edge: edge_t -> costgraph_t -> unit
val del_edge: edge_t -> costgraph_t -> unit
val is_edge_present: edge_t -> costgraph_t -> bool
val modify_edge_weight: edge_t -> int -> costgraph_t -> unit

(** Graph queries *)
val print_stats: costgraph_t -> unit
val create_empty_graph: unit -> costgraph_t
val get_all_nodes: costgraph_t -> node_t list
val get_all_edges: costgraph_t -> edge_t list
