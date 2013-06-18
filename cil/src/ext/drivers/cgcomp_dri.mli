(*===========================================================================*)
(*
 * Device-driver analysis: This file contains code for manipulating and 
 * querying the call-graph.
 *
 * Vinod Ganapathy <vg@cs.wisc.edu>, August 1, 2006.
*)
(*===========================================================================*)

type callnode = {
 	cnInfo: Cil.varinfo; 
 	cnCallees: (string, callnode) Hashtbl.t; 
 	cnCallers: (string, callnode) Hashtbl.t; 
 	sccNums : int ref;
}

type callgraph = (string, callnode) Hashtbl.t

type callgraph_retval = {
	fullcg: callgraph;
	fpta_info: (string, Cil.fundec) Hashtbl.t;
	varinfo_fundec: (Cil.varinfo, Cil.fundec) Hashtbl.t;
	funcs_with_no_fundecs: Cil.varinfo list;
	topsort_norec: callnode list;
}

val callgraph_build: Cil.file -> bool -> callgraph_retval

val fpta_strip_leading_PTR: string -> string

val fpta_tostring_exp_as_lval_type: Cil.exp -> string

val do_function_pointer_analysis: bool ref  
