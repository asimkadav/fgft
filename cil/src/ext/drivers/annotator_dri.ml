(*===========================================================================*)
(*
 * CIL Module for annotating functions in the driver as belonging to 
 * user-space or kernel-space.
 * 
 * This is the file where we will have to implement the annotation algorithms
 * (for example, using network flows and so on). This will also require some
 * data-flow analysis, for example, to determine functions that lock the same
 * data structure (which will entail them being on the same side).
 *
 * The output is a file that tells, for each function, whether it is
 * implemented in user-space, or kernel-space.
 *
 * Vinod Ganapathy <vg@cs.wisc.edu>, September 25, 2006.
 *)
(*===========================================================================*)

(* TODO: 
   1. Functions that do not have fundecs must automatically receive "kern"
   annotations 
   2. Ensure that each function has exactly one annotation in the final output.
*)

open Cil
open Scanf
open Utils_dri          (* General utilities *)
open Cgcomp_dri         (* Callgraph-manipulating functions *)
open Costgraph_dri      (* Costgraph-manipulating functions *) 
open Carb

(** Convenient defintions *)
let kern_annot = "kern"

module StringOrder = 
struct
  type t = string
  let compare s1 s2 = (String.compare s1 s2) 
end

module Stringset = Set.Make (StringOrder)

(*---------------------------------------------------------------------------*)
(** Stuff to compute the location of the last line of a function *)

(** Get location of the last line of a function *)
let get_last_line_loc (fdec: fundec) : location = 
  begin
    (try
      let laststmt = (List.hd (List.rev fdec.sbody.bstmts)) in
      let laststmt_loc = ref (get_stmtLoc laststmt.skind) in
      (match laststmt.skind with
      | Instr(ilist) -> 
        if (List.length ilist) <> 0 then 
        laststmt_loc := (get_instrLoc (List.hd (List.rev ilist)));
      | If (_, _, _, _) -> 
        let succlist = (laststmt.succs) in 
        if (List.length succlist) = 1 then begin
          let succ = (List.hd succlist) in 
          laststmt_loc := (get_stmtLoc succ.skind);
        end;
      | Loop(_) -> (warning ["Loop @end of function"; fdec.svar.vname]);
      | Block(_) -> (warning ["Block @end of function"; fdec.svar.vname]);
      | TryFinally(_) -> (warning ["Tryfinally @end of function"; fdec.svar.vname]);
      | TryExcept(_) -> (warning ["TryExcept @end of function"; fdec.svar.vname]);
      | _ -> ();
      );
      !laststmt_loc;
    with Failure("hd") -> (fdec.svar.vdecl));
  end

(*---------------------------------------------------------------------------*)
(** Propagation-based annotation
 *
 * This class contains methods to do a simple forwards propagation of root
 * annotations on the cost-graph. This annotator will be useful when we do
 * not wish to obtain profile information or precise costs.
 *
 * In this propagator, the cost-graph is directly derived from the call-graph
 * and costs on edges and nodes are irrelevant. (Thus the name cost-graph is
 * somewhat misleading). We use the cost-graph as our data-structure for
 * propagation because we may wish to introduce additional constraints
 * (represented by edges) to determine how to do propagation 
 *)
class propagation_based_annotator = object (self)
  (* This is the full call-graph *)
  val mutable fullcallgraph : callgraph = (Hashtbl.create 117);

  (* This is a hashtable storing fundecs. Useful to find which functions
     actually have implementations *)
  val mutable fundecinfo : (varinfo, fundec) Hashtbl.t = (Hashtbl.create 117); 

  (* List of function names that are implemented internally in the driver *)
  val mutable internal_functions : string list = [];
  val mutable internal_fundecs   : fundec list = [];

  (* This hash table stores the length of each function. We only compute and
     store the lengths of functions that are implemented in the device driver. 
     I created two hashtables
  *)
  val mutable fundec_linecounts : (fundec, int) Hashtbl.t = (Hashtbl.create 117);

  (* Color map for output. *)
  val colormap : (string, string) Hashtbl.t = (Hashtbl.create 5)

  (* Each function is annotated with a string of labels. Thus, the annotation
   * data structure is a hashtable of string-sets, with the function name 
   * being the key for hashing *)
  val annotations : (string, Stringset.t ref) Hashtbl.t = (Hashtbl.create 117)

  (* Hashtable to store annotation statistics. Stores "annot" "funcname" map. *)
  val annot_stats : (string, string) Hashtbl.t = (Hashtbl.create 5) 

  (* Hashtbl to store annotation/line count statistics. Store "annot",
     (linecounts, contribfuncs) map. *)
  val annot_loc_stats: (string, (int * int)) Hashtbl.t = (Hashtbl.create 5)

  (* Given a function name, check if the function is an internal driver
   * function -- as determined by whether it has a fundec or not *)
  method is_internal_function (fname: string) : bool = 
    begin
      (List.mem fname internal_functions);
    end

  (* is_a_driver_specific_function: This takes a fundec and decides whether the
   * function is implemented in the driver that we're analyzing, or is part of
   * header files. Heuristic-based. Must be modified based upon specific driver
   * family that we're analyzing. 
	 * 
	 * To override this filter, modify it to always return true;
	*)
  method is_a_driver_specific_function (fdec: fundec) : bool =
    begin
      let retval = ref false in
      let filename = fdec.svar.vdecl.file in
      let prefix1 = "drivers/" in
      let prefix2 = "sound/" in
      let prefixl = prefix1::[prefix2] in
      for i = 0 to (List.length prefixl) - 1 do
        let ith = (List.nth prefixl i) in 
        let ithlen = (String.length ith) in
        let filenamepref = (String.sub filename 0 ithlen) in
        if (String.compare ith filenamepref) = 0 then retval := true;
      done;
      !retval;
    end

  (* is_a_driver_specific_function_fname: This function does exactly the same
   * thing as the previous function, but takes an fname as the argument 
   *
   * To override this filter, modify it to always return true.
   *)
  method is_a_driver_specific_function_fname (fname: string) : bool = 
    begin
      let retval = ref false in
      (* First locate the fundec of this function. Then call the prev function. *)
      let fdecl = (list_bindings fundecinfo) in 
      for i = 0 to (List.length fdecl) - 1 do
        let ith = (List.nth fdecl i) in
        if (String.compare ith.svar.vname fname) = 0 then
        retval := (self#is_a_driver_specific_function ith);
      done;
      !retval;
    end

  (* Add_loc_count. Add the input LOC count to the count maintained for the
   * input label. Add only if the input LOC count is not -1 (i.e., don't know) 
   * Use the function name to perform any filtering during counting 
  *)
  method add_loc_count (fname: string) 
                       (annotlabel: string) 
                       (loc: int) 
                       (contrib: int) : unit = 
    begin
      if (loc <> (0 - 1)) &&
				 (self#is_a_driver_specific_function_fname fname) 
			then begin
        (try
          let (currcount, currcontrib) = (Hashtbl.find annot_loc_stats annotlabel) in 
          let newcount = currcount + loc in 
          let newcontrib = currcontrib + contrib in 
          (Hashtbl.replace annot_loc_stats annotlabel (newcount, newcontrib));
        with Not_found -> (
          (Hashtbl.add annot_loc_stats annotlabel (loc, contrib));
        ));
      end
    end

  (* Get_loc_count: Get the LOC count for a given function name. If not known,
     return -1 *)
  method get_loc_count (fname: string) : int = 
    begin
      let retval = ref (0 - 1) in
      let fundecs = (list_keys fundec_linecounts) in
      for i = 0 to (List.length fundecs) - 1 do
        let ith = (List.nth fundecs i) in
        if (String.compare ith.svar.vname fname) = 0 
        then retval := (Hashtbl.find fundec_linecounts ith);
      done;
      !retval;
    end

  (* Get total LOC: Count the total number of lines of code for all functions
   * in the hashtable fundec_linecounts that do not have a line count of -1.
   * Returns the total line count, as well as the number of functions whose
   * line counts were included in the total*)
  method get_total_loc () : (int * int) = 
    begin
      let allfns = (list_keys fundec_linecounts) in 
      let total = ref 0 in 
      let contributing_funcs = ref 0 in
      for i = 0 to (List.length allfns) - 1 do
        let ith = (List.nth allfns i) in 
        let curr_loc = (Hashtbl.find fundec_linecounts ith) in 
        if (curr_loc <> (0 - 1)) &&
           (self#is_a_driver_specific_function ith)
        then begin
          (Printf.fprintf stderr "LOC: %s %d\n%!" ith.svar.vname curr_loc);
          total := !total + curr_loc;
          contributing_funcs := !contributing_funcs + 1;
        end;
      done;
      (!total, !contributing_funcs);
    end

  (** Populate the annotation set using the root annots from the file 'filename' *)
  (* Empty lines and lines beginning with "#" in the input file are ignored *)
  method populate_annotations (rootannot: (string, Stringset.t ref) Hashtbl.t)
    (filename: string) : unit =
    begin
      let linectr = ref 0 in
      let sanity_check (str: string) : unit =
        begin
          if (str = "") then
            (fatal ["Parse err. No label: "; (Int32.to_string (Int32.of_int !linectr))]);
          if (String.contains str ' ') then 
            (fatal ["Parse err. Improper label "; (Int32.to_string (Int32.of_int !linectr))]);
        end in
      let is_comment (str: string) : bool = 
        begin
          if (str = "") || (String.get str 0) = '#'
          then true else false;
        end in
      let enter_annotation (funname: string) (label: string)=
        begin
          (sanity_check label);
          try
            let labset = (Hashtbl.find rootannot funname) in
            labset := (Stringset.add label !labset);
            (Hashtbl.replace rootannot funname labset);
          with Not_found -> (
            let labset = ref(Stringset.empty) in
            labset := (Stringset.add label !labset);
            (Hashtbl.add rootannot funname labset);
          );
        end in
      if filename = "" then (fatal ["Filename not given\n%!"]);
      (Printf.fprintf stderr "** Reading file %s **\n%!" filename);
      try
        let instream = (open_in_gen [Open_rdonly] 0 filename) in
        try
          while true do
            let currline = (input_line instream) in
            linectr := !linectr + 1;
            if (is_comment currline) = false then
              begin
                try
                  (Scanf.sscanf currline "%s %s" enter_annotation);
                with Scan_failure(s) -> ((warning ["Error populating annotations: "; s]));
              end
          done;
        with End_of_file -> (close_in instream);
      with Sys_error(error) -> ((fatal ["Cannot open annotations file\n%!"]));
    end

  (** Populate the color map. *)
  method populate_colormap (colormap: (string, string) Hashtbl.t)
    (filename: string) : unit =
    begin
      let is_comment (str: string) : bool = 
        begin
          if (str = "") || (String.get str 0) = '#'
          then true else false;
        end in
      let enter_color (label: string) (color: string) = 
        (Hashtbl.add colormap label color);
      in
      if filename = "" then (fatal ["Filename not given\n%!"]);
      (Printf.fprintf stderr "** Reading file %s **\n%!" filename);
      try
        let instream = (open_in_gen [Open_rdonly] 0 filename) in
        try
          while true do
            let currline = (input_line instream) in
            if (is_comment currline) = false then
              begin
                try
                  (Scanf.sscanf currline "%s %s" enter_color);
                with Scan_failure(s) -> ((warning ["Error populating colormap: "; s]));
              end
          done;
        with End_of_file -> (close_in instream);
      with Sys_error(error) -> ((fatal ["Cannot open colormap file\n%!"]));
    end
      
  (** Build the graph on which we will do propagation using the call-graph.
   * Note that this propagator currently uses the call-graph as the cost-graph,
   * but we may wish to add additional edges in the future *)
  method build_cost_graph_from_callgraph (cg: Cgcomp_dri.callgraph) :
    Costgraph_dri.costgraph_t =
    begin
      let costgraph = Costgraph_dri.create_empty_graph() in
      let allnodes_list = (list_keys cg) in
      (* Add the nodes in the call-graph to the cost-graph *)
      for i = 0 to (List.length allnodes_list) - 1 do
        let ith = (List.nth allnodes_list i) in
        let newnode = Node(ith,0) in
        (Costgraph_dri.add_node newnode costgraph);
        (* Add edges to all its callees *)
        let currnode = (Hashtbl.find cg ith) in
        let curr_callees_list = (list_keys currnode.cnCallees) in
        for j = 0 to (List.length curr_callees_list) - 1 do
          let jth = (List.nth curr_callees_list j) in
          let calleenode = Node(jth,0) in
          let newedge = Edge(newnode,0,calleenode) in
          (Costgraph_dri.add_edge newedge costgraph);
        done;
      done;
      (Costgraph_dri.print_stats costgraph);
      costgraph;
    end

  (** propagate_annotations_to_succs: propagates annotations downwards in
   * the cost-graph. *) 
  method propagate_annotations_to_succs (cg: Costgraph_dri.costgraph_t) 
    (annot: (string, Stringset.t ref) Hashtbl.t) : unit =
    begin
      (* wl is the list of nodes whose annotations have to be propagated *) 
      let wl = ref(Stringset.empty) in  
      (* Initialize the worklist with nodes in the initial annotation *) 
      let rootannot = (list_keys annot) in
      for i = 0 to (List.length rootannot) - 1 
      do
          wl := (Stringset.add (List.nth rootannot i) !wl);
      done;
      (* The fixpoint loop that propagates annotations *)
      (* Invariant: Each node in the worklist has at least one annotation *)
      while (Stringset.cardinal !wl) <> 0 
      do
        let currnode_name = (Stringset.choose !wl) in
        (* The following node is guaranteed to be in annot because of the
           invariant above. So this Hashtbl.find WILL NEVER throw an exception *)
        let currannot = (Hashtbl.find annot currnode_name) in 
        wl := (Stringset.remove currnode_name !wl); 
        try
          let currnode_cg = Node(currnode_name,0) in
          let currnode_callees = (Costgraph_dri.get_successors_list currnode_cg cg) in
          for i = 0 to (List.length currnode_callees) - 1
          do
            let newannot = ref(Stringset.empty) in
            let ithcallee = (List.nth currnode_callees i) in
            let Node(ithcallee_name,_) = ithcallee in
            try
              let ithannot = (Hashtbl.find annot ithcallee_name) in
              newannot := (Stringset.union !ithannot !currannot);
              (* If the annotation of ithnode changed, put in worklist *)
              if (Stringset.equal !newannot !ithannot) = false then 
                begin
                  (Hashtbl.replace annot ithcallee_name newannot);   
                  wl := (Stringset.add ithcallee_name !wl);
                end;
            with Not_found -> ( (* This callee does not yet have annotations*)
              (* Initialize annotation, and put the node in the worklist *)
              (Hashtbl.add annot ithcallee_name currannot);   
              wl := (Stringset.add ithcallee_name !wl);
            );
          done;
        with Not_found -> (
          (fatal ["In propagate_annotations_to_succs: ["; currnode_name; "]"; 
                  "Could not find node in the costgraph. Things to check:
                 1. Did you #define static?
                 2. Are the functions in your annotations file implemented in
                    this driver?"]);
        )
      done;
    end

  (** post_process_annotations: Tasks that this function does:
   * 1. Ensure that external functions (non internal_functions) have "kern"
   * annotation 
   *)
  method post_process_annotations 
    (annots: (string, Stringset.t ref) Hashtbl.t) : unit =
    begin
      for i = 0 to (List.length (list_keybindings annots)) - 1 do
        let (ithfn, ithannot) = (List.nth (list_keybindings annots) i) in
        if (self#is_internal_function ithfn) = false then begin
          (* Ensure that it only has the kern annotation *)
          (infomsg ["Making internal function a kern"; ithfn]);
          let labset = ref (Stringset.empty) in
          labset := (Stringset.add kern_annot !labset);
          (Hashtbl.replace annots ithfn labset);
        end;
      done;
    end

  (** Print out the cost-graph with all the annotations *)
  method print_annotations_and_cg (cg: Costgraph_dri.costgraph_t)
    (annot: (string, Stringset.t ref) Hashtbl.t) 
    (filename: string) : unit =
    begin  
      (* Print out annotations for each node. Don't merge annotations *)
      let printAnnots (out: out_channel) (s: string) : unit =
        try
          let labset = (Hashtbl.find annot s) in
          let currloc = (self#get_loc_count s) in 
          (Printf.fprintf out "\t %s [label=<<table border=\"0\" cellborder=\"0\"><tr><td><font color=\"black\">%s (LOC=%d)</font></td></tr>" s s currloc);
          begin
            (Stringset.iter
               (fun str ->
                  let currloc = (self#get_loc_count s) in 
                  if (currloc <> -1) then (self#add_loc_count s str currloc 1);
                  (try
                     let color = (Hashtbl.find colormap str) in
                     (Hashtbl.add annot_stats str s);
                     (Printf.fprintf out "<tr><td bgcolor=\"%s\">%s</td></tr>" color str);
                   with Not_found -> (
                     (Printf.fprintf out "<tr><td bgcolor=\"gray\">%s</td></tr>" str);
                   )))
               !labset);
          end;
          (Printf.fprintf out "</table>>];\n");
        with Not_found -> (
          let no_annotation = "no_annotation" in 
          (Hashtbl.add annot_stats no_annotation s);
          let currloc = (self#get_loc_count s) in 
          if (currloc <> -1) then (self#add_loc_count s no_annotation currloc 1);
          (Printf.fprintf out "\t %s [label=\"%s (LOC=%d)\"];\n" s s currloc);
        )
      in
      try
        let numnodes = ref 0 in
        let external_funcs = ref 0 in
        let outstream = (open_out filename) in
        (Printf.fprintf outstream "digraph G\n {\n");
        (Printf.fprintf outstream "\t ratio=\"fill\";\n");
        (Printf.fprintf outstream "\t size=\"8,10\";\n");
        let nodeslist = (Costgraph_dri.get_all_nodes cg) in
        for i = 0 to (List.length nodeslist) - 1 do
          let Node(ith,_) = (List.nth nodeslist i) in
          (* Only account for internal driver functions *)
          if (self#is_internal_function ith) 
          then begin
            (printAnnots outstream ith);  
            numnodes := !numnodes + 1;
          end
          else begin
            external_funcs := !external_funcs + 1;
          end
        done;
        let edgeslist = (Costgraph_dri.get_all_edges cg) in
        for i = 0 to (List.length edgeslist) - 1 do
          let Edge(src,_,tgt) = (List.nth edgeslist i) in
          let Node(src_name,_) = src in
          let Node(tgt_name,_) = tgt in
          if (self#is_internal_function src_name) && 
            (self#is_internal_function tgt_name)
          then (Printf.fprintf outstream "\t %s -> %s;\n" src_name tgt_name);
        done;
        (* Print out the color map and the statistics in the dotty file*)
        (* numnodes is the total number of internal driver functions, including
         * those obtained from headers. total_loc is the total number of lines
         * of code. contributing functions is the total number of functions
         * that contributed to this line count. Note contributing_funcs <=
         * numnodes *) 
        let (total_loc, contributing_funcs) = (self#get_total_loc()) in 
        (Printf.fprintf outstream "label = <<table border=\"0\" cellborder=\"1\">\n");
        (Printf.fprintf outstream 
          "<tr><td>STATS - total functions: %d</td></tr>\n" contributing_funcs);
        (Printf.fprintf outstream 
          "<tr><td>STATS - total_LOC: %d</td></tr>\n" total_loc);
        (Printf.fprintf stderr "STATS: total=%d" contributing_funcs);
        (Printf.fprintf stderr " total_LOC=%d" total_loc);
        let allannots = (remove_repeats (list_keys annot_stats)) in
        for i = 0 to (List.length allannots) - 1 do
          let ith = (List.nth allannots i) in
          (* funcs is the list of all the functions with the ith annotation *)
          (* let funcs = (Hashtbl.find_all annot_stats ith) in *)
          (* Get the LOC total of all these functions from annot_loc_stats.
           * Note that the LOC total may be subject to filtering, based upon
           * how we computed the totals. *)
          let funcs_loc = ref (0 - 1) in
          let contrib_funcs = ref (0 - 1) in
          (try
            let (loc, contrib) = (Hashtbl.find annot_loc_stats ith) in
            funcs_loc := loc;
            contrib_funcs := contrib;
          with Not_found -> ());
          (try
             let color = (Hashtbl.find colormap ith) in
             (Printf.fprintf outstream 
                 "<tr><td bgcolor=\"%s\">STATS - %s: %d</td></tr>\n" 
                 color ith !contrib_funcs);
             (Printf.fprintf outstream 
                 "<tr><td bgcolor=\"%s\">STATS - %s_LOC: %d</td></tr>\n" 
                 color ith !funcs_loc);
             (Printf.fprintf stderr " %s=%d" ith !contrib_funcs);
             (Printf.fprintf stderr " %s_LOC=%d" ith !funcs_loc);
           with Not_found -> (
             (Printf.fprintf outstream 
                "<tr><td bgcolor=\"gray\">STATS - %s: %d</td></tr>\n" 
                 ith !contrib_funcs);
             (Printf.fprintf outstream 
                "<tr><td bgcolor=\"gray\">STATS - %s_LOC: %d</td></tr>\n" 
                 ith !funcs_loc);
             (Printf.fprintf stderr " %s=%d" ith !contrib_funcs);
             (Printf.fprintf stderr " %s_LOC=%d" ith !funcs_loc);
           ));
        done;
        (Printf.fprintf stderr " kern_funcs=%d" !external_funcs);
        (Printf.fprintf stderr " kern_funcs_LOC=-1\n%!");
        (Printf.fprintf outstream "<tr><td>STATS - kern_funcs: %d</td></tr>\n" !external_funcs);
        (Printf.fprintf outstream "<tr><td>STATS - kern_funcs_LOC: -1</td></tr>\n");
        (Printf.fprintf outstream "</table>>\n");
    (*(Printf.fprintf outstream "</table>>\n");*)
        (Printf.fprintf outstream "}\n");
        close_out(outstream);
      with Sys_error(error) -> ((fatal ["Output file not found:\n%!"; filename]));
    end

  (** Print out all the annotations *)
  method print_annotations (cg: Costgraph_dri.costgraph_t) 
    (annot: (string, Stringset.t ref) Hashtbl.t) 
    (filename: string) : unit =
    begin  
      let printAnnots (out: out_channel) (s: string) : unit = 
        try
          let labset = (Hashtbl.find annot s) in 
          (Printf.fprintf out "%s " s);
          (* POLICY DECISION: > 1 annotation => Put kern annotation *)
          (* OLD (Stringset.iter (fun str -> (Printf.fprintf out " %s" str)) !labset); *)
          if (Stringset.cardinal !labset) = 1 
          then 
            (Stringset.iter (fun str -> (Printf.fprintf out " %s" str)) !labset)
          else
            (Printf.fprintf out " kern");
          (Printf.fprintf out "\n%!");
        with Not_found ->
          begin
            (* POLICY DECISION: No annotation on function => if it's an
               internal driver function, put in user space.  If it's not internal,
               put it in kernel, since we obviously couldn't move it to user-space
               if we wanted to.
            
            if (self#is_internal_function s) = false then
              (Printf.fprintf out "%s kern\n" s)
            else
              (Printf.fprintf out "%s user\n" s);
            *)

            if (is_risky_function s) = true then 
                (Printf.fprintf out "%s user\n" s)
            
            else
                (Printf.fprintf out "%s kern\n" s);
            
            (* Asim - modified to put everything in core kernel 
             Printf.fprintf out "%s kern\n" s; *)
          end
      in
      try
        let outstream = (open_out filename) in
        let nodeslist = (Costgraph_dri.get_all_nodes cg) in
        for i = 0 to (List.length nodeslist) - 1 do
          let Node(ith,_) = (List.nth nodeslist i) in
          (printAnnots outstream ith);  
        done;
        close_out(outstream);
      with Sys_error(error) -> ((fatal ["Output file not found:\n%!"; filename]));
    end

  (* Take a list of fundecs, and output line counts for each of them. Line
   * count is simply the line number of the last line - that of the first 
   * line.
  *)
  method calculate_linecounts (flist: fundec list) : unit = 
  begin
    (* Initialize all values to unknown *)
    for i = 0 to (List.length flist) - 1 do
      let ith = (List.nth flist i) in 
      (add_if fundec_linecounts ith (0 - 1));
    done;
    (* Last-line location - first line location *)
    for i = 0 to (List.length flist) - 1 do
      let currfdec = (List.nth flist i) in 
      let lastlineloc = (get_last_line_loc currfdec) in 
      if ((currfdec.svar.vdecl.line) <> (0 - 1)) &&
         (lastlineloc.line <> (0 -1))
      then begin
        let currlinecount = (lastlineloc.line - currfdec.svar.vdecl.line + 1) in 
        (Hashtbl.replace fundec_linecounts currfdec currlinecount);
      end;
    done;
  end

  (** propannot_toplevel: Top-level function of propagation_based_annotator *)
  method propannot_toplevel (f : file) 
    (rootannot_file: string) 
    (color_file: string)
    (dotty_file: string) 
    (annot_output: string) : unit =
    begin
      let cgbuild = (Cgcomp_dri.callgraph_build f false) in
      fullcallgraph  <- cgbuild.fullcg;
      fundecinfo <- cgbuild.varinfo_fundec; 

      (* Only functions that have an implementation will have fundecs *)
      for i = 0 to (List.length (list_keys fundecinfo)) - 1 do
        let ith = (List.nth (list_keys fundecinfo) i) in
        internal_functions <- ith.vname::internal_functions;
        let ithfdec = (Hashtbl.find fundecinfo ith) in 
        internal_fundecs <- ithfdec::internal_fundecs;
      done;
      
      (* Calculate line counts for driver functions *)  
      (self#calculate_linecounts internal_fundecs);

      (* Perform annotation propagation *)
      (self#populate_annotations annotations rootannot_file);
      (self#populate_colormap colormap color_file);
      let costgraph = (self#build_cost_graph_from_callgraph fullcallgraph) in
      (self#propagate_annotations_to_succs costgraph annotations);
      (self#post_process_annotations annotations);
      (self#print_annotations_and_cg costgraph annotations dotty_file);
      (self#print_annotations costgraph annotations annot_output);
    end
end

(*---------------------------------------------------------------------------*)
(** Network-flows-based annotation.
 * TODO: Implement min-cut algorithm on the cost-graph to figure out the
 * optimal cut.
 *)
class cost_based_annotator = object (self)
  method costannot_toplevel (f: file) : unit = 
    begin
      (fatal ["Not yet implemented"]);
    end
end


(*---------------------------------------------------------------------------*)
(** Main function to call the annotator. This is the one that determines which
 * annotation algorithm will be used *)
let do_annotation (f: file)
    (rootannot_file: string) 
    (color_file: string) 
    (dotty_file: string) 
    (annot_output: string) : unit =
  begin
    ( dobeefyfind f); (* ASIM  *)

    let obj : propagation_based_annotator = (new propagation_based_annotator) in
    (obj#propannot_toplevel f rootannot_file color_file dotty_file annot_output);
    
  (* Invocations of other annotation algorithms must go here *)
  end

(* Take a list of fundecs, and output line counts for each of them. For the
   * last function in each file, output 1 as the linecount. Assumption is that
   * this function will be used with a dummy function at the end of each file
  *)
(*
  method calculate_linecounts (flist: fundec list) : unit = 
  begin
    let sortfn (fdec1: fundec) (fdec2: fundec) : int = 
    begin
      let filenmcmp = (String.compare fdec1.svar.vdecl.file
                                      fdec2.svar.vdecl.file) in
      if (filenmcmp = 0) 
      then (fdec1.svar.vdecl.line - fdec2.svar.vdecl.line)
      else filenmcmp;
    end in
    let sortedflist = (List.sort sortfn flist) in 
    (* Initialize all values to unknown *)
    for i = 0 to (List.length sortedflist) - 1 do
      let ith = (List.nth sortedflist i) in 
      (add_if fundec_linecounts ith (0 - 1));
    done;
    (* If you have only one function, you can't determine its length.
       Therefore, the loop below assumes at least 2 elements *)
    for i = 1 to (List.length sortedflist) - 1 do
      let pred = (i - 1) in 
      let predfdec = (List.nth sortedflist pred) in 
      let currfdec = (List.nth sortedflist i) in 
      if ((currfdec.svar.vdecl.line) <> -1) &&
         ((predfdec.svar.vdecl.line) <> -1)
      then begin
        if ((String.compare (predfdec.svar.vdecl.file)
                            (currfdec.svar.vdecl.file)) = 0) then
        begin
          let predlinecount = 
            (currfdec.svar.vdecl.line - predfdec.svar.vdecl.line) in
          (Hashtbl.replace fundec_linecounts predfdec predlinecount);
        end;
      end;
    done;
  end
*)

