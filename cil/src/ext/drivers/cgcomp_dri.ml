(*===========================================================================*)
(*
 * Device-driver analysis: This file contains code for manipulating and 
 * querying the call-graph. Also contains a simple type-based function 
 * pointer analysis.
 *
 * Vinod Ganapathy <vg@cs.wisc.edu>, August 1, 2006.
 *)
(*===========================================================================*)

open Cil
open Utils_dri              (* General utilities *)

let do_function_pointer_analysis = ref false

(** Prettyprinter for a type. This is used for diagnostics, and also for
 * matching. Will currently punt if we have anonunions and anonstructs *)
(* NOTE: Any change below in the string representation of types will have 
   to be reflected in fpta_strip_leading_PTR below *)
(* NOTE: Results of function pointer analysis will be imprecise if the address
   of a vararg function is taken. *)
let rec fpta_tostring_typ (t: typ) : string =
  begin
    (match t with
       | TVoid(_) -> "void";
       | TInt(_) -> "int";
       | TFloat(_) -> "float";
       | TPtr(t',_) -> "PTR#" ^ (fpta_tostring_typ t');
       | TArray(t',_,_) -> "ARR#" ^ (fpta_tostring_typ t');
       | TNamed(tinfo,_) -> (fpta_tostring_typ tinfo.ttype); (* "TINFO#" ^ tinfo.tname; *)
       | TComp(cinfo,_) -> 
           if cinfo.cstruct 
           then "STRUCT#" ^ cinfo.cname
           else "UNION#" ^ cinfo.cname;
       | TFun(t1,alopt,_,_) ->
           let retval = ref("FUNC#RET=" ^ (fpta_tostring_typ t1) ^ "#ARGS=") in
           (match alopt with 
              | None -> ();
              | Some(al) ->
                  for j = 0 to (List.length al) - 1
                  do
                    let curname,curtype,curattr = (List.nth al j) in
                    retval := !retval ^ "_" ^ (fpta_tostring_typ curtype);
                  done;
           );
           !retval;
       | TEnum(einfo,_) -> "EINFO#" ^ einfo.ename;
       | TBuiltin_va_list(_) -> "builtin_va_list";
    );
  end


(** Consider LVALS that are EXPRS and get their types *)
let rec fpta_tostring_exp_as_lval_type (e: exp) : string =
  begin
    (* Print out the type of an LVAL. If an offset is present, then
       the type of the offset is printed, i.e., lowest level *)
    let rec fpta_tostring_lval_type (lv: lval) : string =
      begin
        let rec fpta_tostring_offset_type (off: offset) : string = 
          begin
            (match off with
               | Field(finfo, NoOffset) -> (fpta_tostring_typ finfo.ftype);
               | Field(_,off') -> (fpta_tostring_offset_type off');
               | Index(e', NoOffset) -> (fpta_tostring_exp_as_lval_type e');
               | Index(_,off') -> (fpta_tostring_offset_type off');
               | _ -> "unrecognized_field_kind";
            );
          end in
        let (lh, off) = lv in  
        (match lh, off with
           | (Var(vi), NoOffset) -> (fpta_tostring_typ vi.vtype);
           | (Mem(e), NoOffset) -> (fpta_tostring_exp_as_lval_type e);
           | (_, off') -> (fpta_tostring_offset_type off')
        ); 
      end in
    (match e with
       | Lval(lv) -> (fpta_tostring_lval_type lv);
       | AddrOf(lv) -> (fpta_tostring_lval_type lv);
       | StartOf(lv) -> (fpta_tostring_lval_type lv);
       | CastE(t,e') -> (fpta_tostring_typ t);
       | SizeOf(_) -> "int";
       | SizeOfE(_) -> "int";
       | SizeOfStr(_) -> "int";
       | AlignOf(t') -> (fpta_tostring_typ t');
       | UnOp(_,_,t') -> (fpta_tostring_typ t');
       | BinOp(_,_,_,t') -> (fpta_tostring_typ t');
       | Const(c) -> 
           (match c with
              | CInt64(_) -> "int"; 
              | CStr(_) -> "PTR#char";
              | CChr(_) -> "char";
              | CWStr(_) -> "PTR#char";
              | CReal(_) -> "float";
              | CEnum(_) -> "";
           );
       | _ -> "type_expr_not_an_lval"
    );
  end


(* Strip the leading "PTR#" from an input string *)
(* FIXME: Currently hacked up. Both the warnings in this function must 
 * be fatal. However, the return string, non_ptr_type, will ensure that
 * the string returned by this function will not have any matches.
 *)
let fpta_strip_leading_PTR (s: string) : string = 
  begin
    if (String.length s) < 4 
    then begin
      (warning ["String of length less than 4"; s]);
      "non_ptr_type" 
    end
    else if (String.compare (String.sub s 0 4) "PTR#") <> 0
    then begin
      (warning ["String does not have leading PTR#"; s]);
      "non_ptr_type" 
    end
    else (String.sub s 4 ((String.length s) - 4));
  end


(** DIAGNOSTICS ONLY *)
let rec fpta_tostring_exp_as_lval (e: exp) : string =
  begin
    let rec tostring_lval (lv: lval) : string =
      begin
        let rec tostring_offset (off: offset) : string = 
          begin
            (match off with
               | Field(finfo, NoOffset) -> finfo.fname;
               | Field(finfo, off') -> finfo.fname ^ "->" ^ (tostring_offset off');
               | _ -> "unrecognized_field_kind"
            );
          end in
        let (lh, off) = lv in  
        (match lh, off with
           | (Var(vi), NoOffset) -> vi.vname;
           | (Mem(e), NoOffset) -> (fpta_tostring_exp_as_lval e);
           | (Var(vi), off') -> vi.vname ^ "->" ^ (tostring_offset off')
           | (Mem(e), off') -> (fpta_tostring_exp_as_lval e) ^ "->" ^ (tostring_offset off')
        ); 
      end in
    (match e with
       | Lval(lv) -> (tostring_lval lv);
       | AddrOf(lv) -> "ADDROF_" ^ (tostring_lval lv);
       | StartOf(lv) -> "STARTOF_" ^ (tostring_lval lv);
       | _ -> "expr_not_an_lval");
  end


(** Information for type-based function pointer analysis. This is a global
 * variable, which is set by the class typebased_fpta. This so that this
 * information can be exploited during callgraph construction*)
let fpta_addrtaken: (string, fundec) Hashtbl.t = (Hashtbl.create 11);


(*===========================================================================*)
(** A simple type-based function pointer analysis. This class collects the
 * info used in the actual analysis. Use of function pointer targets is in
 * two places: once during callgraph construction, and a second time when
 * grammars are extracted and processed.
 *
 * To switch off function pointer analysis, do not populate fpta_addrtaken.
 *) 
class typebased_fpta = object (self)
  inherit nopCilVisitor

  (** Get the functions grouped by type *)
  method get_funcs_groupbytype () = fpta_addrtaken;
    
  (** Gather the set of functions whose address was taken *)
  method vfunc (fdec: fundec) : fundec visitAction =
    begin
      if (fdec.svar.vaddrof = true) && (!do_function_pointer_analysis = true) then 
        begin
          let type_of_func = fpta_tostring_typ fdec.svar.vtype in
          Hashtbl.add fpta_addrtaken type_of_func fdec
        end;
      DoChildren
    end;
end


(*===========================================================================*)
(* Call-graph, and related data structures and functions.                    *)
(* In addition, also a mapping from varinfo to fundec, where applicable.     *)
(*                                                                           *)
(* Employs a simple type-based function pointer analysis to obtain the call  *)
(* graph in the presence of function pointers.                               *)
(*                                                                           *)
(* Algorithm:                                                                *)
(* (1) Get the names and types of all functions whose addresses are taken.   *)
(* (2) Get the names and types of all function pointers.                     *)
(* (3) Match based upon type.                                                *)
(* Note that this is unsound in the presence of casts.                       *)
(*===========================================================================*)
(* Each node in the call-graph *)
type callnode = {
  (* the function the node describes. This must be a varinfo, because not
     all functions will have function declarations, example: function ptrs,
     and library functions without function bodies. *)
  cnInfo: varinfo; 
  (* the set of functions that this one calls *)
  cnCallees: (string, callnode) Hashtbl.t; 
  (* the set of functions that call this one *)
  cnCallers: (string, callnode) Hashtbl.t; 
  (* SCC numbers - strongly connected component *)
  sccNums : int ref;
}


(** The call-graph itself is a hash table of callnodes *)
type callgraph = (string, callnode) Hashtbl.t

(*===========================================================================*)
(** Traverse program text and populate the call-graph hashtable *)
class callgraphcomputer = object (self)
  inherit nopCilVisitor

  (** The call-graph as a hashtable *)
  val graphtbl: callgraph = (Hashtbl.create 117);

  (** The current function we're in *)
  val mutable curFunc: callnode option = None;

  (** DFS time *)
  val mutable dfsTime: int = 0;

  (** Current SCC Number *)
  val mutable currsccnum: int = 0;

  (** Does the call structure contain recursion? 
   * If there is no recursion, the value is 0. If there are self loops only,
   * the value is 1, and if there is non-trivial recursion, the value is 2,
   * and if both, the value is set to 3. *)
  val mutable rec_cntr: int = 0;

  (** Depth-first search related markings *)
  val starttime: (callnode, int) Hashtbl.t  = (Hashtbl.create 117);
  val endtime: (callnode, int) Hashtbl.t = (Hashtbl.create 117);
  val dfsstatus: (callnode, int) Hashtbl.t = (Hashtbl.create 117);

  (** SCC related markings *)
  val sccnums: (callnode, int) Hashtbl.t = (Hashtbl.create 117);

  (** A mapping from varinfo to fundecs *)
  val varinfo_fundec : (varinfo, fundec) Hashtbl.t = (Hashtbl.create 117);

  (** Set of resolutions *)
  val fptr_resolutions : (string, bool) Hashtbl.t = (Hashtbl.create 5);

  (** Print out the resolutions *)
  method print_fptr_resolutions () : unit = 
    begin
      for i = 0 to (List.length (list_keys fptr_resolutions)) - 1 do
	let ith = (List.nth (list_keys fptr_resolutions) i) in
	(infomsg [ith]);
      done;
    end
      
  (** getGraph: Return the graph *)
  method getGraph (): callgraph = graphtbl;

  (** get_varinfo_fundec:  Return the varinfo_fundec mapping *)
  method get_varinfo_fundec () : (varinfo, fundec) Hashtbl.t = varinfo_fundec;

  (** get_functions_with_no_fundecs: Return the list of varinfos of functions
   * for which we could not find fundecs. These are computed as follows. We
   * traverse the call-graph and look for leaves, as those are the only
   * functions that potentially do not have fundecs. We then check to see that
   * they have entries in varinfo_fundec. If not, we add them as functions with
   * no fundecs. *)
  method get_functions_with_no_fundecs () : varinfo list = 
    begin
      let retval = ref [] in 
      let nodes = (list_bindings graphtbl) in
      for i = 0 to (List.length nodes) - 1 do
	let ith = (List.nth nodes i) in
	if (Hashtbl.length ith.cnCallees) = 0 then begin
	  (try
	     (ignore (Hashtbl.find varinfo_fundec ith.cnInfo));
	   with Not_found -> (retval := (List.append !retval [ith.cnInfo])));
	end;
      done;
      !retval;
    end


  (** Given a function's varinfo, retrieve its callnode. Create an
   * entry if one does not already exist *)
  method getNode (vinfo : varinfo) : callnode = 
    begin
      let name = vinfo.vname in 
      try 
        (Hashtbl.find graphtbl name)
      with Not_found -> (
        (* make a new node *)
        let ret : callnode = {
          cnInfo = vinfo;
          cnCallees = (Hashtbl.create 5);
          cnCallers = (Hashtbl.create 5);
          sccNums = ref 0;
        } in
        (Hashtbl.add graphtbl name ret);
        ret
      )
    end

  (** Overload vfunc. Set curFunc. Also populate the varinfo_fundec mapping   *)
  method vfunc (fdec : fundec) : fundec visitAction = 
    begin
      curFunc <- (Some (self#getNode fdec.svar));
      (add_if varinfo_fundec fdec.svar fdec);
      DoChildren
    end

  (** Overload vinst to build the call-graph by noting caller-callee pairs.
   * Handling calls via function pointers using a simple type-based points-to
   * analysis algorithm *)
  method vinst (i : instr) : instr list visitAction =
    begin
      (match curFunc, i with
         | Some(caller), Call(retlv,expression,arglist,_) -> (* ignore function pointers *)
             begin
               (match expression with
                  | Lval(Var(vi), NoOffset) ->
                      (
                        let callee : callnode = self#getNode vi in
                        add_if caller.cnCallees callee.cnInfo.vname callee;
                        add_if callee.cnCallers caller.cnInfo.vname caller
                      );
                  | Lval(Mem(e'), NoOffset) ->
                      begin
                        let typeinfo = fpta_tostring_exp_as_lval_type e' in
                        (* NOTE: Any change below must be reflected in the tostring functions *)
                        (* typeinfo will be a string with a leading "PTR#". We must strip
                           that before we search the hashtable for matching functions *)
                        try
                          (* The following strips the leading "PTR#" that is in the type of
                             the function pointer e'. PTR# is added by
                             fpta_tostring_exp_as_lval *)
                          let strptypeinfo = fpta_strip_leading_PTR typeinfo in
                          let ptlist = Hashtbl.find_all fpta_addrtaken strptypeinfo in
                          for j = 0 to (List.length ptlist) - 1
                          do
                            let jth = List.nth ptlist j in
                            let callee : callnode = self#getNode jth.svar in 
                            add_if caller.cnCallees callee.cnInfo.vname callee;
                            add_if callee.cnCallers caller.cnInfo.vname caller;
			    let resolution = "FUNC PTR.: Var/Target: " ^
                              (fpta_tostring_exp_as_lval e') ^ "/" ^ callee.cnInfo.vname in
			    add_if fptr_resolutions resolution true;
                          done;
                        with Invalid_argument(_) -> (
                          (* Function pointer without leading PTR# in type. Currently unhandled *)
                          (warning ["Wierd function pointer";typeinfo])
                        );
                      end
                  | Lval(_, offset) -> warning ["Call instr/offset. Unhandled POINTS-TO"];
                  | _ -> warning ["Call not an LVAL. Unhandled POINTS-TO"]);
             end
         | _ -> ((* Instruction is not a function call *))
      ); 
      DoChildren
    end

  (** Print the call-graph *)
  method printgraph (out:out_channel) : unit = 
    begin
      let printEntry (s: string) (n: callnode) : unit = 
        (Printf.fprintf out "        %s\n" s) in
      let printNode (s: string) (n: callnode) : unit = 
        begin
          (Printf.fprintf out "%s ->\n%!" s);
          (Hashtbl.iter printEntry n.cnCallees);
          (Printf.fprintf out "\n%!");
        end in
      (Hashtbl.iter printNode graphtbl)
    end

  (** Print the call-graph in dotty format *)
  method printgraph_dotty (out:out_channel) : unit =
    begin
      let printEntry (caller: string) (s: string) (n: callnode) : unit =
        (Printf.fprintf out "%s -> %s;\n" caller s) 
      in
      let printNode (s: string) (n: callnode) : unit =
        (Hashtbl.iter (printEntry s) n.cnCallees);
      in
      (Hashtbl.iter 
         (fun s n -> (Printf.fprintf out "%s [label = \"%s\"];\n" s s))
         graphtbl);
      (Hashtbl.iter printNode graphtbl);
    end

  (* Added this to make it very clear which functions could ultimately be called
     by each root function.  If unique is true, then the entire call graph for each
     function is NOT shown:  only the first copy of a function is included.  This
     mode is helpful to see if it's POSSIBLE for one function to call another.
     If unique is false, then the entire call graph for the function is printed.
     This may include many copies of common functions like "readl" and "Writel", 
     since many functions call these. *)
  method printgraph_transitive (out:out_channel) (unique: bool) : unit =
    begin
      let depth = ref 0 in
      let alreadyDone = Hashtbl.create 117 in 
      let rec printEntry (callee: string) (n: callnode) : unit =
        let str : string = (String.make !depth ' ') in
        if ((unique = false) || ((unique = true) && (Hashtbl.mem alreadyDone callee) = false)) then
          begin
            (* Only print it if:
               1) unique is false, or
               2) unique is true, and we've not seen it before.
            *)
            (Printf.fprintf out "%s%s => \n" str callee);
            (Hashtbl.add alreadyDone callee callee);
            depth := !depth + 2;
            (Hashtbl.iter printEntry n.cnCallees);
            depth := !depth - 2;
          end;
      in
      let printFullEntry (callee : string) (n: callnode) : unit =
        (Hashtbl.clear alreadyDone);
        (printEntry callee n);
      in
      (Hashtbl.iter printFullEntry graphtbl);
    end

  (** DFS related routines: the following routine obtains start 
   * and finishing times on the nodes *)
  method dfs () : unit =
    begin
      let set_val (table: (callnode, int) Hashtbl.t) (n: int) 
          (s: string) (node: callnode) : unit =
        (Hashtbl.replace table node n) 
      in
      let do_dfs_visit (s: string) (node: callnode) : unit = 
        if (Hashtbl.find dfsstatus node) = 0 
        then (self#dfs_visit node)  (* Visits children *) 
      in
      Hashtbl.clear dfsstatus;
      Hashtbl.clear starttime;
      Hashtbl.clear endtime;
      (* Initialization steps *)
      Hashtbl.iter (set_val dfsstatus 0) graphtbl;
      Hashtbl.iter (set_val starttime 0) graphtbl;
      Hashtbl.iter (set_val endtime 0) graphtbl;
      dfsTime <- 0;  (* Reset the DFS time *)
      (* DFS Visit step *)
      Hashtbl.iter do_dfs_visit graphtbl;
      Printf.fprintf stderr "DFS completed\n";
    end

  (** DFS related routines *)
  method dfs_visit (node: callnode) : unit =
    begin
      let visitchild (s: string) (child: callnode) : unit = 
        if (Hashtbl.find dfsstatus child) = 0 
        then (self#dfs_visit child) 
      in
      (Hashtbl.replace dfsstatus node 1);
      dfsTime <- (dfsTime + 1);
      Hashtbl.replace starttime node dfsTime; 
      Hashtbl.iter visitchild node.cnCallees;  
      Hashtbl.replace dfsstatus node 2;
      dfsTime <- (dfsTime + 1);
      Hashtbl.replace endtime node dfsTime;
    end

  (** DFS routine on the inverted graph, in reverse order of completion 
   *  numbers from a first DFS routine. Used in SCC Computation.
   *  ASSUME: That dfs has already been called on the graph. *)
  method dfs_scc () : unit = 
    begin
      let set_val (table: (callnode, int) Hashtbl.t) (n: int) 
          (s: string) (node: callnode) : unit =
        (Hashtbl.replace table node n)
      in
      let do_dfs_visit (node: callnode) : unit = 
        if (Hashtbl.find dfsstatus node) = 0
        then (self#dfs_visit_scc node); (* This visits parents - G^T*)
        currsccnum <- (currsccnum + 1);
      in
      let dfs_order_list = self#nodes_dfs_order() in
      (Hashtbl.clear dfsstatus);
      (Hashtbl.clear starttime);
      (Hashtbl.clear endtime);
      (Hashtbl.clear sccnums);
      (Hashtbl.iter (set_val dfsstatus 0) graphtbl);
      (Hashtbl.iter (set_val starttime 0) graphtbl);
      (Hashtbl.iter (set_val endtime 0) graphtbl);
      (Hashtbl.iter (set_val sccnums 0) graphtbl);
      dfsTime <- 0;
      currsccnum <- 0;
      (* Actual DFS step. Applies dfs_visit in order to dfs_order_list *)
      (ignore (List.map do_dfs_visit dfs_order_list));
      (* After we're all done with the DFS, finally set the sccNum values 
         for each of the nodes
         XXX: Beware: if there are any hashtables with callnode as the first
         element, then, you have to remove the node from those hashtables,
         and add the node with the changed value of sccNUM. Otherwise, the
         hash value of the node will have changed, and you will run into all
         sorts of problems *)
      let get_and_set_sccnums (node: callnode) (sccnum: int) : unit =
        begin
          let name = node.cnInfo.vname in
          let newnode = node in
          newnode.sccNums := sccnum;
          (Hashtbl.replace graphtbl name newnode);
        end in
      (Hashtbl.iter get_and_set_sccnums sccnums);
    end

  (** DFS related routines: Set SCC number here. *)
  method dfs_visit_scc (node: callnode) : unit =
    begin
      let visitchild (s: string) (child: callnode) : unit = 
        if (Hashtbl.find dfsstatus child) = 0 
        then (self#dfs_visit_scc child);
      in
      (Hashtbl.replace dfsstatus node 1);
      dfsTime <- (dfsTime + 1);
      (Hashtbl.replace starttime node dfsTime); 
      (Hashtbl.replace sccnums node currsccnum); 
      (Hashtbl.iter visitchild node.cnCallers); 
      (Hashtbl.replace dfsstatus node 2);
      dfsTime <- (dfsTime + 1);
      (Hashtbl.replace endtime node dfsTime);
    end

  (** nodes_dfs_order Return a list of nodes sorted in decreasing order 
   *  by DFS finishing times. ASSUME: That dfs has already been done 
   **)
  method nodes_dfs_order (): callnode list =  
    begin
      let sortfun (a: callnode) (b: callnode) : int =
        if (Hashtbl.find endtime a) = (Hashtbl.find endtime b) 
        then 0
        else if (Hashtbl.find endtime a) < (Hashtbl.find endtime b)
        then -1
        else 1
      in
      let flatlist : callnode list = (list_keys endtime) in
      (List.rev (List.sort sortfun flatlist))
    end

  (** SCC number computation *)  
  method sccnum_compute (): unit = 
    begin
      (* Obtain completion numbers using one DFS *)
      self#dfs();
      (* Reverse the graph and do DFS to obtain SCC numbers *)
      self#dfs_scc();
    end

  (** Break recursion in the call-graph. Break non-trivial recursion by 
      removing an edge from the SCC in an arbitrary manner. Good enough 4 now *)
  method break_recursion () : unit = 
    begin
      self#sccnum_compute();
      (* A little hashtable of SCCs for which we've broken recursion already *)
      let broken_recursion : (int, bool) Hashtbl.t = (Hashtbl.create 5) in
      (* Create "inverse" SCC map, and populate it. Collision => SCC *)
      let inv_sccmap : (int, callnode) Hashtbl.t = (Hashtbl.create 117) in
      let populate_inverse_map (s: string) (node: callnode) =
        let num = node.sccNums in
        if (Hashtbl.mem inv_sccmap !num)
        then (* We found a recursive node *)
          begin
            if (Hashtbl.mem broken_recursion !num) = false
            then (* We've not yet fixed this SCC. Fix it *)
              begin
                (* First search for a node called by "node" with the same SCC number *)
                let nodecallee = ref node in
                let iterfun (a: string) (b: callnode) =
                  if (a <> node.cnInfo.vname) && (b.sccNums = node.sccNums)
                  then nodecallee := b;
                in
                (Hashtbl.iter iterfun node.cnCallees);
                let peer = !nodecallee in
                (* Remove it here *)
                (Hashtbl.remove node.cnCallees peer.cnInfo.vname);
                (Hashtbl.remove peer.cnCallers node.cnInfo.vname);
                (Hashtbl.add broken_recursion !num true);
                (warning ["Rec. broken caller/callee: "; (node.cnInfo.vname); (peer.cnInfo.vname);]); 
              end;
          end
        else (Hashtbl.add inv_sccmap !num node);
      in
      (Hashtbl.iter populate_inverse_map graphtbl); 
      (* If we broke recursion, then, rerun the algorithm once again, to make
         sure there are no more recursive cases to be broken *)
      if (Hashtbl.length broken_recursion) > 0 
      then if (self#check_recursion() > 1) then self#break_recursion();
    end


  (** Check for presence of recursion in the call-graph. 
   * 0-> no recursion, 1-> self loops, 2-> non-trivial SCC, 3-> 1 and 2 *)
  method check_recursion (): int =
    begin
      (* If the function calls itself, set rec_cntr to 1 *)
      let call_myself (s: string) (node: callnode) : unit = 
        if (Hashtbl.mem node.cnCallees node.cnInfo.vname)
        then
          begin
            (* if we're at 2 or 3, set to 3, else set to 1 *)
            if ((rec_cntr = 2) || (rec_cntr = 3))
            then rec_cntr <- 3 else rec_cntr <- 1;
            (warning [" SELF LOOP "; node.cnInfo.vname]);
          end
      in
      (* Are there any self loops in the call-graph. If so, rec_cntr
         will be set to 1. Otherwise, it will remain 0. *)
      let self_loops () : unit = (Hashtbl.iter call_myself graphtbl) 
      in
      (* Are there two nodes with the same SCC number?
         Form the "inverse" table of sccnums. If you encounter collision, 
         then there is recursion, and rec_cntr wll be set to 2. *)   
      let non_trivial_scc () : unit =
        let inv_sccmap : (int, callnode) Hashtbl.t = 
          (Hashtbl.create 117) in
        let add_inv (s: string) (node: callnode) : unit = 
          let num = node.sccNums in
          (Hashtbl.add inv_sccmap !num node) in
        let count_bindings (sccn: int) (node: callnode) : unit  =
          let nodeInfol = (Hashtbl.find_all inv_sccmap sccn) in 
          let retvname(n: callnode) : string = n.cnInfo.vname in
          let vnamel = (List.map retvname nodeInfol) in
          if (List.length nodeInfol) > 1
          then 
            begin
              (* If we're at 1 or 3, set to 3, else set to 2 *)
              if ((rec_cntr = 1) || (rec_cntr = 3)) 
              then rec_cntr <- 3 else rec_cntr <- 2;
              (warning ([" NON-TRIVIAL SCC "]@vnamel));
            end
        in
        (Hashtbl.iter add_inv graphtbl);
        (Hashtbl.iter count_bindings inv_sccmap)
      in
      
      rec_cntr <- 0;  
      self_loops();
      self#sccnum_compute();
      non_trivial_scc();
      (match rec_cntr with
         | 1 -> (warning ["\n\t ------------ SELF LOOPS EXIST -------------\n"]);
         | 2 -> (warning ["\n\t ------------ NON-TRIVIAL SCCS -------------\n"]);
         | 3 -> (warning ["\n\t ---------- RECURSION: BOTH KINDS ----------\n"]);
         | _ -> ());
      rec_cntr;
    end


  (** Topological sort. Return a list of nodes in increasing order of their
   * finishing times. If there is an SCC, break recursion. If there
   * is a self loop, it does not matter, we just ignore the self loop *)
  method topsort () : callnode list = 
    begin
      self#break_recursion();
      self#dfs();
      List.rev (self#nodes_dfs_order());
    end

end
  (*===========================================================================*)


(*===========================================================================*)
(* The following code denotes what is exported from cgcomp_dri *)
(*===========================================================================*)
type callgraph_retval = {
  fullcg: callgraph;
  fpta_info: (string, Cil.fundec) Hashtbl.t;
  varinfo_fundec: (Cil.varinfo, Cil.fundec) Hashtbl.t;
  funcs_with_no_fundecs: varinfo list;
  topsort_norec: callnode list;
}
    
let callgraph_build (f: Cil.file) (dotopsort: bool) =
  begin
    (Printf.fprintf stderr "Computing FTPA...\n%!");
    let fpta = (new typebased_fpta) in 
    (visitCilFileSameGlobals (fpta :> cilVisitor) f);
    let fpta_info_for_cg = (fpta#get_funcs_groupbytype()) in
    if (!do_function_pointer_analysis = false) then begin
      warning ["FPTA analysis was not done: function-pointer targets will be empty"];
    end;

    (Printf.fprintf stderr "Computing callgraph + funcptrs...\n%!");
    let cg: callgraphcomputer = (new callgraphcomputer) in
    (visitCilFileSameGlobals (cg :> cilVisitor) f);
    (cg#print_fptr_resolutions());
    
    (* <DIAGNOSTICS> *)
    (*
      (infomsg ["Dumping the call-graph....."]);
      let diagchan = (open_out "Call-graph-DIAGNOSTICS.txt") in
      (cg#printgraph_transitive diagchan true);
      (close_out diagchan);
      (infomsg ["done\n"]);
    *)
    (* </DIAGNOSTICS> *)
    
    let topsort = ref [] in
    if (dotopsort = true) then topsort := (cg#topsort()) else topsort := [];

    let ret: callgraph_retval = { 
      fullcg = (cg#getGraph()); 
      fpta_info = fpta_info_for_cg;
      varinfo_fundec = (cg#get_varinfo_fundec());
      funcs_with_no_fundecs = (cg#get_functions_with_no_fundecs());
      topsort_norec = !topsort;
    } 
    in ret
	 
  end;

(* TODO: Need to return the following:
   fullcg
   topsort_sccdag
   cg_norec
   topsort_norec
   fpta_info
   varinfo_fundec 
   funcs_with_no_fundecs
*)
