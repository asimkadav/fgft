(*
 *
 * Copyright (c) 2005, 
 *  George C. Necula    <necula@cs.berkeley.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

(** This is a module that inserts runtime checks for memory reads/writes and 
 * allocations *)

(*
 *  TODO: This can be modified to remove the variable odft_error and just use
 *  and enclosing if over calls to logRead/logWrite.
 *
 *
 *)


open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

let doSfi = ref false
let doSfiReads = ref false
let doSfiWrites = ref true
let doSfiKernel = ref 0 

(* list_append: Append an element to a list *)
let list_append (lst : 'a list) (elt: 'a) : 'a list =
 begin
   (List.append lst [elt]);
 end

 (* Converts an exp to a string *)
  let exp_tostring (e: exp) : string =
      begin
          (Pretty.sprint 100 (d_exp() e))
 end 

(* Converts a statement to a string. *)
  let stmt_tostring (stmt: stmt) : string =
      Pretty.sprint 100 (d_stmt () stmt);;

(* Converts an instr to a string *)
let instr_tostring (i: instr) : string =
  begin
    (Pretty.sprint 100 (d_instr() i));
  end


(* Create an expression from an Lval *)
 let expify_lval (l: lval) : exp = Lval(l)
 let lvalify_varinfo (v: varinfo) : lval = (Var(v),NoOffset)

(* A number of functions to be skipped *)
let skipFunctions : (string, unit) H.t = H.create 13
let mustSfiFunction (f: fundec) : bool = 
  not (H.mem skipFunctions f.svar.vname)

let addFunctions: (string, unit) H.t = H.create 13
(*
let contains_nosfi_prefix (s: string) : bool =
  begin
    let nosfi_prefix_regexp = (Str.regexp "odft_nosfi_") in
    (Str.string_match nosfi_prefix_regexp s 0);
  end 
*)
let mustSfiFunction2 (f:fundec) : bool =
    (H.mem addFunctions f.svar.vname);

(** Some functions are known to be allocators *)
type dataLocation = 
    InResult  (* Interesting data is in the return value *)
  | InArg of int (* in the nth argument. Starts from 1. *)
  | InArgTimesArg of int * int (* (for size) data is the product of two 
                                * arguments *)
  | PointedToByArg of int (* pointed to by nth argument *)

(** Compute the data based on the location and the actual argument list *)
let extractData (dl: dataLocation) (args: exp list) (res: lval option) : exp = 
  let getArg (n: int) = 
    try List.nth args (n - 1) (* Args are based at 1 *)
    with _ -> E.s (E.bug "Cannot extract argument %d at %a" 
                     n d_loc !currentLoc)
  in
  match dl with 
    InResult -> begin
      match res with 
        None -> 
          E.s (E.bug "Cannot extract InResult data (at %a)" d_loc !currentLoc)
      | Some r -> Lval r
    end
  | InArg n -> getArg n
  | InArgTimesArg (n1, n2) -> 
      let a1 = getArg n1 in
      let a2 = getArg n2 in 
      BinOp(Mult, mkCast ~e:a1 ~newt:longType, 
            mkCast ~e:a2 ~newt:longType, longType)
  | PointedToByArg n -> 
      let a = getArg n in 
      Lval (mkMem a NoOffset)
      


(* for each allocator, where is the length and where is the result *)
let allocators: (string, (dataLocation * dataLocation)) H.t = H.create 13
let _ = 
  H.add allocators "malloc" (InArg 1, InResult);
  H.add allocators "kmalloc" (InArg 1, InResult);
  H.add allocators "calloc" (InArgTimesArg (1, 2), InResult);
  H.add allocators "realloc" (InArg 2, InResult);
  H.add allocators "mutex_lock" (InArg 1, InArg 1)

(* for each deallocator, where is the data being deallocated *)
let deallocators: (string, dataLocation) H.t = H.create 13
let _= 
  H.add deallocators "free" (InArg 1);
  H.add deallocators "kfree" (InArg 1);
  H.add deallocators "realloc" (InArg 1)
  
(* Returns true if the given lvalue offset ends in a bitfield access. *) 
let rec is_bitfield lo = match lo with
  | NoOffset -> false
  | Field(fi,NoOffset) -> not (fi.fbitfield = None)
  | Field(_,lo) -> is_bitfield lo
  | Index(_,lo) -> is_bitfield lo 

(* Return an expression that evaluates to the address of the given lvalue.
 * For most lvalues, this is merely AddrOf(lv). However, for bitfields
 * we do some offset gymnastics. 
 *)
let addr_of_lv (lv: lval) = 
  let lh, lo = lv in 
  if is_bitfield lo then begin
    (* we figure out what the address would be without the final bitfield
     * access, and then we add in the offset of the bitfield from the
     * beginning of its enclosing comp *) 
    let rec split_offset_and_bitfield lo = match lo with 
      | NoOffset -> failwith "logwrites: impossible" 
      | Field(fi,NoOffset) -> (NoOffset,fi)
      | Field(e,lo) ->  let a,b = split_offset_and_bitfield lo in 
                        ((Field(e,a)),b)
      | Index(e,lo) ->  let a,b = split_offset_and_bitfield lo in
                        ((Index(e,a)),b)
    in 
    let new_lv_offset, bf = split_offset_and_bitfield lo in
    let new_lv = (lh, new_lv_offset) in 
    let enclosing_type = TComp(bf.fcomp, []) in 
    let bits_offset, bits_width = 
      bitsOffset enclosing_type (Field(bf,NoOffset)) in
    let bytes_offset = bits_offset / 8 in
    let lvPtr = mkCast ~e:(mkAddrOf (new_lv)) ~newt:(charPtrType) in
    (BinOp(PlusPI, lvPtr, (integer bytes_offset), ulongType))
  end else 
    (mkAddrOf (lh,lo)) 


let mustLogLval (forwrite: bool) (lv: lval) : bool = 
  match lv with
    Var v, off -> (* Inside a variable. We assume the array offsets are fine *)
      false
  | Mem e, off -> 
      if forwrite && not !doSfiWrites then 
        false
      else if not forwrite && not !doSfiReads then 
        false

      (* If this is an lval of function type, we do not log it *) 
      else if isFunctionType (typeOfLval lv) then 
        false
      else 
        true

(* Create prototypes for the logging functions *)
let mkProto (name: string) (args: (string * typ * attributes) list) = 
  let fdec = emptyFunction name in
  fdec.svar.vtype <- TFun(intType, 
                          Some args, false, []);
  fdec
  
 let generate_check_code (lv: lval) : stmt =
     begin
       (* let check_var = (UnOp(LNot, (expify_lval lv) , voidPtrType)) in*)
       let check_var =  (expify_lval lv) in
       let false_block = (mkBlock [(mkEmptyStmt ())]) in
       let true_block  = (mkBlock
       [(mkStmt(Return(Some(Const(CInt64((Int64.of_int 75),IInt,None))),
       locUnknown)))]) in
       let new_stmt = (mkStmt (If (check_var, true_block, false_block,
       locUnknown))) in
       new_stmt;
     end

let expify_fundec (g: fundec) : exp = Lval(Var(g.svar),NoOffset)

 let generate_check_code2 (lv: lval) (f:fundec) : stmt =
     begin
       (* let check_var = (UnOp(LNot, (expify_lval lv) , voidPtrType)) in*)
       let check_var =  (expify_lval lv) in
       let false_block = (mkBlock [(mkEmptyStmt ())]) in
       let odft_fundec = emptyFunction (f.svar.vname) in
       
       	let callargs = ref[] in
	for i = 0 to (List.length f.sformals) - 1  do	
	let arg = List.nth f.sformals i in	  
	 callargs := !callargs@[(expify_lval (lvalify_varinfo arg))];	
	done;

       let (ret_var_type,_,_,_) = splitFunctionType f.svar.vtype in
       let callfdec = match ret_var_type with
       | TVoid(_) -> Call(None, expify_fundec odft_fundec, !callargs, locUnknown);
       | _ ->  ( Call(Some(lv), expify_fundec odft_fundec, !callargs, locUnknown));
	in
	
       let retstmt = match ret_var_type with
  	| TVoid(_) -> Return(None, locUnknown);
	| _ ->  ((Return(Some(expify_lval lv), locUnknown))) ;
	in	
       	
	
	let true_block  = (mkBlock
       [ 
       (mkStmtOneInstr callfdec);	
       (mkStmt(retstmt));
	] 
       ) in
       let new_stmt = (mkStmt (If (check_var, true_block, false_block,
       locUnknown))) in
       new_stmt;
      end

let logReads = mkProto "logRead" [ ("addr", voidPtrType, []);
                                   ("what", charPtrType, []);
                                   ("where", charPtrType, []);
                                   ("file", charPtrType, []);
                                   ("line", intType, []) ]
let callLogRead (lv: lval) (cfdec:fundec) (cfvar:varinfo)  = 
  let what = Pretty.sprint 80 (d_lval () lv) in
  Call(Some(lvalify_varinfo cfvar), Lval(Var(logReads.svar),NoOffset),
    [ addr_of_lv lv; mkString what; mkString cfdec.svar.vname; mkString
    !currentLoc.file;integer !currentLoc.line], !currentLoc )   
  (*    
  let call_stmt = mkStmtOneInstr ( Call(Some(lvalify_varinfo temp_var), (* None, *) 
       Lval(Var(logReads.svar),NoOffset), 
       [ addr_of_lv lv; mkString what; mkString cfdec.svar.vname; mkString !currentLoc.file;
         integer !currentLoc.line], !currentLoc ))  in 
  let chkcode = generate_check_code (lvalify_varinfo temp_var) in
  call_stmt::chkcode
  *)

let logWrites = mkProto "logWrite" [ ("addr", voidPtrType, []);
                                     ("what", charPtrType, []);
                                     ("where", charPtrType, []);
                                     ("file", charPtrType, []);
                                     ("line", intType, []) ]
let callLogWrite (lv: lval) (cfdec:fundec) (cfvar:varinfo)  = 
  let what = Pretty.sprint 80 (d_lval () lv) in
  Call(Some(lvalify_varinfo cfvar), 
       Lval(Var(logWrites.svar), NoOffset), 
       [ addr_of_lv lv; mkString what; mkString cfdec.svar.vname; mkString !currentLoc.file;
         integer !currentLoc.line], !currentLoc )

let logStackFrame  = mkProto "logStackFrame" [ ("func", charPtrType, []) ]
let logSfiFrame  = mkProto "logSfiFrame" [ ("func", charPtrType, []) ]
let logStackVar  = mkProto "logStackVar" [ ("var", charPtrType, []) ]

let callLogStack (fname: string) = 
  Call(None, 
       Lval(Var(logStackFrame.svar), NoOffset), 
       [ mkString fname; ], !currentLoc )

let callLogSfi (fname: string)(lv: lval) =
  Call(Some(lv), 
       Lval(Var(logSfiFrame.svar), NoOffset), 
       [ mkString fname; ], !currentLoc )

let callLogStackVar (svarlv: lval) = 
  Call(None, 
       Lval(Var(logStackVar.svar), NoOffset), 
       [ addr_of_lv svarlv ; ], !currentLoc )

let logAlloc = mkProto "logAlloc" [ ("addr", voidPtrType, []);
                                    ("size", intType, []);
                                    ("fn", charPtrType, []);
                                    ("file", charPtrType, []);
                                    ("allocfn", charPtrType, []);    
                                    ("line", intType, []) ]
let callLogAlloc (szloc: dataLocation) 
                 (resLoc: dataLocation) 
                 (args: exp list) 
                 (res: lval option)
                 (cfdec:fundec) 
                 (fvname: string) = 
  let sz = extractData szloc args res in 
  let res = extractData resLoc args res in 
  Call(None, 
       Lval(Var(logAlloc.svar), NoOffset), 
       [ res; sz; mkString cfdec.svar.vname; mkString !currentLoc.file;
       mkString fvname; integer !currentLoc.line ], !currentLoc )
    
let logFree = mkProto "logFree" [ ("addr", voidPtrType, []);
                                  ("fn", charPtrType, []);
                                  ("file", charPtrType, []);
                                  ("allocfn", charPtrType, []);
                                  ("line", intType, []) ]
let callLogFree  (dataloc: dataLocation) 
                 (args: exp list) 
                 (res: lval option) 
                 (cfdec:fundec)
                 (fvname: string) = 
let data = extractData dataloc args res in 
  Call(None, 
       Lval(Var(logFree.svar), NoOffset), 
       [ data; mkString cfdec.svar.vname; mkString !currentLoc.file;
       mkString fvname; integer !currentLoc.line ], !currentLoc )

let should_add_if (s:stmt) (l:lval) : int*lval  =
    let ret_val = ref 0 in
    let cfvar = makeVarinfo false "asim" intType in
    (
        match s.skind with 
        | Instr(ilist) ->
                for j = 0 to (List.length ilist) - 1 do
                    let cur_instr = (List.nth ilist j) in
                    match cur_instr with
                    | Call(lvalue_option,e,el,loc) -> ( 
                        if (String.compare  (exp_tostring e) "LogRead" == 0) then
                            ret_val := 1;
                    );
              |_ ->();
                done;
     |_-> ();  
    );
    !ret_val, lvalify_varinfo cfvar

let process_ilist (ilist: instr list) (labels: label list) :stmt list =  
    let stmt_list = ref [] in 
    let curr_instr_list = ref [] in
    let chkcode = ref (mkEmptyStmt ())  in
    for j = 0 to (List.length ilist) - 1 do
        let cur_instr = (List.nth ilist j) in
        curr_instr_list := (List.append !curr_instr_list [(List.nth ilist j)]);
        match cur_instr with
        | Call(lvalue_option,e,el,loc) -> ( 
            if 
	     (String.compare  (exp_tostring e) "logRead" == 0) ||
             (String.compare  (exp_tostring e) "logWrite" == 0)  
	then 
                match lvalue_option with
                | Some(lv) ->( chkcode := generate_check_code(lv); 
                let curr_stmt = mkStmt(Instr(!curr_instr_list)) in
                curr_stmt.skind <- Instr(!curr_instr_list);
                stmt_list := !stmt_list@[curr_stmt];
                stmt_list := !stmt_list@[!chkcode]);
                Printf.fprintf stderr "Added new statement "; 
                curr_instr_list := [];
                (* new_stmt_list := !new_stmt_list@[chkcode]; *)
                |_ -> ();

              );
         |_ -> ();
    done;
    let final_stmt = mkStmt(Instr(!curr_instr_list)) in 
    final_stmt.skind <- Instr(!curr_instr_list);
    let final_stmt_list = List.append !stmt_list [final_stmt] in    
    if (List.length final_stmt_list > 0) then
      ignore((List.hd final_stmt_list).labels <- labels);
    final_stmt_list;
    (* !chkcode*)

class postsfiVisitorClass  = object (self)
    inherit nopCilVisitor
    val mutable cfvar: varinfo = makeVarinfo false "asim" intType;

    method vblock (b: block) =
    begin    
       let new_stmt_list = ref [] in 
       for i = 0 to (List.length b.bstmts) - 1 do
      (
       (* Printf.fprintf stderr "Seen %s.\n" (stmt_tostring (List.nth
         b.bstmts i)) ;  *) 
       match (List.nth b.bstmts i).skind with 
        | Instr(ilist) ->  new_stmt_list := !new_stmt_list@process_ilist
        ilist (List.nth b.bstmts i).labels;
        |_ -> new_stmt_list := !new_stmt_list@[List.nth b.bstmts i];
        );
        (*
        if (should_add_if (List.nth fdec.sbody.bstmts i) cfvar == 1) then
          let chkcode = generate_check_code (lvalify_varinfo cfvar) in
          new_stmt_list := !new_stmt_list@[chkcode];
          *)
        
       done;
       b.bstmts <- !new_stmt_list;
       DoChildren;
 end

   method vfunc (f: fundec): fundec visitAction =
   begin
       if ((mustSfiFunction2 f) == true) then
         DoChildren
       else
          SkipChildren;
    end     

method vstmt (s: stmt) : stmt visitAction =
begin
	DoChildren;
end

end

class sfiVisitorClass : Cil.cilVisitor = object (self)
  inherit nopCilVisitor
  val mutable cfdec : fundec = emptyFunction "temp";    
  val mutable cfvar: varinfo = makeVarinfo false "asim" intType; 
  method vexpr (e: exp) : exp visitAction = 
    match e with 
      Lval lv when mustLogLval false lv -> (* A read *)
        self#queueInstr [ callLogRead lv cfdec cfvar];
        DoChildren

    | _ -> DoChildren
 
 method vvdec (v: varinfo) : varinfo visitAction =
 match v.vtype with
	TPtr(_,_) -> (if (!doSfiKernel == 0) then  self#queueInstr [callLogStackVar(lvalify_varinfo v)]); DoChildren
	|_ -> DoChildren	

  method vinst (i: instr) : instr list visitAction = 
    match i with 
      Set(lv, e, l) when mustLogLval true lv ->
        self#queueInstr [ callLogWrite lv cfdec cfvar];
        DoChildren

    | Call(lvo, f, args, l) -> 
        (* Instrument the write *)
        (match lvo with 
          Some lv when mustLogLval true lv -> 
            self#queueInstr [ callLogWrite lv cfdec cfvar]
        | _ -> ());
        (* Do the expressions in the call, and then see if we need to 
         * instrument the function call *)
        ChangeDoChildrenPost
          ([i], 
           (fun il -> 
             currentLoc := l;
             match f with 
               Lval (Var fv, NoOffset) -> begin 
                 (* Is it an allocator? *)
                 try 
                   let szloc, resloc = H.find allocators fv.vname in
                   il @ [callLogAlloc szloc resloc args lvo cfdec fv.vname]
                 with Not_found -> begin
                   (* Is it a deallocator? *)
                   try 
                     let resloc = H.find deallocators fv.vname in 
                     il @ [ callLogFree resloc args lvo cfdec fv.vname ]
                   with Not_found -> 
                     il
                 end
               end
             | _ -> il))

    | _ -> DoChildren


  method vfunc (fdec: fundec) =
    cfdec <- fdec;  
    let tickvar = makeLocalVar fdec
          ("__odft_error") intType in
    cfvar <- tickvar;
    let snt_reset = Set((lvalify_varinfo tickvar),  zero, locUnknown) in
    let stmt_init_var = (mkStmt (Instr [snt_reset])) in
      Printf.fprintf stderr "Activating sfi for fn:%s.\n" fdec.svar.vname; 
    (* Instead a stack log at the start of a function *)
     if (!doSfiKernel = 1)  then (
    Printf.fprintf stderr "****Doing the kernel part..\n"; 
    ChangeDoChildrenPost 
      (fdec,
       fun fdec -> 
         fdec.sbody <- 
	mkBlock 
             [ 
	       stmt_init_var;	
	       mkStmtOneInstr (callLogSfi fdec.svar.vname (lvalify_varinfo tickvar));
	       generate_check_code2 (lvalify_varinfo tickvar) fdec;
               mkStmt (Block fdec.sbody) ];
         fdec) )
	else (
      ChangeDoChildrenPost
      (fdec,
       fun fdec -> 
         fdec.sbody <- 
	mkBlock 
             [ 
	       stmt_init_var;	
	       mkStmtOneInstr (callLogStack fdec.svar.vname);
               mkStmt (Block fdec.sbody) ];
         fdec) );
end

let doit (f: file) (gen_kern: int) =
  Printf.fprintf stderr "Performing sfi wth gen_kern %d..\n" gen_kern;
  Arg.Set doSfiReads;
  Arg.Set doSfiWrites;
  let sfiVisitor = new sfiVisitorClass in
  let appendglobals = ref [] in
  let postsfiVisitor: postsfiVisitorClass  = new postsfiVisitorClass in
  let compileLoc (l: location) = function
      ACons("inres", []) -> InResult
    | ACons("inarg", [AInt n]) -> InArg n
    | ACons("inargxarg", [AInt n1; AInt n2]) -> InArgTimesArg (n1, n2)
    | ACons("pointedby", [AInt n]) -> PointedToByArg n
    | _ -> E.warn "Invalid location at %a" d_loc l;
        InResult
  in
  doSfiKernel := 0;
  doSfiKernel := gen_kern;
  iterGlobals f
    (fun glob -> 
      match glob with 
        (* GFun(fdec, _) when mustSfiFunction fdec -> Commented by asim and
         * below line added  *) 
        GFun(fdec, _) when mustSfiFunction2 fdec -> 
	     (*
	     let nfdec = fdec in
	     nfdec.svar.vname <- "odft_nosfi_" ^ fdec.svar.vname;
	     let nfun = GFun(nfdec, locUnknown) in
	     f.globals <- nfun :: f.globals; 	
	     *)		 
	     ignore (visitCilFunction sfiVisitor fdec);
            (*ignore (visitCilFunction postsfiVisitor fdec)
             ignore (visitCilFunction postsfiVisitor fdec)*)
      | GPragma(Attr("sfiignore", al), l) -> 
          List.iter 
            (function AStr fn -> H.add skipFunctions fn ()
              | _ -> E.warn "Invalid argument in \"sfiignore\" pragma at %a"
                            d_loc l)
            al

      | GPragma(Attr("sfialloc", al), l) -> begin
          match al with 
            AStr fname :: locsz :: locres :: [] -> 
              H.add allocators fname (compileLoc l locsz, compileLoc l locres)
          | _ -> E.warn "Invalid sfialloc pragma at %a" d_loc l
      end
                
      | GPragma(Attr("sfifree", al), l) -> begin
          match al with 
            AStr fname :: locwhat :: [] -> 
              H.add deallocators fname (compileLoc l locwhat)
          | _ -> E.warn "Invalid sfifree pragma at %a" d_loc l
      end
                

      | _ -> ());
  (* Now add the prototypes for the instrumentation functions *)
  if (gen_kern = 1) then
	f.globals <- GVarDecl (logSfiFrame.svar, locUnknown) :: f.globals;
  if (gen_kern = 0) then 
  f.globals <-                      
    GVarDecl (logReads.svar, locUnknown) ::
    GVarDecl (logWrites.svar, locUnknown) ::
    GVarDecl (logStackFrame.svar, locUnknown) ::
    GVarDecl (logStackVar.svar, locUnknown) ::
    GVarDecl (logAlloc.svar, locUnknown) :: 
    GVarDecl (logFree.svar, locUnknown) :: f.globals;
    if (gen_kern = 0) then	
    visitCilFileSameGlobals (postsfiVisitor :> cilVisitor) f

let feature : featureDescr = 
  { fd_name = "sfi";
    fd_enabled = doSfi;
    fd_description = "instrument memory operations";
    fd_extraopt = [
    "--sfireads", Arg.Set doSfiReads, " SFI for reads";
    "--sfiwrites", Arg.Set doSfiWrites, " SFI for writes";
    ];
    fd_doit = (function (f: file) -> doit f 0);
    (*fd_doit = doit; *)
    fd_post_check = true;
  } 

