(* Asim modified the safe drive fault injection to fit in with on demand fault
 * tolerance. Orignial credits to the safe drive team.
 *) 


open Pretty
open Hashtbl
open Cil
module E = Errormsg
module H = Hashtbl

exception CannotChange;;

let dofault = ref false
let doSfiReads = ref false
let doSfiWrites = ref true
let kindarg = ref "asim"

let lval_tostring (lv: lval) : string = (Pretty.sprint 100 (d_lval() lv))


(* Converts an exp to a string *)
  let exp_tostring (e: exp) : string =
      begin
          (Pretty.sprint 100 (d_exp() e))
      end

 (* A list of memcpy-like functions that read or writes a buffer:
    function name -> index of size parameter *)
 let scan_funcs : (string, int) H.t = H.create 10
 let init_scan_funcs () =
   H.add scan_funcs "memcpy" 2;
   H.add scan_funcs "strncpy" 2;
   H.add scan_funcs "memset" 2;
   H.add scan_funcs "memchr" 2;
   H.add scan_funcs "memscan" 2;
   H.add scan_funcs "copy_from_user" 2;
   H.add scan_funcs "copy_to_user" 2
 ;;

 let nth = ref 0
 let flipCount = ref 0

 type faultkind =
   FlipCondition
 | LoopBound
 | ScanOverrun
 | OffByOne
 | CorruptParameter
 | MissingAssignment
 | MissingCall
 | UnknownKind
 ;;


 let kind_of_string s =
     match s with
     "flipcondition" -> FlipCondition
   | "loopbound" -> LoopBound
   | "scanoverrun" -> ScanOverrun
   | "offbyone" -> OffByOne
   | "corruptparameter" -> CorruptParameter
   | "missingassignment" -> MissingAssignment
   | "missingcall" -> MissingCall
   | _ -> E.s (E.error "Unknown fault category %s." s)


 (* increment fault counter for this file, or test if current fault
    should be inject when actually injecting*)
 let flip (): bool = 
    flipCount := !flipCount + 1;
   if (!flipCount == !nth) then
       true
   else false

    let expandBound e =
        let delta = match Random.int 100 with
        x when x >= 0 && x < 50 -> 1
                       | x when x >= 50 && x < 93 -> Random.int 1023 + 2
                       | _ -> (Random.int 3 + 2) * 1024
        in
        BinOp (PlusA, e, Const (CInt64 (Int64.of_int
        delta,IInt,None)), typeOf e)



(* list_append: Append an element to a list *)
let list_append (lst : 'a list) (elt: 'a) : 'a list =
 begin
   (List.append lst [elt]);
 end

 (* Converts an exp to a string *)
  let exp_to_string (e: exp) : string =
      begin
          (Pretty.sprint 100 (d_exp() e))
 end 

(* Converts a statement to a string. *)
  let stmt_to_string (stmt: stmt) : string =
      Pretty.sprint 100 (d_stmt () stmt);;

(* Converts an instr to a string *)
let instr_to_string (i: instr) : string =
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

let add2Functions: (string, unit) H.t = H.create 13

let mustSfiFunction2 (f:fundec) : bool =
    (H.mem add2Functions f.svar.vname);

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
  H.add allocators "calloc" (InArgTimesArg (1, 2), InResult);
  H.add allocators "realloc" (InArg 2, InResult)

(* for each deallocator, where is the data being deallocated *)
let deallocators: (string, dataLocation) H.t = H.create 13
let _= 
  H.add deallocators "free" (InArg 1);
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
                        if (String.compare  (exp_to_string e) "LogRead" == 0) then
                            ret_val := 1;
                    );
              |_ ->();
                done;
     |_-> ();  
    );
    !ret_val, lvalify_varinfo cfvar


 (* Our instruction flipper *)   
let process_ilist (ilist: instr list) (labels: label list) :stmt list =  
    let stmt_list = ref [] in 
    let curr_instr_list = ref [] in
    let chkcode = ref (mkEmptyStmt ())  in
    for j = 0 to (List.length ilist) - 1 do
        let cur_instr = (List.nth ilist j) in
        curr_instr_list := (List.append !curr_instr_list [(List.nth ilist j)]);
        match cur_instr with
        | Call(lvalue_option,e,el,loc) -> ( 
            if (String.compare  (exp_to_string e) "logRead" == 0) ||
            (String.compare  (exp_to_string e) "logWrite" == 0)  then
                match lvalue_option with
                | Some(lv) ->( chkcode := generate_check_code(lv); 
                let curr_stmt = mkStmt(Instr(!curr_instr_list)) in
                curr_stmt.skind <- Instr(!curr_instr_list);
                stmt_list := !stmt_list@[curr_stmt];
                stmt_list := !stmt_list@[!chkcode]);
                Printf.fprintf stderr "FAULTINJECTIONAdded new statement "; 
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

class fVisitor = object
   inherit nopCilVisitor
 
    method vfunc (f: fundec): fundec visitAction =
   begin
       (* Uncomment to inject failure in all functions *)
       (* Hashtbl.add add2Functions f.svar.vname ();*)
       if ((mustSfiFunction2 f) == true) then (
           Printf.fprintf stderr "FAULTINJECTIONfVisitor %s.\n" f.svar.vname;  
         DoChildren )
       else
          SkipChildren;
    end           

 end

class postsfiVisitorClass  = object (self)
    inherit nopCilVisitor
    val mutable cfvar: varinfo = makeVarinfo false "asim" intType;

    method vblock (b: block) =
    begin    
       let new_stmt_list = ref [] in 
       for i = 0 to (List.length b.bstmts) - 1 do
      (
       (* Printf.fprintf stderr "FAULTINJECTIONSeen %s.\n" (stmt_to_string (List.nth
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

  (* This is every expression. We can perform memory and variable
   * corruption here *)  
  method vexpr (e: exp) : exp visitAction =
   match e with
    Lval lv when mustLogLval false lv -> (* A read *)
       self#queueInstr [ ];
        DoChildren
   | _ -> DoChildren   

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
(* End of fault injection class *)

(* return a "corrupted" version of the expression of a specific type *)
 let randomExp = function typ ->
   let typ = unrollType typ in
     match typ with
     TPtr(_,_) -> Formatcil.cExp "(void *) 0" []
       | TInt(ikind,_) -> begin
         (* 0 with 0.5 prob, a random int with 0.5 prob *)
         match Random.int 2 with
         0 -> Const (CInt64 (Int64.zero, ikind, None))
           | _ -> Const (CInt64 (Int64.sub (Random.int64 4294967296L) 2147483648L, ikind, None))
     end
       | TFloat(fkind,_) -> begin
         (* 0.0 with 0.5 prob, a random float between 0 and 1000
            with 0.5 prob *)
         match Random.int 2 with
         0 -> Const (CReal (0.0, fkind, None))
           | _ -> Const (CReal ((Random.float 2000.0) -. 1000.0, fkind, None))
     end
       | TEnum(einfo, _) -> begin
         (* Randomly pick an enum const *)
         let (name, value, _) = List.nth einfo.eitems (Random.int (List.length einfo.eitems)) in
           Const (CEnum (value, name, einfo))
     end
       (* FIXME: what about TComp as function parameters? *)
       | _ -> E.log "Fault: cannot corrupt expression type %a\n" d_type typ;
       raise CannotChange
 ;;    
 

 (* Copy overrun: for calls to kmalloc(), memcpy(), copy_from_user(),
    copy_to_user(), we make the count larger by: 50% 1 byte, 44% 2-1024
    bytes, 6% 2-4KB. *)
 class scanOverrunVisitor = object
     inherit fVisitor 
   method vinst (i: instr) : instr list visitAction =
     match i with
     Call(_lv, (Lval (Var v, NoOffset) as func), args, l) ->
       Printf.fprintf stderr "FAULTINJECTIONProcesss.ong ***** %s .\n" v.vname;  
       if H.mem scan_funcs v.vname  then
         let args_a = Array.of_list args in
         let idx = H.find scan_funcs v.vname in
           Printf.fprintf stderr "FAULTINJECTIONFault: expanded size in call to %s() at %d\n"
           v.vname  l.line;
           args_a.(idx) <- expandBound args_a.(idx);
           Printf.fprintf stderr "FAULTINJECTIONFI: Flipped at %d.\n" l.line;
           ChangeTo [Call(_lv, func, Array.to_list args_a, l)]
       else
         DoChildren
       | _ -> DoChildren
 end   

  (* while(1) {...  if (i ...) break; where i is an integer. we make the
    count larger by: 50% 1 byte, 44% 2-1024 bytes, 6% 2-4KB *)
 class loopBoundVisitor = object
     inherit fVisitor
   method vstmt (s: stmt) : stmt visitAction =
     match s.skind with
     (* Match anything like "if (var < ...) break;" *)
     If (BinOp (binop,(Lval(Var v, NoOffset) as lv),e2,typ), b1, b2, l) ->
       if isIntegralType (typeOf e2) then begin 
           match b2.bstmts with
           {skind=Break(_)} :: _ ->
             (* if flip () then begin *)
               if true then begin
             Printf.fprintf stderr "FAULTINJECTIONFault: make loop bound at %d larger.\n"
             l.line;
             let s' = mkStmt (If(BinOp(binop,lv,expandBound e2,typ), b1, b2, l)) in
             ChangeDoChildrenPost (s',(fun x -> x))
             end 
             else
               DoChildren
         | _ -> DoChildren
         end
       else
         DoChildren
       | _ -> DoChildren
 end


(* Off-by-one: change < to <= etc in expressions *)
 class offByOneVisitor = object
  inherit fVisitor 
   method vexpr (e: exp) : exp visitAction =
     match e with
     BinOp (op, e1, e2, t) ->
       let b = match op with
           Lt | Gt | Le | Ge | PlusPI | PlusA  -> true
         | _ -> false
       in
         if b && flip () then
           let op' = match op with
           Lt -> Div 
         | Gt -> Div  
         | Le -> Div
	 | PlusA -> Div
	 | PlusPI -> Div
         | _ -> Div 
           in
         Printf.fprintf stderr "FAULTINJECTIONFault: inserted off-by-one error in expression:
             %s at %d\n" (exp_tostring e) !currentLoc.line;
         ChangeTo (BinOp (op', e1, (integer 0), t))
         else
           DoChildren
       | _ -> DoChildren
 end



   (* remove the result of assignment at random, this includes removing
    initializer of local/global variables. *)
 class missingAssignmentVisitor = object
   inherit fVisitor
 
   method vinst (i: instr) : instr list visitAction =
     match i with
     Set(lv, e, l) ->
       (* MissingAssignment (1/2) *)
       if (flip ()) then (
           Printf.fprintf stderr "FAULTINJECTIONFault: deleted expression assignment of %s=%s
           at %d \n" (lval_tostring lv) (exp_tostring e) l.line;
           ChangeTo []
         ) else
         DoChildren
       | Call(Some lv, func, args, l) ->
       if flip () then
         begin
           (* MissingAssignment (2/2) *)
           Printf.fprintf stderr "FAULTINJECTIONIgnored return value of function call at %d\n"
           l.line;
           ChangeDoChildrenPost ([Call(None, func, args, l)], (fun x -> x))
         end else DoChildren
       | _ -> DoChildren
 end

 (* remove a function call in an expression at random, if return value
    is non-pointer: the result is 50% 0, 50% a fixed random number. If
    pointer, NULL is used. *)
 class missingCallVisitor = object
   inherit fVisitor
 
   method vinst (i: instr) : instr list visitAction =
     match i with
     Call(Some lv, func, args, l) ->
       let typ = unrollType (typeOfLval lv) in begin
         match typ with
         (* we cannot delete a call that returns a struct *)
         TComp(_,_) -> DoChildren
           | _ ->
           if flip () then begin
               Printf.fprintf stderr "FAULTINJECTIONFault: deleted call to %s at %d\n"
               (exp_tostring func) l.line;
               ChangeTo [Set(lv, randomExp typ, l)]
             end else
             DoChildren
         end
       | _ -> DoChildren
 end

(* corrupt any non-pointer argument to a function at random, set any
    pointer parameter to NULL at random *)
 class corruptParameterVisitor = object
   inherit fVisitor
   
   method vinst (i: instr) : instr list visitAction =
     match i with
     Call(Some lv, func, args, l) ->
       if (List.length args > 0 ) then
         begin 
           let idx = Random.int (List.length args) in
           let args_a = Array.of_list args in
           let typ = unrollType (typeOf (List.nth args idx)) in
         try
               args_a.(idx) <- randomExp typ;
           Printf.fprintf stderr "FAULTINJECTIONFault: corrupted parameter %d to %s at %d\n"
           idx (exp_tostring func) l.line;
           ChangeTo [Call(Some lv, func, Array.to_list args_a, l)]
         with CannotChange ->
           (Printf.fprintf stderr "FAULTINJECTIONFault: ignored change to parameter.\n";
            DoChildren)
         end else DoChildren
       | _ -> DoChildren
 end

 (* flip all if conditions *)
 class flipConditionVisitor = object
   inherit fVisitor
   
   method vstmt (s: stmt) : stmt visitAction =
     match s.skind with
     If(e,b1,b2,l) -> if flip () then (
         Printf.fprintf stderr "FAULTINJECTIONFault: flip if condition at %d\n" (get_stmtLoc
         s.skind).line;
         let s' = mkStmt (If(UnOp(LNot,e,typeOf e), b1, b2, l)) in
           ChangeDoChildrenPost (s', (fun x -> x))
       ) else
         DoChildren
       | _ -> DoChildren
 end   

 let getVisitor kind = match kind with
  FlipCondition -> new flipConditionVisitor
         | LoopBound -> new loopBoundVisitor
         | ScanOverrun -> new scanOverrunVisitor
         | OffByOne -> new offByOneVisitor
         | CorruptParameter -> new corruptParameterVisitor
         | MissingAssignment -> new missingAssignmentVisitor
         | MissingCall -> new missingCallVisitor
         | _ -> E.error "Not implemented yet";
         new fVisitor

 let injectFaults (f: file) (k:string) =
   let fvisit = getVisitor (kind_of_string k) in
     visitCilFileSameGlobals fvisit f


let dosfi2 (f: file) (kind:string) (n: int)  =
  Printf.fprintf stderr "FAULTINJECTIONPerforming FAULT INJECTION ..\n";
  let postsfiVisitor: postsfiVisitorClass  = new postsfiVisitorClass in
  init_scan_funcs();
  nth := n;
  iterGlobals f
    (fun glob -> 
      match glob with 
        (* GFun(fdec, _) when mustSfiFunction fdec -> Commented by asim and
         * below line added  *) 
        GFun(fdec, _) when mustSfiFunction2 fdec -> 
            ignore (visitCilFunction postsfiVisitor fdec);
            (*ignore (visitCilFunction postsfiVisitor fdec)
             ignore (visitCilFunction postsfiVisitor fdec)*)

      | _ -> ());
  (* Now do visit 2 *)
    visitCilFileSameGlobals (postsfiVisitor :> cilVisitor) f;
    injectFaults f kind



let feature : featureDescr = 
  { fd_name = "sfi2";
    fd_enabled = dofault;
    fd_description = "static fault injection";
    fd_extraopt = [
        "--flipcount", Arg.Int (fun s -> nth:=s; Printf.fprintf stderr
        "**Got flip %d.\n" !nth;), " No of instructuions to flip";
    "--faulttype", Arg.String (fun s -> Printf.fprintf stderr "FAULTINJECTION**Got %s.\n" s ; kindarg := s), " Fault injection type";
    ];
    fd_doit = (function (f: file) -> dosfi2 f !kindarg !nth); 
    fd_post_check = true;
  } 

