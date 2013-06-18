(*********************************************)
(* Specify any other modules we're using.  These commands operate like
   java import statements.
*)
open Pretty
open Cil
open Ptranal

(*********************************************)
(* Utility Functions *)
(*********************************************)

(* Converts a typ to a string *)
let typ_to_string (t: typ) : string =
  begin
    (Pretty.sprint 100 (d_type() t));
  end

(* Converts an instr to a string *)
let instr_to_string (i: instr) : string =
  begin
    (Pretty.sprint 100 (d_instr() i));
  end

(* Converts an lval to a string *)
let lval_to_string (lv: lval) : string =
  begin
    (Pretty.sprint 100 (d_lval() lv))
  end

(* Converts an exp to a string *)
let exp_to_string (e: exp) : string =
  begin
    (Pretty.sprint 100 (d_exp() e))
  end

(* Converts a statement to a string. *)
let stmt_to_string (stmt: stmt) : string =
  Pretty.sprint 100 (d_stmt () stmt);;

(* Converts a list of statements to a string. *)
let stmt_list_to_string (stmt_list : stmt list) : string =
  let combiner (string : string) (stmt : stmt) =
    match string with
      | "" -> stmt_to_string stmt
      | _ -> string ^ "\n" ^ (stmt_to_string stmt)
  in
    List.fold_left combiner "" stmt_list;;

(* Computes the intersection of two lists. *)
let list_intersection list1 list2 =
  begin
    List.filter (fun element -> List.mem element list1) list2
  end

(* Prints a string as a line on stderr. *)
let println (channel : out_channel) (string : string) : unit =
  output_string channel (string ^ "\n");;

(* Partial application to make a function that outputs to stderr. *)
let println_err = println stderr;;

(* We're going to be printing if-statements a lot *)
(* Returns a string representation of the if-stmt parameter,
   or the empty string if it's not an if-stmt *)
let if_stmt_to_string (s: stmt) : string =
  begin
    (match s.skind with
       |If(e,_,_,_) -> (exp_to_string e);
       | _ -> "";
    );
  end

(* list_append: Append an element to a list *)
let list_append (lst : 'a list) (elt: 'a) : 'a list =
  begin
    (List.append lst [elt]);
  end

(* Calculates the cartesian product of a list and itself, excluding pairs
   consisting of the same element. This is the same as forming a complete graph
   from a list of vertices and returning a list of edges. That is, from a given
   list, this function creates list of n * (n - 1) / 2 pairs of elements. *)
let list_product_complete (list : 'a list) : ('a * 'a) list =
  let list_end = (List.length list) - 1
  and result = ref [] in
    for index1 = 0 to list_end do
      for index2 = 0 to list_end do
	if (index1 <> index2) then (
	  result := List.append !result [((List.nth list index1), (List.nth list index2))]
	)
      done
    done;
    !result

(*********************************************)
(* Functions for copying CIL datastructures. *)
(*********************************************)

(* Some of the following functions are mutually recursive, and must be defined
   in one let. They are helper functions that carry out the internal aspects of
   the code copying and should not be called directly. The callable functions
   follow. In particular, the functions are not "public" because they must keep
   track of what labels were copied so that copied gotos can be adjusted to
   refer to the copied statements identified by the copied labels. The "public"
   functions that follow do this extra bookkeeping work. *)

(* Copies a statement. *)
let rec copy_stmt_helper (statement : stmt) (copied_statements : (stmt, stmt) Hashtbl.t) : stmt =
  let statement_kind_copy = copy_skind_helper statement.skind copied_statements in
  let statement_copy = Cil.mkStmt statement_kind_copy in
    (* Add the statement and its copy to the record of copies. *)
    Hashtbl.add copied_statements statement statement_copy;
    (* Copy the labels, etc. of the statment. *)
    statement_copy.labels <- copy_labels_helper statement.labels;
    statement_copy.sid <- statement.sid;  (* This may need to be changed to a unique id. *)
    statement_copy

(* Function that creates a mappable copying of a statement. *)
and make_mappable_stmt_helper (copied_statements : (stmt, stmt) Hashtbl.t) =
  function element -> copy_stmt_helper element copied_statements

(* Copies a list of labels, making them unique. *)
and copy_labels_helper (labels : label list) : label list =
  List.map copy_label_helper labels

(* Copies a single label, making it unique by appending a string. If the label
   isn't really a label, just returns the object unmodified. *)
and copy_label_helper (label : label) : label =
  match label with
    | Label(string, location, real) -> Label(string ^ "_copy", location, real)
    | _ -> label

(* Copies the skind field of a statement. Only copies kinds with nested
   structure. Others are just returned uncopied. *)
and copy_skind_helper (statement_kind : stmtkind) (copied_statements : (stmt, stmt) Hashtbl.t) : stmtkind =
  (* Construct statement copies of the different kinds. *)
  match statement_kind with
    | If(guard_expression, block_true, block_false, location) ->
	(* Copy the blocks to a new if statement. *)
	If(guard_expression,
	   (copy_block_helper block_true copied_statements),
	   (copy_block_helper block_false copied_statements),
	   location)
    | Switch(expression, block, statement_list, location) ->
	(* Copy the block to a new switch statement. *)
	Switch(expression,
	       (copy_block_helper block copied_statements),
	       (List.map (make_mappable_stmt_helper copied_statements) statement_list),
	       location)
    | Loop(block, location, continue, break) ->
	(* Copy the block to a new loop. *)
	Loop((copy_block_helper block copied_statements), location, continue, break)
    | Block(block) ->
	(* Copy the block to a new block. *)
	Block((copy_block_helper block copied_statements))
    | TryFinally(block1, block2, location) ->
	(* Copy the blocks to a new try-finally. *)
	TryFinally((copy_block_helper block1 copied_statements),
		   (copy_block_helper block2 copied_statements),
		   location)
    | TryExcept(block1, (instruction_list, expression), block2, location) ->
	(* Copy the blocks to a new try-except. *)
	TryExcept((copy_block_helper block1 copied_statements),
		  (instruction_list, expression),
		  (copy_block_helper block2 copied_statements),
		  location)
    | Goto(statement_reference, location) ->
	(* Copy the statement reference, so when it is later modified it will not affect the original. *)
	let ref_copy = ref (!statement_reference) in
	  (* Make a new goto statement that will be modified later when it is reconnected to its label. *)
	  Goto(ref_copy, location)
    | _ ->
	(* All others, do not copy, just return the original statement kind. *)
	statement_kind

(* This deeply copies a block down to the level of individual instructions. See
   also copy_stmt. *)
and copy_block_helper (block : block) (copied_statements : (stmt, stmt) Hashtbl.t) : block =
  (* Copy the list of statements into a new block. Each statement is also copied. *)
  let block_copy = Cil.mkBlock (List.map (make_mappable_stmt_helper copied_statements) block.bstmts) in
    (* Assign the block attributes. *)
    block_copy.battrs <- block.battrs;
    block_copy
;;

(* The following functions work together to reassign the copied gotos to point
   to the copied labels. *)

(* Relinks the gotos in an skind.  *)
let rec relink_gotos_skind (statement_kind : stmtkind) (copied_statements : (stmt, stmt) Hashtbl.t) : unit =
  let mappable_relink_gotos_statement element =
    relink_gotos_statement element copied_statements
  in
    match statement_kind with
      | If(guard_expression, block_true, block_false, location) ->
	  relink_gotos_block block_true copied_statements;
	  relink_gotos_block block_false copied_statements
      | Switch(expression, block, statement_list, location) ->
	  relink_gotos_block block copied_statements;
	  List.iter mappable_relink_gotos_statement statement_list
      | Loop(block, location, continue, break) ->
	  relink_gotos_block block copied_statements
      | Block(block) ->
	  relink_gotos_block block copied_statements
      | TryFinally(block1, block2, location) ->
	  relink_gotos_block block1 copied_statements;
	  relink_gotos_block block2 copied_statements
      | TryExcept(block1, (instruction_list, expression), block2, location) ->
	  relink_gotos_block block1 copied_statements;
	  relink_gotos_block block2 copied_statements
      | Goto(statement_reference, location) ->
	  (* Look up the statement that this goto should point to and relink it to that statement. *)
	  (* If the statement wasn't copied, it won't be in the hash table, so just leave it. *)
	  if Hashtbl.mem copied_statements !statement_reference then (
	    statement_reference := Hashtbl.find copied_statements !statement_reference;
	  ) else (
	    println_err "Goto target statement ((";
	    println_err (stmt_to_string !statement_reference);
	    println_err ")) not found in table of copied statements.";
	    println_err "Referencing goto not relinked and may be broken.\n"
	  )
      | _ -> ()

(* Relinks the gotos in a block. *)
and relink_gotos_block (block : block) (copied_statements : (stmt, stmt) Hashtbl.t) : unit =
  let mappable_relink_gotos_statement element =
    relink_gotos_statement element copied_statements
  in
    List.iter mappable_relink_gotos_statement block.bstmts

(* Relinks the gotos in a statement. *)
and relink_gotos_statement (statement : stmt) (copied_statements : (stmt, stmt) Hashtbl.t) : unit =
  relink_gotos_skind statement.skind copied_statements
;;

(* This deeply copies a statement down to the level of individual
   instructions. That is, all nested code structures are copied, but the
   instructions are not copied. CFG-related aspects of the CIL data structures
   are not copied. In general, blocks are copied, expressions are not. After the
   copying is complete, the gotos and labels are rematched so that the control
   flow of the copied code is identical to the original. If this is not done,
   gotos will refer to labels in the original and not the copy. *)
let copy_stmt (statement : stmt) : stmt =
  (* Make a hash table to hold the mapping of original to copied statements. I
     expect no more than 100 statements. *)
  let statements_to_copies = Hashtbl.create 100 in
  let statement_copy = copy_stmt_helper statement statements_to_copies in
    relink_gotos_statement statement_copy statements_to_copies;
    statement_copy
;;

(*********************************************)
(* Classes and data structures. *)
(*********************************************)

(* The idea is to store the pairs of if-statements that are correlated along
   with all the statements inbetween. *)
type pair_of_ifs = {
  mutable if_stmt1 : stmt;   (* The first if-stmt *)
  mutable if_stmt2 : stmt;   (* The second if-stmt *)
  mutable var_list1 : lval list; (* The l-values in the first if-stmt expression *)
  mutable var_list2 : lval list; (* The l-values in the second if-stmt expression *)
  mutable bl : block;        (* The block this pair of statements shares *)
  mutable stmts : stmt list; (* Statments in between the two ifs *)
};;

let pair_of_ifs_to_string (pair : pair_of_ifs) : string =
  let lval_combiner (string : string) (lval : lval) =
    if (String.length string = 0) then (
      lval_to_string lval
    ) else (
      string ^ ", " ^ (lval_to_string lval)
    )
  in
  let lval_list_to_string (lval_list : lval list) =
    List.fold_left lval_combiner "" lval_list
  in
  let header = "pair_of_ifs {\n"
  and if1 = "if_stmt1: " ^ (stmt_to_string pair.if_stmt1) ^ "\n"
  and if2 = "if_stmt2: " ^ (stmt_to_string pair.if_stmt2) ^ "\n"
  and vars1 = "var_list1: " ^ (lval_list_to_string pair.var_list1) ^ "\n"
  and vars2 = "var_list2: " ^ (lval_list_to_string pair.var_list2) ^ "\n"
  and block = "bl: {\n" ^ (stmt_list_to_string pair.bl.bstmts) ^ "\n}\n"
  and statements = "stmts: {\n" ^ (stmt_list_to_string pair.stmts) ^ "\n}"
  in
    header ^ if1 ^ if2 ^ vars1 ^ vars2 ^ block ^ statements;;

(* Implements the main visitor, which gives us access to all parts of the input
   source code. *)
class refactorVisitor = object (self) (* self is equivalent to this in Java/C++ *)
  inherit nopCilVisitor

  (* Returns whether the given if statements are correlated. *)
  method are_correlated_ifs (pair : pair_of_ifs) : bool =
    match pair.if_stmt1.skind, pair.if_stmt2.skind with
      | If(guard1, _, _, _), If(guard2, _, _, _) ->
	  (* The main idea is to find out if (guard1 <=> guard2). *)
	  (* If the guards are lexically equal, then they are correlated. *)
	  (* Otherwise we have to resort to a theorem prover. *)
	  if ((exp_to_string guard1) = (exp_to_string guard2)) then
	    true
	  else
	    self#are_correlated_ifs_theorem_prover guard1 guard2
      | _, _ -> false

  (* Uses more advanced methods including a theorem prover to determine if the
     given if statements are correlated. *)
  method are_correlated_ifs_theorem_prover (guard1 : exp) (guard2 : exp) : bool =
    (* Do some checking of the expression to see if it is even worth giving to
       the theorem prover. *)
    (* Find the variables mentioned in each guard. If they don't match there is
       no way that the expressions could be equivalent. *)
    let rec contains_unhandled_bits expression =
      (* This function returns true if there is some part of the expression the
	 theorem prover cannot handle. The theorem prover will not handle
	 SizeOf, AlignOf, CastE, AddrOf and their variants or anything to do
	 with pointers. *)
      match expression with
	| Const(_) -> false
	| Lval(lval) -> (
	    match lval with (lhost, offset) -> (
	      match lhost with
		| Var(varinfo) -> false  (* Can handle variables. *)
		| Mem(exp) -> true  (* Cannot handle pointer dereferences. *)
	    )
	  )
	| UnOp(unop, exp, typ) -> (
	    match unop with
	      | BNot -> true
	      | _ -> contains_unhandled_bits exp
	  )
	| BinOp(binop, exp1, exp2, typ) -> (
	    match binop with
	      | PlusA | MinusA | Mult | Div | Mod | Lt | Gt | Le | Ge | Eq | Ne | LAnd | LOr ->
		  (contains_unhandled_bits exp1) || (contains_unhandled_bits exp2)
	      | _ -> true
	  )
	| _ -> true
    and guard1_variables = self#find_lvals_exp guard1
    and guard2_variables = self#find_lvals_exp guard2
    in
    let variables_intersection = list_intersection guard1_variables guard2_variables in
    let g1vars_length = List.length guard1_variables
    and g2vars_length = List.length guard2_variables
    and intersection_length = List.length variables_intersection
    and expression_acceptable_by_theorem_prover =
      (not (contains_unhandled_bits guard1)) && (not (contains_unhandled_bits guard2))
    in
      (*
      println_err ("Are expressions (" ^ (exp_to_string guard1) ^ ") and (" ^
		     (exp_to_string guard2) ^ ") acceptable to the theorem prover?: " ^
		     (string_of_bool expression_acceptable_by_theorem_prover) ^ "\n");
      *)
      if (g1vars_length = intersection_length) &&
	(intersection_length = g2vars_length) &&
	expression_acceptable_by_theorem_prover
      then
	self#invoke_theorem_prover guard1 guard2
      else
	false

  (* Invokes the theorem prover. *)
  method invoke_theorem_prover (guard1 : exp) (guard2 : exp) : bool =
    (* Helper functions. *)
    let get_variables_types expression =
      let lval_to_variable_type lval =
	match lval with (lhost, offset) -> (
	  match lhost with
	    | Var(varinfo) -> Some(varinfo.vname, varinfo.vtype)
	    | _ -> None
	)
      in
	List.map lval_to_variable_type (self#find_lvals_exp expression)
    and variables_types_to_string variables_types =
      let variable_type_to_string variable_type =
	match variable_type with
	  | Some(vname, vtype) -> (
	      match vtype with
		| TInt(_, _) -> vname ^ ": INT;"
		| TFloat(_, _) -> vname ^ ": REAL;"
		| _ -> vname ^ ": TYPE;"
	    )
	  | None -> ""
      and string_combiner string next_string =
	if (String.length string) = 0 then
	  next_string
	else
	  string ^ "\n" ^ next_string
      in
	List.fold_left string_combiner "" (List.map variable_type_to_string variables_types)
    and translate_expression_to_prover_syntax expression =
      (* The following translations need to take place: '==' -> '=', '!=' ->
	 '/=', '!' -> 'NOT ', '&&' -> 'AND', '||' -> 'OR'. *)
      Str.global_replace (Str.regexp_string "==") "="
	(Str.global_replace (Str.regexp_string "!") "NOT "
	   (Str.global_replace (Str.regexp_string "!=") "/="
	      (Str.global_replace (Str.regexp_string "&&") "AND"
		 (Str.global_replace (Str.regexp_string "||") "OR" expression))))
    and read_input_into_buffer input buffer =
      try
	while true do
	  Buffer.add_string buffer (input_line input);
	  Buffer.add_string buffer "\n"
	done
      with End_of_file -> ()
    and check_prover_output output =
      if (output = "Valid.\n") then (
	println_err "Prover says guards are equivalent. Returning true.\n";
	true
      ) else (
	println_err "Prover says guards are different. Returning false.\n";
	false
      )
    and guard1_string = exp_to_string guard1
    and guard2_string = exp_to_string guard2
    in
      (* Display message that theorem prover will be invoked. *)
      println_err "\nInvoking theorem prover to solve:";
      println_err ("((" ^ guard1_string ^ ") <=> (" ^ guard2_string ^ ")) ?");

      (* Gather all the variables and their types. *)
      let variables = variables_types_to_string (get_variables_types guard1)
      (* Translate the guards to a query for the theorem prover. *)
      and g1string_translated = translate_expression_to_prover_syntax guard1_string
      and g2string_translated = translate_expression_to_prover_syntax guard2_string
      in
	(* Set up for prover query. *)
      let prover_query = variables ^ "\nQUERY ((" ^ g1string_translated ^ ") <=> (" ^ g2string_translated ^ "));\n"
      and prover_exe_name = "../tools/cvc3-1.2.1/bin/cvc3"
      in
	(* Start prover as Unix process where we control input and output and error. *)
      let prover_output, prover_input, prover_error = Unix.open_process_full prover_exe_name [||]
      and prover_output_buffer = Buffer.create 128
      and prover_error_buffer = Buffer.create 256
      in
	(* Display assembled query. *)
	println_err "Prover query: <<";
	println_err (prover_query ^ ">>");

	(* Feed query to prover. *)
	output_string prover_input prover_query;
	close_out prover_input;

	(* Read output of prover. *)
	read_input_into_buffer prover_output prover_output_buffer;
	close_in prover_output;

	(* Read error of prover. *)
	read_input_into_buffer prover_error prover_error_buffer;
	close_in prover_error;

	(* Terminate prover process. *)
	let prover_result = Unix.close_process_full (prover_output, prover_input, prover_error)
	and prover_output_string = Buffer.contents prover_output_buffer
	and prover_error_string = Buffer.contents prover_error_buffer
	in
	  (* Report the prover's result for human inspection. *)
	  println_err "Prover output: <<";
	  println_err (prover_output_string ^ ">>");
	  println_err "Prover error: <<";
	  println_err (prover_error_string ^ ">>");

	  (* Examine the prover's result. *)
	  match prover_result with
	    | Unix.WEXITED(code) ->
		(* The prover exited normally with the given code. *)
		println_err ("The prover exited normally with code: " ^ (string_of_int code));
		if (code = 0) then (
		  (* Check the prover output for success. *)
		  check_prover_output prover_output_string
		) else (
		  println_err "Prover failed. Returning false.\n";
		  false  (* Prover failed, so return false. *)
		)
	    | _ ->
		(* Some error occurred. Say so and return false. *)
		println_err "The prover did not exit normally (due to a signal).\n";
		false

  (* Returns true if any of the variables in the second if statement are
     assigned to in between the two if statements. If so, the branches are not
     necessarily correlated. *)
  method variables_changed (pair: pair_of_ifs) : bool =
    begin
      let if1 = pair.if_stmt1 in
	match if1.skind with
          | If(_,true_block,false_block,_) ->
              let true_branch_list = self#find_lvals_stmt_list true_block.bstmts
              and false_branch_list = self#find_lvals_stmt_list false_block.bstmts
              and inbetween_list = self#find_lvals_stmt_list pair.stmts in
              let all_lvals_assigned =
		(List.append true_branch_list
                   (List.append false_branch_list inbetween_list)) in
              let intersection = list_intersection all_lvals_assigned pair.var_list1 in
		
		(*
		Printf.fprintf stderr "All Lvals Assigned: ";
		List.iter (function lval -> (Printf.fprintf stderr "%s " (lval_to_string lval))) all_lvals_assigned;
		Printf.fprintf stderr "\n";

		Printf.fprintf stderr "Intersection: ";
		List.iter (function lval -> (Printf.fprintf stderr "%s " (lval_to_string lval))) intersection;
		Printf.fprintf stderr "\n";
		*)

		List.length intersection > 0;
          | _ -> (Printf.fprintf stderr "YOU IDIOT"); false;
    end
      
  (* Helper function, prints out comments for particular kinds of
     statements. This method does not do anything but print to the screen. *)
  method print_stmt (s: stmt) : unit =
    begin
      (match s.skind with
         | Instr(ins) ->
             begin
               let printer = (function x -> (Printf.fprintf stderr "Instr: %s\n" (instr_to_string x)); x) in
               let res = (List.map printer ins) in
		 (ignore res);
             end
         | Return(_,_) -> (Printf.fprintf stderr "Return\n\n");
         | Block(_) -> (Printf.fprintf stderr "Block in print_stmt\n");
         | If(e,_,_,_) ->
             begin
               (Printf.fprintf stderr "If %s\n" (exp_to_string e));
             end
         | _ ->  (Printf.fprintf stderr "Other\n");
      );
    end

  (* Finds all the lvals (variables) in a CIL instruction. *)
  method find_lvals_instr (i: instr) : lval list =
    begin
      match i with
        | Set(lval, exp, _) -> 
            lval::[];
        | Call(lval_option, exp, exp_list, _) ->
            begin
              match lval_option with
                | Some (lval) -> lval :: [];
                | _ -> [];
            end
        | _ -> [];
    end

  (* Finds all the lvals (variables) in a list of CIL instructions. *)
  method find_lvals_instr_list (i: instr list) : lval list =
    begin
      List.flatten (List.map self#find_lvals_instr i)
    end

  (* Finds all the lvals (variables) in a CIL statement. *)
  method find_lvals_stmt (s: stmt) : lval list =
    begin
      (match s.skind with
         | Instr(ins_list) -> self#find_lvals_instr_list ins_list
         | Return(_,_) -> [];
         | If(_, block1, block2, _) -> 
             List.append
               (self#find_lvals_stmt_list block1.bstmts)
               (self#find_lvals_stmt_list block2.bstmts)
         | Switch(_, block, slist, _) ->
             List.append
               (self#find_lvals_stmt_list block.bstmts)
               (self#find_lvals_stmt_list slist)
         | Loop (block, _, stmt_option1, stmt_option2) ->
             List.append
               (self#find_lvals_stmt_list block.bstmts)
               (List.append
                  (match stmt_option1 with
                     | Some (stmt) -> self#find_lvals_stmt stmt
                     | _ -> []
                  )
                  (match stmt_option2 with
                     | Some (stmt) -> self#find_lvals_stmt stmt
                     | _ -> []
                  )
               )
         | Block(block) ->
             self#find_lvals_stmt_list block.bstmts
         | TryFinally(block1, block2, _) ->
             List.append
               (self#find_lvals_stmt_list block1.bstmts)
               (self#find_lvals_stmt_list block2.bstmts)
         | TryExcept(block1, (ins_list, _), block2, _) ->
             List.append
               (self#find_lvals_stmt_list block1.bstmts)
               (List.append
                  (self#find_lvals_instr_list ins_list)
                  (self#find_lvals_stmt_list block2.bstmts))
         | _ -> [];
      );
    end

  (* Finds all the lvals (variables) in a list of CIL statements. *)
  method find_lvals_stmt_list (lst: stmt list) : lval list =
    let unaliased_list = List.flatten (List.map self#find_lvals_stmt lst) in
    let addrof_unaliased_list = List.map mkAddrOf unaliased_list in
    let exp_varinfo_list =
      try List.flatten (List.map Ptranal.resolve_exp addrof_unaliased_list)
      with Not_found -> [] in
    let lval_varinfo_list = (* List of varinfos *)
      try List.flatten (List.map Ptranal.resolve_lval unaliased_list)
      with Not_found -> [] in
    let alias_list = List.append exp_varinfo_list lval_varinfo_list in
    let alias_lval_list = List.map var alias_list in (* List of lvals that are pointed to by any of
                                                        the variables that were assigned *)
    List.append unaliased_list alias_lval_list;

  method find_lvals_exp (e: exp) : lval list =
    begin
      (match e with
         | Const(c) -> 
             []; (* Constant *)
         | Lval(l) -> (* Lvalue *)
             l::[];
         | SizeOf(s) -> 
             []; (* SizeOf(type) *)
         | SizeOfE(e) ->
             (* (self#analyze_exp e);  (* SizeOf(expression) *) *)
             [];
         | SizeOfStr(s) -> 
             []; (* SizeOfStr, as in sizeof ("strlit") *)
         | AlignOf(t) -> 
             []; (* Corresponds to the GCC __alignof__ *)
         | AlignOfE(e) -> 
             (* (self#analyze_exp e);*)
             [];
         | UnOp(op, e, t) -> 
             (self#find_lvals_exp e); (* Unary operator, includes type of result *)
         | BinOp(b, e1, e2, t) -> 
             (* Binary operator, includes type of result *)
             (List.append (self#find_lvals_exp e1) (self#find_lvals_exp e2));
         | CastE(t, e) -> 
             (self#find_lvals_exp e); (* Cast *)
         | AddrOf(l) -> 
             l::[]; (* Address of (lval) *)
         | StartOf(l) -> 
             l::[]; (* Conversion from array to a pointer to the beginning of the array *)
      );
    end
      
  (* Finds all pairs of if statements in the current block *)
  method analyze_block_find_pairs (cur_block: block) : pair_of_ifs list =
    begin
      let if_stmt_list = ref [] in
      (* This is important:  the if statements are added to this list
         in the order that we encounter them.  Thus, if-stmts at a lower
         index in this list occur earlier in the block *)
      for i = 0 to (List.length cur_block.bstmts) - 1 do
        let cur_stmt = (List.nth cur_block.bstmts i) in
        (match cur_stmt.skind with
           | If(e,_,_,_) ->
               begin
                 (* Add all if-stmts to the list *)
                 if_stmt_list := (list_append !if_stmt_list cur_stmt);
		 (* println_err ("if(" ^ (exp_to_string e) ^ ")"); *)
               end
           | _ -> ();
        );
      done;

      (* At this point, we have a list of all the if statements in the
         current block.  Now, we just need to generate all pairs of if
         statements *)
      let if_pair_list = ref [] in
      for i = 0 to (List.length !if_stmt_list) - 1 do
        for j = i to (List.length !if_stmt_list) - 1 do
          let cur_if1 = (List.nth !if_stmt_list i) in (* The earlier if *)
          let cur_if2 = (List.nth !if_stmt_list j) in (* The later if *)
          if (i <> j) then (* Don't put an if-stmt in a pair with itself *)
            begin
              (* Construct the pair *)
              let new_pair : pair_of_ifs = {
                if_stmt1=cur_if1;
                if_stmt2=cur_if2;
                var_list1 =
                  (match cur_if1.skind with
                     | If(e, _, _, _) -> self#find_lvals_exp e
                     | _ -> []
                  );
                var_list2 =
                  (match cur_if2.skind with
                     | If(e, _, _, _) -> self#find_lvals_exp e
                     | _ -> []
                  );
                bl=cur_block;
                stmts=[]
              } in
              (* See if the two if statements are correlated.
                 This is where we will want to call the theorem prover *)
              if (self#are_correlated_ifs new_pair) then
                (* If they're correlated, add the pair to the list *)
                (* At this point, we've still not established if the
                   variables constituting the two conditions are
                   modified in-between.  We do that later *)
                if_pair_list := (list_append !if_pair_list new_pair);
            end
        done
      done;

      !if_pair_list;
    end
      
  (* Finds all statements between each pair of if statements in the current block *)
  (* Note that this function does not change the contents of the current block. *)
  method analyze_block_find_stmts (cur_block: block) (if_pair_list : pair_of_ifs list) : pair_of_ifs list =
    begin
      (* Look for the first if-stmt of the pair *)
      if ((List.length if_pair_list) >= 1) then
        begin
          let the_pair = (List.nth if_pair_list 0) in
          let record = ref false in (* Save copies of the statements *)
          for i = 0 to (List.length cur_block.bstmts) - 1 do
            let cur_stmt = (List.nth cur_block.bstmts i) in
            (* (Printf.fprintf stderr "Considering: ");
            (self#print_stmt cur_stmt); *)
            
            (* We use stmt sid to compare.  This is an id
               unique to the function the stmt appears in. *)
            if (cur_stmt.sid = the_pair.if_stmt1.sid) then
              begin
                (* At this point, we are in-between the
                   pair of correlated branches *)
                (* Printf.fprintf stderr "Starting...\n"; *)
                record := true;
              end;
            
            (* Stop recording, we've hit the second branch
               in the pair *)
            if (cur_stmt.sid = the_pair.if_stmt2.sid) then
              begin
                (* Printf.fprintf stderr "Ending...\n"; *)
                record := false;
              end;
            
            (* Only add the stmts that are _between_
               the two correlated if-stmts! *)
            if (!record &&
                  cur_stmt.sid <> the_pair.if_stmt1.sid &&
                  cur_stmt.sid <> the_pair.if_stmt2.sid) then
              (* Store the statements that are in-between the two
                 if/else pairs *)
              the_pair.stmts <- (list_append the_pair.stmts cur_stmt);
          done;
        end;
      if_pair_list;
    end

  (* This function rewrites the correlated branches by eliminating the code between
     the pair, and eliminating the second if/else pair completely *)
  method refactor_branches (cur_block : block) (if_pair_list : pair_of_ifs list) : unit =
    (*
    Printf.fprintf stderr "Entering refactor_branches...\n";
    Printf.fprintf stderr "List.length if_pair_list: %i\n" (List.length if_pair_list);
    *)
    if (List.length if_pair_list >= 1) then (
      (* Look for the first if-stmt of the pair *)
      let the_pair = List.hd if_pair_list in

	(* Print out the pair_of_ifs structure to verify. *)
	(* println_err (pair_of_ifs_to_string the_pair); *)

        (* Only refactor if the variables that make up the if-stmts have not
           been modified in-between *)
        if (self#variables_changed the_pair) = false then (
	  (* Given a list of statements, and a statement, returns true if the
             statement is NOT present in the list. *)
	  let not_exists_in_list lst st = not (List.exists (function x -> st.sid = x.sid) lst)
	  and inbetween_stmts_copy = List.map copy_stmt the_pair.stmts
	  in
	    (*
	    println_err "\nThe block statements before and after filtering:";
	    println_err "------------------------------------------------------------";
	    println_err (stmt_list_to_string cur_block.bstmts);
	    println_err "------------------------------------------------------------";
	    *)

	    (* Re-sets the current block's statements to _exclude_ those located
               between the two correlated branches.  We don't want to include
               these because they are now in both the if and else branches *)
	    cur_block.bstmts <- (List.filter (not_exists_in_list the_pair.stmts) cur_block.bstmts);

	    (*
	    println_err (stmt_list_to_string cur_block.bstmts);
	    println_err "------------------------------------------------------------";
	    *)

	    (* Also remove the second if-else pair *)
	    cur_block.bstmts <- (List.filter (not_exists_in_list [the_pair.if_stmt2]) cur_block.bstmts);

	    (*
	    println_err (stmt_list_to_string cur_block.bstmts);
	    println_err "------------------------------------------------------------";
	    *)

	    (* Now we add the statements that we removed to the first
               if-statement.  The second if statement is gone as far as the
               block data structure is concerned, but we still have copies of
               all the statements that made it up inside the the_pair.if_stmt2
               structure *)
	    match the_pair.if_stmt1.skind, the_pair.if_stmt2.skind with
              | If (exA, bA1, bA2, _), If (exB, bB1, bB2, _) ->
		  (* Naming:
		     - bA1 = the "if" block of the first if-stmt
		     - bA2 = the "else" block of the first if-stmt
		     - bB1 = the "if" block of the second if-stmt
		     - bB2 = the "else" block of the second if-stmt
		  *)

		  (*
		  println_err "\nWhat there is prior to refactoring:";
		  println_err "-- if 1, true block:";
		  println_err "-- {";
		  println_err (stmt_list_to_string bA1.bstmts);
		  println_err "-- }";
		  println_err "-- if 1, false block:";
		  println_err "-- {";
		  println_err (stmt_list_to_string bA2.bstmts);
		  println_err "-- }";
		  println_err "-- The in-between statements:";
		  println_err "-- {";
		  println_err (stmt_list_to_string the_pair.stmts);
		  println_err "-- }";
		  println_err "-- The in-between statements copy:";
		  println_err "-- {";
		  println_err (stmt_list_to_string inbetween_stmts_copy);
		  println_err "-- }";
		  println_err "-- if 2, true block:";
		  println_err "-- {";
		  println_err (stmt_list_to_string bB1.bstmts);
		  println_err "-- }";
		  println_err "-- if 2, false block:";
		  println_err "-- {";
		  println_err (stmt_list_to_string bB2.bstmts);
		  println_err "-- }";
		  *)

		  bA1.bstmts <- (List.append bA1.bstmts the_pair.stmts);
		  bA1.bstmts <- (List.append bA1.bstmts bB1.bstmts);
		  bA2.bstmts <- (List.append bA2.bstmts inbetween_stmts_copy);
		  bA2.bstmts <- (List.append bA2.bstmts bB2.bstmts);
              | _, _ -> ()
        )
    )

  (* Prints if-stmt pairs in a particular block.
     Also prints the statements between the if statements,
     so it's clear what's being affected by the refactoring.
  *)
  method print_if_pairs (if_pair_list : pair_of_ifs list) : unit =
    begin
      if (List.length if_pair_list) > 0 then
        begin
          println_err "\nAll if statements in block:";
          let printer =
            function x -> 
              (* Print out the pair *)
              Printf.fprintf stderr "If #1 %s, #2 %s\n"
                (if_stmt_to_string x.if_stmt1)
                (if_stmt_to_string x.if_stmt2);
              
              (* Print out the stmts between the pair *)
              for i = 0 to (List.length x.stmts) - 1 do
                let cur_stmt = (List.nth x.stmts i) in
                self#print_stmt cur_stmt
              done;
              
              (* Print out the variables that make up the
                 expressions in the if statements *)
              Printf.fprintf stderr "Vars in if-stmt1: ";
              List.iter (function param -> Printf.fprintf stderr "%s " (lval_to_string param)) x.var_list1;
              Printf.fprintf stderr "\n";
              
              Printf.fprintf stderr "Vars in if-stmt2: ";
              List.iter (function param -> Printf.fprintf stderr "%s " (lval_to_string param)) x.var_list2;
              Printf.fprintf stderr "\n";
          in
          (List.iter printer if_pair_list);
          (Printf.fprintf stderr "-------------------\n");
        end
    end

  (* Visits every "instruction" *)
  method vinst (i: instr) : instr list visitAction =
    begin
      DoChildren;
    end

  (* Visits every "statement" *)
  method vstmt (s: stmt) : stmt visitAction =
    begin
      DoChildren;
    end

  (* Visits every block *)
  method vblock (block: block) : block visitAction =
    begin
      (* println_err ("First statement in the block we are visiting: " ^ (stmt_to_string (List.nth block.bstmts 0))); *)

      (* The main, interesting stuff. This does the refactoring on a block by
	 block basis.  *)
      let if_pair_list = self#analyze_block_find_pairs block in
	if (List.length if_pair_list) > 0 then (
	  (* Find the in-between statements and put them in the pair_of_ifs structure. *)
	  ignore (self#analyze_block_find_stmts block if_pair_list);

	  (*
	  println_err "\nThe whole block before refactoring:";
	  println_err "VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV";
	  println_err (stmt_list_to_string block.bstmts);
	  println_err "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n";
	  *)

	  (* self#print_if_pairs revised_pair_list; *)
	  self#refactor_branches block if_pair_list;
	  
	  (*
	  println_err "\nThe whole block after refactoring:";
	  println_err "VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV";
	  println_err (stmt_list_to_string block.bstmts);
	  println_err "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n";
	  *)
	);

	DoChildren;
    end

  (* Visits every block *)
  method vfunc (f: fundec) : fundec visitAction =
    begin
      (Cil.prepareCFG f);
      (Cil.computeCFGInfo f false); (* false = per-function stmt numbering,
                                       true = global stmt numbering *)
      (* Printf.fprintf stderr "\nFunction: %s\n" f.svar.vname; *)
      DoChildren;
    end

  (* Call this method to start visiting *)
  method top_level (f: file) : unit =
    begin
      (* Do some points-to analysis *)
      Ptranal.no_sub := false;
      Ptranal.analyze_mono := true;
      Ptranal.smart_aliases := false;
      Ptranal.analyze_file f; (* Performs actual points-to analysis *)
      Ptranal.compute_results false; (* Just prints the points-to-graph to screen *)
      
      (* Start the visiting to run our transformation. *)
      visitCilFileSameGlobals (self :> cilVisitor) f;
    end
end

(*********************************************)
(* Initialization / Setup *)
(*********************************************)

(* Analysis entry point *)
let doanalysis (f:file) : unit =
  begin
    println_err "";  (* Have a blank line at the beginning of our output to separate it from the gcc output. *)
    Cil.useLogicalOperators := true;
    Cil.lineDirectiveStyle := None;

    let refVisitor : refactorVisitor = new refactorVisitor in
    refVisitor#top_level f;
  end

(* Some CIL junk.  Allows us to add command line params, and specifies the entry
  point (doanalysis). *)
let feature : featureDescr =
  { fd_name = "refactor";
    fd_enabled = Cilutil.refactor;
    fd_description = "Automated Refactoring of Correlated Branches";
    fd_extraopt = [];
    fd_doit = doanalysis;
    fd_post_check = true;
  }
