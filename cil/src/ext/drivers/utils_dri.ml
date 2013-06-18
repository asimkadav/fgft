(*===========================================================================*)
(*
 * Device driver analysis: This file contains general utilities.
 *
 * Vinod Ganapathy <vg@cs.wisc.edu>, August 1, 2006.
 *)
(*===========================================================================*)

open Cil
open Scanf

(* All warnings: to be flushed out at the end *)
let allwarnings = ref []

let rec strip_whitespace (s: string) : string =
  if String.length s = 0 then ""
  else if String.length s = 1 then
    if String.compare s "\n" = 0 ||
      String.compare s "\t" = 0 ||
      String.compare s " " = 0 ||
      String.compare s "\r" = 0
    then
      " "
    else
      s
  else
    let s1 = String.sub s 0 1 in
    let s2 = String.sub s 1 (String.length s - 1) in
    (strip_whitespace s1) ^ (strip_whitespace s2);;

(** Warning function *)
let warning(sl: string list) : unit = 
  begin
    let print_string(s: string) : unit = (Printf.fprintf stderr " %s%!" s) in
    (Printf.fprintf stderr "!!!!! NOTE !!!!! : %!");
    (ignore (List.map print_string sl));
    (Printf.fprintf stderr "\n%!");
  end

(** Terminal warning function *)
let fatal(sl: string list) : 'a = 
  begin
    let print_string(s: string) : unit = (Printf.fprintf stderr " %s%!" s) in
    (Printf.fprintf stderr "!!!!! FATAL WARNING !!!!! : %!");
    (ignore (List.map print_string sl));
    (Printf.fprintf stderr "\n%!");
    assert(false);
  end

(* Dereference a type *)
let rec deref_ptr_typ (t: typ) : typ = 
  begin
    (match t with
       | TPtr(deref_t, _) -> deref_t; (* Get the type we point to *)
       | TNamed(tinfo, _) -> (deref_ptr_typ tinfo.ttype); (* Get rid of typedef *)
       | _ -> (fatal ["Non-pointer type passed for dereference to deref_ptr_typ"]);
    );
  end

(** add_to_list: Add an element to an input list *)
let add_to_list (elem: 'a) (inlist: 'a list) : 'a list = 
 begin
    (List.append inlist [elem]);
  end

(** remove_repeats: Returns the input list without repeated elements *)
(* We use a hash value based comparison, because direct comparison may not
 * always be possible for all input data types *)
let remove_repeats (inlist : 'a list) : 'a list = 
  begin
    let retval : 'a list ref = ref [] in
    let values_seen : (int, bool) Hashtbl.t  = (Hashtbl.create 5) in 
    let is_in_retval_list (elem: 'a) : bool = 
      begin
        let is_present = ref false in
        (try 
           let hashelem = (Hashtbl.hash elem) in
           (ignore (Hashtbl.find values_seen hashelem));
           is_present := true;
         with Not_found -> ());
        !is_present;
        (* Old implementation, that could run out of memory for certain data types.
           for i = 0 to (List.length !retval) - 1 do
           if (List.nth !retval i) = elem then is_present := true;
           done;
        *)
      end in
    for i = 0 to (List.length inlist) - 1 do
      let ith = (List.nth inlist i) in
      if (is_in_retval_list ith) = false
      then begin
        let hashval = (Hashtbl.hash ith) in
        (Hashtbl.add values_seen hashval true);
        retval := (List.append !retval [ith]);
      end
    done;
    !retval;
  end

(** add_if adds a binding to 'key' in a hashtable if it's not bound already *)
let add_if (table: ('a,'b) Hashtbl.t) (key: 'a) (data: 'b): unit = 
  begin
    if (not (Hashtbl.mem table key)) then
      (Hashtbl.add table key data)
  end

(** add_if_binding adds a binding 'bind' to 'key' if the key::bind binding does
 * not already exist *)
let add_if_binding (table: ('a,'b) Hashtbl.t) (key: 'a) (data: 'b): unit =
  begin
    let bindings_for_key = (Hashtbl.find_all table key) in
    if (List.mem data bindings_for_key) = false then
      (Hashtbl.add table key data);
  end

(** List all the keys in a hashtable *)
let list_keys (table: ('a,'b) Hashtbl.t) : 'a list =
  begin
    let retval : 'a list ref = ref [] in
    let iterfun (key : 'a) (data : 'b) : unit = 
      retval := key::!retval
    in
    begin
      (Hashtbl.iter iterfun table);
      !retval
    end
  end

(** List all the bindings in a hashtable *)
let list_bindings (table: ('a,'b) Hashtbl.t) : 'b list =
  begin
    let retval : 'b list ref = ref [] in
    let iterfun (key: 'a) (data : 'b) : unit = 
      retval := data::!retval
    in
    begin
      (Hashtbl.iter iterfun table);
      !retval
    end
  end

(** List all the key/binding pairs in a hashtable *)
let list_keybindings (table: ('a,'b) Hashtbl.t) : ('a * 'b) list =
  begin
    let retval : ('a * 'b) list ref = ref [] in
    let iterfun (key: 'a) (data : 'b) : unit = 
      retval := (key,data)::!retval
    in
    begin
      (Hashtbl.iter iterfun table);
      !retval
    end
  end

(** Two input hashtables with string keys: Are their key sets the same? 
 * Note that this will remove repeated keys, and check equality as sets *)
let are_keysets_same (tab1: (string,'a) Hashtbl.t)
    (tab2: (string,'b) Hashtbl.t) : bool =
  begin
    let sortfun s1 s2 = (String.compare s1 s2) in
    let keys_tab1 = (List.sort sortfun (remove_repeats (list_keys tab1))) in
    let keys_tab2 = (List.sort sortfun (remove_repeats (list_keys tab2))) in
    if (keys_tab1 = keys_tab2) then true else false;
  end

(** Copy hashtable in-place: 'dst' is emptied, and 'src'
 * is copied into 'dst' *)
let copy_htab_in_place (src: ('a,'b) Hashtbl.t) 
    (dst: ('a,'b) Hashtbl.t) : unit =
  begin
    let iterfun (key: 'a) (data : 'b) : unit = 
      (Hashtbl.add dst key data)
    in
    (Hashtbl.clear dst);
    (Hashtbl.iter iterfun src);
  end

(** Merge two input hashtbls into a single hashtbl. If multiple entries
 * exist in the input hashtables, they will be retained in the output
 * hashtable. If the same keys exist in both hashtables, a duplicate 
 * entry will be created. *)
let merge_hashtbls (tab1: ('a,'b) Hashtbl.t) 
    (tab2: ('a,'b) Hashtbl.t) : ('a,'b) Hashtbl.t =
  begin
    let retval = (Hashtbl.copy tab1) in
    let tab2_keybindings = (list_keybindings tab2) in
    for i = 0 to (List.length tab2_keybindings) - 1 do
      let (ithkey, ithdata) = (List.nth tab2_keybindings i) in
      (Hashtbl.add retval ithkey ithdata);
    done;
    (Hashtbl.copy retval);
  end

(** Integer to string *)
let itoa(i: int) : string = 
  begin
    (Int32.to_string (Int32.of_int i));
  end

(** Get the attributes associated with an input type *)
let get_attribs (t: typ) : attributes = 
  begin
    (match t with
       | TVoid(a) -> a;
       | TInt(_,a) -> a;
       | TFloat(_,a) -> a;
       | TPtr(_,a) -> a;
       | TArray(_,_,a) -> a;
       | TFun(_,_,_,a) -> a;
       | TNamed(_,a) -> a;
       | TComp(_,a) -> a;
       | TEnum(_,a) -> a;
       | TBuiltin_va_list(a) -> a;
    );
  end

(** Strip attributes from an input type *)
let rec strip_typ_attribs (t: typ) : typ = 
  begin
    (match t with
       | TVoid(_) -> TVoid([]);
       | TInt(k,_) -> TInt(k,[]);
       | TFloat(k,_) -> TFloat(k,[]);
       | TPtr(t',_) -> TPtr((strip_typ_attribs t'), []);
       | TArray(t',e,_) -> TArray((strip_typ_attribs t'), e, []);
       | TFun(t',l,b,_) -> TFun((strip_typ_attribs t'),l,b,[]);
       | TNamed(tinfo,_) -> TNamed(tinfo,[]);
       | TComp(cinfo,_) -> TComp(cinfo,[]);
       | TEnum(einfo,_) -> TEnum(einfo,[]);
       | TBuiltin_va_list(_) -> TBuiltin_va_list([]);
    );
  end

(* Are two input types the same? Hash the types to compare*)
let is_same_typ (t1: typ) (t2: typ) : bool = 
  begin
    let t1hash = (Hashtbl.hash t1) in
    let t2hash = (Hashtbl.hash t2) in
    if t1hash = t2hash then true else false;
  end

(** tcomp_compinfo: extract the compinfo from a TComp type. Also
 * try to do this if this is a named type *)
let rec tcomp_compinfo (t: typ) : compinfo option = 
  begin
    (match t with
       | TComp(cinfo,_) -> Some(cinfo);
       | TNamed(tinfo,_) -> (tcomp_compinfo tinfo.ttype);
       | _ -> None;
    );
  end

(** Is the input type a compound type? *)
let isCompoundType (t: typ) : bool = 
  begin
    (match (tcomp_compinfo t) with
       | Some(_) -> true;
       | None -> false;
    );
  end

(** get_fieldinfo: Get a fieldinfo from a compound type. Handle
 * exceptions here. *)
let get_fieldinfo (cinfo: compinfo) (s: string) : fieldinfo = 
  begin
    (* Initialize the return value with a dummy *)
    if (List.length cinfo.cfields) = 0 then
      (fatal ["Compound type has no fields available"]);
    let retval = ref (List.nth cinfo.cfields 0) in
    (try
       retval := (getCompField cinfo s);
     with Not_found -> ((fatal ["Could not fetch fieldinfo"])));
    !retval;
  end

(** Fieldinfo_name *)
let fieldinfo_name (f: fieldinfo) : string = 
  begin
    f.fname ^ "#" ^ f.fcomp.cname;
  end

(** Makevarname: Take the input string, and ensure that it only has characters
 * that C will accept as a character in a legal variable name. This is needed
 * because we use lval names as hints for variables, and lvalnames can contain
 * "*", " ", "(", ")", "-", and ">" *)
let makevarname (s: string) : string =
  begin
    let retval = ref "" in
    for i = 0 to (String.length s) - 1 do
      let ith = (String.get s i) in
      (match ith with
         | '>' -> ();
         | '-' -> ();
         | '*' -> ();
         | ' ' -> ();
         | '(' -> ();
         | ')' -> ();
         | '+' -> ();
         | '.' -> ();
         | ']' -> ();
         | '[' -> ();
         | _ -> retval := !retval ^ (String.make 1 ith);
      );
    done;
    !retval;
  end

(* Prettyprint a type *)
let typ_tostring (t: typ) : string =
  begin
    (* (Pretty.sprint 50 (d_typsig() (typeSig t))); *)
    (Pretty.sprint 100 (d_type() t));
  end

(* Prettyprint an instruction *)
let instr_tostring (i: instr) : string = 
  begin
    (Pretty.sprint 100 (d_instr() i));
  end
  
 (* Converts a statement to a string. *)
  let stmt_to_string (stmt: stmt) : string =
    Pretty.sprint 100 (d_stmt () stmt);;


 (* Converts a stmt list to a string.  *)
let stmtlist_tostring (stmt_list : stmt list) : string =
   let combiner (string : string) (stmt : stmt) =
     match string with
      | "" -> stmt_to_string stmt
      | _ -> string ^ "\n" ^ (stmt_to_string
           stmt)
    in
    List.fold_left combiner "" stmt_list;;

(* Prettyprint a type, discarding attributes *)
let typ_tostring_noattr (t: typ) : string = 
  begin
    let t_stripped = (strip_typ_attribs t) in
    (typ_tostring t_stripped);
  end

(* Prettyprint only the attributes of a type *)
let typ_tostring_attronly (t: typ) : string =
  begin
    let t_attronly = (get_attribs t) in
    let resultstr = ref (Printf.sprintf "%d" (List.length t_attronly)) in
    for i = 0 to (List.length t_attronly) - 1 do
      let ith = (match (List.nth t_attronly i) with
                   | Attr (name, _) -> name;
                ) in
      resultstr := !resultstr ^ " " ^ ith;
    done;
    !resultstr;
  end

(* Printing the name of an lval *)
let lval_tostring (lv: lval) : string = (Pretty.sprint 100 (d_lval() lv))

(* Printing the name of an exp *)
let exp_tostring (e: exp) : string = (Pretty.sprint 100 (d_exp() e))

(* Given a fundec, convert its parameters list into a string *)
let get_params_str_fundec (f : fundec) : string =
  begin
    let retval = ref "" in
    let num_params = List.length f.sformals in
    if num_params >= 1 then
      let last_elt = List.nth f.sformals (List.length f.sformals - 1) in
      let store_vi vi =
        let param = Pretty.sprint 80 (Pretty.dprintf "%a %s" d_type vi.vtype vi.vname) in
        retval := !retval ^ param;
        if vi.vname <> last_elt.vname then
          retval := !retval ^ ",\n"
        else
          retval := !retval ^ ""
      in
      List.iter store_vi f.sformals;
    else
      retval := !retval ^ "void";
    !retval;
  end

(* Create an expression from a fundec using the variable name *)
let expify_fundec (g: fundec) : exp = Lval(Var(g.svar),NoOffset)
  
(* Create an expression from an Lval *)
let expify_lval (l: lval) : exp = Lval(l)

(* Create an Lval from a varinfo *)
let lvalify_varinfo (v: varinfo) : lval = (Var(v),NoOffset)

(* Create an Lval from a varinfo and a field offset *)
let lvalify_varinfo_field (v: varinfo) (f: fieldinfo) : lval = 
  (Var(v), Field(f,NoOffset))

(* A wrapper for lvalify_varinfo/lvalify_varinfo_field *)
let lvalify_wrapper (v: varinfo) (fopt: fieldinfo option) : lval = 
  begin
    match fopt with
      | Some(f) -> (lvalify_varinfo_field v f);
      | None -> (lvalify_varinfo v);
  end

(** Add a field offset to an input offset: Add it at the bottom.
 * Thus, if we're given offset f.g, and we're passed field h, we
 * must make it f.g.h *)
(** Add a field offset to an lval *)
let add_field_to_lval (lv: lval) (f: fieldinfo) : lval = 
  begin
    let the_off = Field(f, NoOffset) in
    (addOffsetLval the_off lv);
  end

(** Wrapper for add_field_to_lval *)
let add_field_to_lval_wrapper (lv: lval) (fopt: fieldinfo option) : lval =
  begin
    (match fopt with
       | Some(f) -> (add_field_to_lval lv f);
       | None -> lv;
    );
  end

(** Add a field offset to an lval. Here we're provided a string as input *)
let add_field_to_lval_str (lv: lval) (field: string) : lval = 
  let lvtyp = typeOfLval lv in
  match lvtyp with
    | TComp(compinfo,_) ->
        (try 
           let finfo = getCompField compinfo field in
           add_field_to_lval lv finfo;
         with Not_found -> (
           let lvtypstr = typ_tostring lvtyp in
           fatal ["Field info not found in given type"; field; lvtypstr])
        );
    | _ -> fatal ["Non compinfo type passed to add_field_to_lval"];;
        
(* Creates an if statement given the condition, the list of statements to execute if it's true,
   and the list of statements to execute if it's false.
*)
let mkIfStmt (ifguard : exp) (ifbody : stmt list) (elsebody : stmt list) : stmt = 
  begin
    let true_block = ifbody in
    let false_block = elsebody in
    let check_trueblock = (mkBlock true_block) in
    let check_falseblock = (mkBlock false_block) in
    let check_ifstmt = If(ifguard, check_trueblock, check_falseblock, locUnknown) in
    let stmt_check = (mkStmt check_ifstmt) in
    stmt_check;
  end

let addwarn (sl : string list) : unit = 
  allwarnings := List.append !allwarnings [sl]
    
(* allwarnings is a list of string lists *)
let flushwarn () : unit = 
  begin
    (Printf.fprintf stderr "=======================================================\n");
    let warns = remove_repeats !allwarnings in
    for i = 0 to (List.length warns) - 1 do
      let ith = (List.nth warns i) in
      let currwarn = ref "[CHECK]:" in
      for j = 0 to (List.length ith) - 1 do
	let jth = (List.nth ith j) in
	currwarn := !currwarn ^ " " ^ jth;
      done;
      currwarn := !currwarn ^ "\n";
      (Printf.fprintf stderr "%s" !currwarn);
    done;
    (Printf.fprintf stderr "=======================================================\n");
  end

(** Announce message *)
let announcemsg(sl: string list) : unit = 
  begin
    let print_string(s: string) : unit = (Printf.fprintf stderr "\t%s\n%!" s) in
    (Printf.fprintf stderr "**************************************************************\n%!");
    (ignore (List.map print_string sl));
    (Printf.fprintf stderr "**************************************************************\n%!");
  end

(** Info message *)
let infomsg(sl: string list) : unit = 
  begin
    let print_string(s: string) : unit = (Printf.fprintf stderr " %s%!" s) in
    (ignore (List.map print_string sl));
    (Printf.fprintf stderr "\n%!");
  end
