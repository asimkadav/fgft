(*===========================================================================*)
(*
 * CIL module to generate XDR specification automatically.
 *
 * Matt Renzelmann <mjr@cs.wisc.edu> March 15 2010
 * 
 *)
(*===========================================================================*)

open Cil
open Scanf
open Utils_dri            (* General utilities *)
open Marshannot_dri       (* For handling array size annotation *)
  
class modifs_dri = object (self)
  inherit nopCilVisitor

  (* User or Kern annotations for each function *)
  val mutable read_list : lval list = [];
  val mutable write_list : lval list = [];
  val mutable current_function : fundec option = None;
  val mutable print_strings : (fundec option, string list) Hashtbl.t = Hashtbl.create 117;

  (* Helper function to add a string to the list *)
  method get_existing_modif (f : fundec option) : string list =
    begin
      try
        let found_list = Hashtbl.find print_strings f in
        found_list
      with Not_found ->
        []
    end

  method add_modif (f : fundec option) (s : string) : unit =
    let string_list = self#get_existing_modif f in
    if (List.mem s string_list) = false then
      let new_string_list = string_list @ [s] in
      Hashtbl.replace print_strings f new_string_list;
      
  (* Helper functions, returns true if a varinfo is actually a parameter
   * or a local variable.
   *)
  method is_vi_a_param (vi : varinfo) : bool =
    match current_function with
      | Some(f) -> 
          if List.mem vi f.sformals then
            true
          else
            false;
      | None ->
          Printf.fprintf stderr "Error: current_function = None!\n";
          false;
          
  (* True if the expression contains a parameter to the current function, e.g.
     aparam->b->c returns true if aparam is parameter 
  *)
  method is_exp_a_param (e : exp) : bool =
    match e with
      | Const(_) -> false;
      | Lval(lv) -> self#is_lv_a_param lv;
      | SizeOf(_) -> false;
      | SizeOfE(_) -> false;
      | SizeOfStr(_) -> false;
      | AlignOf(_) -> false;
      | AlignOfE(_) -> false;
      | UnOp(_, exp1, _) -> self#is_exp_a_param exp1;
      | BinOp(_, exp1, exp2, _) -> (self#is_exp_a_param exp1) || (self#is_exp_a_param exp2);
      | CastE(_, exp1) -> self#is_exp_a_param exp1;
      | AddrOf(lv) -> self#is_lv_a_param lv;
      | StartOf(lv) -> self#is_lv_a_param lv;
          
  (* Return true if the provided lval is a parameter to the function *)
  method is_lv_a_param (lv : lval) : bool =
    let (host, off) = lv in
    match host with 
      | Var(vi) -> self#is_vi_a_param vi;
      | Mem(e) -> self#is_exp_a_param e;
          
  (* Visits every function. *)
  method vfunc (f: fundec) : fundec visitAction =
    begin
      read_list <- [];
      write_list <- [];

      (* Every function gets at least one "modif" even if it's just a
         comment.  This way, we are assured that the dump_modifs
         will output a modif for every function and not skip
         those that would otherwise be empty.
      *)
      let str = Printf.sprintf "    /* %s generated automatically */" f.svar.vname in
      self#add_modif (Some f) str;
      current_function <- Some (f);
      DoChildren;
    end

  (* Checking for writes *)
  method write_should_print (l : lval list) (lv : lval) : bool =
    (List.mem lv l = false) && (* Not in the list *)
      (self#is_lv_a_param lv); (* It's a param  *)

  method write_lval (lv : lval) : unit =
    if self#write_should_print write_list lv then
      begin
        write_list <- write_list @ [lv];
        let str = Pretty.sprint 80 (Pretty.dprintf "    MODIFIES(%a);" d_lval lv) in
        self#add_modif current_function str;
      end

  method function_call (e : exp) (args_list : exp list) : unit =
    match e with
      | Lval(lv) ->
          begin
            let (lhost, offset) = lv in
            match lhost with
              | Var(vi) ->
                  let call_str = Printf.sprintf "    // MODIFANNOT(%s)(" vi.vname in
                  let params_str = self#get_params_str_explist args_list in
                  let closing_str = ");" in
                  self#add_modif current_function (call_str ^ params_str ^ closing_str);
              | _ ->
                  let str = Pretty.sprint 80 (Pretty.dprintf "    // Missed call %a" d_lval lv) in
                  self#add_modif current_function str;
          end
      | _ ->
          let str = Pretty.sprint 80 (Pretty.dprintf "    // Missed call %a" d_exp e) in
          self#add_modif current_function str;

  (* Visit every instr.  This is used for finding writes to
     variables/parameters
  *)
  method vinst (i: instr) : instr list visitAction =
    begin
      match i with
        | Set(lv, e, l) ->
            self#write_lval lv;
        | Call(lv, f_exp, args, l) ->
            begin
              match lv with
                | Some(l) -> self#write_lval l;
                | None -> ();
            end;
            self#function_call f_exp args;
        | _ -> ();
    end;
    DoChildren;
          
  method read_should_print (lv : lval) : bool =
    if List.mem lv read_list = false then
      self#read_lhost lv
    else
      false;

  method read_lhost (lv : lval) : bool =
    let (host, offset) = lv in
    match host with
      | Var(vi) ->
          self#is_vi_a_param vi;
      | Mem(e) ->
          self#is_exp_a_param e;
         
  method read_lval (lv : lval) : unit =
    if self#read_should_print lv then
      begin
        read_list <- read_list @ [lv];
        let str = Pretty.sprint 80 (Pretty.dprintf "    READS(%a);" d_lval lv) in
        self#add_modif current_function str;
      end
                
  method vexpr (e: exp) : exp visitAction =
    begin
      match e with
        | Lval(lv) ->
            self#read_lval lv
        | AddrOf(lv) ->
            self#read_lval lv
        | StartOf(lv) ->
            self#read_lval lv
        | _ -> ()
    end;
    DoChildren

  method get_ret_string (f : fundec) : string = 
    let typ = f.svar.vtype in
    match typ with
      | TFun (t, _, _, _) -> Pretty.sprint 80 (Pretty.dprintf "%a" d_type t)
      | _ -> "";

  (* Given a list of expressions, returns them in string form
     separated with commas *)
  method get_params_str_explist (list : exp list) : string =
    let retval = ref "" in
    let num_params = List.length list in
    if num_params >= 1 then
      let store_exp e = exp_tostring e in
      retval := (String.concat ", " (List.map store_exp list));
    else
      retval := !retval ^ "void";
    !retval;

  method print_fundec (f : fundec) : unit =
    let ret_string = self#get_ret_string f in
    let param_string = get_params_str_fundec f in
    Printf.fprintf stderr "%s MODIFANNOT(%s)(%s) {\n" ret_string f.svar.vname param_string;

  method print_retval (t : typ) : unit =
    match t with 
      | TVoid(a) -> Printf.fprintf stderr ";";
      | TInt(k,a) -> Printf.fprintf stderr " 0;";
      | TFloat(k,_) -> Printf.fprintf stderr " 0.0;";
      | TPtr(sub_typ,a) -> Printf.fprintf stderr " NULL;";
      | TArray(sub_typ,e,a) -> Printf.fprintf stderr " error TArray;";
      | TFun(sub_typ,_,_,_) -> Printf.fprintf stderr " NULL;";
      | TNamed(tinfo,a) -> Printf.fprintf stderr " 0;";
      | TComp(cinfo,_) -> Printf.fprintf stderr " error TComp;";
      | TEnum(einfo,_) -> Printf.fprintf stderr " error TEnum;";
      | TBuiltin_va_list(_) -> Printf.fprintf stderr " error TBuiltin_va_list;";
          
  method print_retstmt (f : fundec) : unit =
    match f.svar.vtype with
      | TFun (t, _, _, _) ->
          Printf.fprintf stderr "    return";
          self#print_retval t;
          Printf.fprintf stderr "\n";
      | _ -> Printf.fprintf stderr "Error with function %s\n" f.svar.vname;

  method dump_modifs () : unit =
    let print_line str =
      Printf.fprintf stderr "%s\n" str in
    let print_batch f str_list =
      match f with
        | Some (f) ->
            let sorted_list = List.sort String.compare str_list in
            self#print_fundec f;
            List.iter print_line sorted_list;
            self#print_retstmt f;
            Printf.fprintf stderr "}\n\n";
        | None -> ()
    in
    Hashtbl.iter print_batch print_strings;

  (* Called by do_extract_hdr *)
  method generate_modifs
    (f: file) : unit =
    begin
      (* Executes vfunc for all functions *)
      visitCilFileSameGlobals (self :> cilVisitor) f;
      self#dump_modifs ();
    end
end
  
(* Main entry point.  Called from the CIL framework *)
(*
 * xdr_dowhat is either "java" or "c" depending on what the
 * desired XDR output format is, or its "rewrite-xdr" if the goal
 * is to modify the generated C RPC code.  xdr_name is used only
 * rewrite-xdr mode, and is used to insert the appropriately named
 * header file so as not to do the usual CIL macro expansion.
 *)
let do_modifs (f: file) : unit =
  begin
    let obj : modifs_dri = new modifs_dri in
    obj#generate_modifs f
  end
