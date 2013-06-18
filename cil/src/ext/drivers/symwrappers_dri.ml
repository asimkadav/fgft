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
  
class symwrappers_dri = object (self)
  inherit nopCilVisitor

  (* Keeps only function declarations (prototypes), not definitions.
     Returns:
     true = keep
     false = discard
  *)
  method initial_filter (glob: global) : bool =
    begin
      (match glob with
         | GType(t, _) -> true; (* A typedef *)
         | GCompTag(c, _) -> false;
         | GCompTagDecl(c, _) -> false; (* A forward structure declaration *)
         | GEnumTag(e, _) -> true; (* An enum *)
         | GEnumTagDecl(e, _) -> false; (* A forward enum declaration *)
         | GVarDecl(v, _) -> false; (* A variable declaration *)
         | GVar(v, i, _) -> false; 
         | GFun(f, _) -> false;
         | GAsm(s, _) -> false;
         | GPragma(a, _) -> false;
         | GText (t) -> false;
      );
    end


  (* Visits every function. *)
  method vfunc (f: fundec) : fundec visitAction =
    begin
      (* Every function gets at least one "modif" even if it's just a
         comment.  This way, we are assured that the dump_modifs
         will output a modif for every function and not skip
         those that would otherwise be empty.
      *)
      (*let str = Printf.fprintf stderr "%s\n" f.svar.vname in*)
      DoChildren;
    end

  (* Called by do_extract_hdr *)
  method generate_wrappers
    (f: file) : unit =
    begin
      (* Executes vfunc for all functions *)
      visitCilFileSameGlobals (self :> cilVisitor) f;
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
let do_symwrappers (f: file) : unit =
  begin
    let obj : symwrappers_dri = new symwrappers_dri in
    obj#generate_wrappers f
  end
