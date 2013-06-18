(*===========================================================================*)
(*
 * CIL Module for device driver analysis.
 *
 * Vinod Ganapathy <vg@cs.wisc.edu>, August 1, 2006.
 *)
(*===========================================================================*)


open Cil
open Str
open Utils_dri              (* General utilities *)
open Cgcomp_dri             (* Callgraph-manipulating functions *)
open Root_annot_dri         (* Extract root annotations *)
open Annotator_dri          (* Propagate root annotations *)
open Marshal_dri            (* Argument copying analysis *)
open Marshannot_dri         (* Marshaling annotations *)
open Splitter_dri           (* Splitter *)
open Modifs_dri             (* Generates modif annotations and print them to the screen *)
open Symwrappers_dri        (* Generates wrappers for kernel functions *)

let rootannot_file = ref "" (* Normally call it ANNOT.txt *)
let colormap_file = ref ""  (* Normally COLORS.txt *)
let dotty_output = ref ""   (* Normally CG.dot *)
let annot_output = ref ""   (* Normally MYOUTPUT.txt *)
let annot_input = ref ""    (* Normally MYOUTPUT.txt *)
let entry_points = ref ""
let do_what = ref ""        (* find-roots, annot-prop, split-user,
                               split-kern, simplify-prototypes,
                               extract-globs, extract-typedefs*)

let split_driver (f:file) (user_kern : string) (feature_mode: string) : unit =
  begin 
    Marshal_dri.do_marshal_analysis f feature_mode;
    Splitter_dri.do_splitting f !annot_input !entry_points user_kern feature_mode;
    Marshannot_dri.do_perform_cleanup f;
  end
    
(*---------------------------------------------------------------------------*)
(** Toplevel function that performs device-driver analysis.
 * This function does different things based upon the value of the flag that
 * it is invoked with. It either performs annotation, or it performs splitting.
 *)
let dodriveranalysis (f:file) : unit = 
  begin
    (* Simplify the program to make it look like a CFG -- the makeCFG
     * feature of CIL. This simplifies the analysis that we have to do *)
    (ignore (Partial.calls_end_basic_blocks f));
    (ignore (Partial.globally_unique_vids f));
    Cil.iterGlobals f (fun glob -> 
                         match glob with
                           | Cil.GFun(fd,_) -> 
                               Cil.prepareCFG fd; 
                               (ignore (Cil.computeCFGInfo fd true))
                           | _ -> ()
                      );

    (* Overide some lame CIL options: *)
    (*Cil.lineDirectiveStyle := Some (LinePreprocessorOutput);*) (* Ensures the stupid #line
                                                         directives are not printed *)
    (* Options include:
       LineComment --commPrintLn
       LinePreprocessorInput   (default/no command line)
       LinePreprocessorOutput  (no command line option provided)
       None   --noPrintLn
    *)

    (match !do_what with
       | "find-roots" ->
           announcemsg ["Generating Root annotations"];
           Root_annot_dri.do_extract_rootannot f !rootannot_file;
       | "annot-prop" -> 
           announcemsg ["Generating Annotations Using Propagation on Call-Graph"];
           Annotator_dri.do_annotation f !rootannot_file !colormap_file 
             !dotty_output !annot_output;
       | "normal-user" ->
           announcemsg ["Generating user-mode split driver"];
           split_driver f "produce-user" "normal";
       | "normal-kern" ->
           announcemsg ["Generating kernel-mode split driver"];
           split_driver f "produce-kern" "normal";
       | "sym-user" ->
           announcemsg ["Generating user-mode symbolic driver"];
           split_driver f "produce-user" "sym";
       | "sym-kern" -> 
           announcemsg ["Generating kernel-mode symbolic stubs"];
           split_driver f "produce-kern" "sym";
       | "modifs" ->
           announcemsg ["Generating modif annotations"];
           Modifs_dri.do_modifs f;
       | "generate-wrappers" ->
           announcemsg ["Generating kernel wrappers"];
           Symwrappers_dri.do_symwrappers f;
       | _ -> fatal ["Unknown option"];
    );

    flushwarn();
    Printf.fprintf stderr "#### Total execution time: %f\n" (Sys.time());
  end


(*---------------------------------------------------------------------------*)
(** Usage message *)
let usage_message() : string = 
  begin
    "Parameters to --dowhat must be entered as a single string, as follows:
     * find-roots <annotfile>
     * annot-prop <root-annot> <colormap> <cost-graph-dotty> <annot-out>
     * sym-user <annot-in> <java-fn-input>
     * sym-kern <annot-in>
     * modifs
     * generate-wrappers"
  end


(*---------------------------------------------------------------------------*)
(** Parse the input parameters, and figure out what analysis to do, and the
 * files that contain relevant information *)
let parse_instructions() : Arg.spec =
  begin
    let cmdparse (args: string) : unit =
      begin
        let argslist = (Str.split (Str.regexp "[ \t]+") args) in
        if (List.length argslist) = 0 
        then (fatal ["Error parsing cmdline args: 0 args given\n"; usage_message()]);
        let dowhat = (List.nth argslist 0) in
        (match dowhat with
           | "find-roots" ->
               (try
                  do_what := "find-roots";
                  rootannot_file := List.nth argslist 1;
                with Failure(_) -> (fatal ["Insufficient arguments: 1 needed\n"; usage_message()]));
           | "annot-prop" -> 
               (try
                  do_what := "annot-prop";
                  rootannot_file := List.nth argslist 1;
                  colormap_file := List.nth argslist 2;
                  dotty_output := List.nth argslist 3;    
                  annot_output := List.nth argslist 4;
                with Failure(_) -> (fatal ["Insufficient arguments: 4 needed\n"; usage_message()]));
           | "normal-user" ->
               (try
                  do_what := "normal-user";
                  annot_input := List.nth argslist 1;
                  entry_points := List.nth argslist 2;
                with Failure(_) -> (fatal ["Insufficient arguments: 2 needed\n"; usage_message()]));
           | "normal-kern" ->
               (try
                  do_what := "normal-kern";
                  annot_input := List.nth argslist 1;
                with Failure(_) -> (fatal ["Insufficient arguments: 1 needed\n"; usage_message()]));
           | "sym-user" ->
               (try
                  do_what := "sym-user";
                  annot_input := List.nth argslist 1;
                  entry_points := List.nth argslist 2;
                with Failure(_) -> (fatal ["Insufficient arguments: 2 needed\n"; usage_message()]));
           | "sym-kern" ->
               (try
                  do_what := "sym-kern";
                  annot_input := List.nth argslist 1;
                with Failure(_) -> (fatal ["Insufficient arguments: 1 needed\n"; usage_message()]));
           | "modifs" ->
               do_what := "modifs";
           | "generate-wrappers" ->
               (try
                  do_what := "generate-wrappers";
                  annot_input := List.nth argslist 1;
                with Failure(_) -> (fatal ["Insufficient arguments: 1 needed\n"; usage_message()]));
           | _ -> (fatal ["Unknown cmdline argument"; dowhat; "\n"; usage_message()]));
      end in
    (Arg.String cmdparse);
  end

(*---------------------------------------------------------------------------*)
(** CIL feature descriptions *)
let feature : featureDescr = 
  { fd_name = "drivers";
    fd_enabled = Cilutil.drivers;
    fd_description = "device-driver analysis";
    fd_extraopt = 
      [("--dofuncptr",
        Arg.Bool (fun f -> Cgcomp_dri.do_function_pointer_analysis := f),
        "<true/false> do/don't do function pointer analysis");
       ("--dobecons",
        Arg.Bool (fun f -> Marshal_dri.do_be_conservative := f),
        "<true/false> be/don't be conservative when generating marshcode for internal kernel functions");
       ("--do_simple_m_dm",
        Arg.Bool (fun f -> Marshal_dri.do_simple_m_dm := f),
        "<true/false> true = use the same marsh/demarsh code for all fns, false = specialize m/dm code per fn");
       ("--do-symdriver-test",
        Arg.Bool (fun f -> Splitter_globals_dri.do_symdriver_test := f),
        "<true/false> true = Generate calls to _check testing functions.  false = don't generate calls");
       ("--dovoidptr",
        Arg.Bool (fun f -> Marshal_dri.do_void_ptr := f),
        "<true/false> true = Do void pointer analysis, false = don't and treat void * as opaque / noderef");
       ("--dowhat", parse_instructions(), usage_message());
      ];
    fd_doit = dodriveranalysis;
    fd_post_check = true
  }

(*===========================================================================*)
