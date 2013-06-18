(*===========================================================================*)
(*
 * CIL module to extract root annotations for different classes of drivers
 *
 * Vinod Ganapathy <vg@cs.wisc.edu>, December 4th, 2006.
 *)
(*===========================================================================*)

(* NOTE: Some annotations, such as __init, __exit, and __devexit, are better
 * derived from the original source code of the driver, because they are 
 * preprocessor macros, and are not visible to CIL *)

open Cil
open Scanf
open Utils_dri            (* General utilities *)
open Cgcomp_dri           (* Call-graph manipulating functions *)

(* General utilities *)
(* Print out a list of attributes *)
let print_attribute_list (al: Cil.attribute list) : string = 
  begin
    let retval = ref "" in
    for i = 0 to (List.length al) - 1 do
      let ith = (List.nth al i) in
      retval := !retval ^ " [" ^ (Pretty.sprint 50 (d_attr () ith)) ^ "]";
      let Attr(_,apl) = ith in 
      for j = 0 to (List.length apl) - 1 do
        let jth = (List.nth apl j) in
        retval := !retval ^ "->" ^ (Pretty.sprint 50 (d_attrparam () jth));
      done;
    done;
    !retval;
  end

(*---------------------------------------------------------------------------*)
(** Find root annotations for network/scsi/sound drivers *)
class find_driver_roots = object (self)
  inherit nopCilVisitor  
    (* Store the mapping "funcname"::"annotation" *)
  val annots : (string, string) Hashtbl.t = (Hashtbl.create 5);

  (* Check for interrupt handling functions. 
   * Function has return type irqreturn_t *)
  method is_interrupt_handler (fdec: fundec) : bool = 
    begin
      (match fdec.svar.vtype with
         | TFun(rettyp,_,_,_) -> 
             (match rettyp with
                | TNamed(tinfo,_) -> 
                    if (String.compare tinfo.tname "irqreturn_t") = 0 
                    then true else false;
                | _ -> false;
             );
         | _ -> false;
      );
    end

  (* Check whether a function name matches a given string. Useful to 
   * identify startup and shutdown functions *)
  method is_funcname_inputstring (fdec: fundec) (s: string): bool = 
    begin
      if (String.compare fdec.svar.vname s) = 0
      then true else false;
    end

  method extract_fn_name_exp(e: exp) : string =
    match e with
      | Lval(Var(v), NoOffset) ->
          begin
            Printf.fprintf stderr "ROOT 1: %s\n" v.vname;
            v.vname
          end
      | AddrOf(Var(v),NoOffset) ->
          begin
            Printf.fprintf stderr "ROOT 2: %s\n" v.vname;
            v.vname
          end
      | CastE(_, e2) -> self#extract_fn_name_exp e2
      | e ->
          begin
            Printf.fprintf stderr "ROOT 3 failed: %s\n" (exp_tostring e);
            ""
          end

  method extract_fn_name(i: init) : string =
    match i with
      | SingleInit(e) -> self#extract_fn_name_exp e
      | CompoundInit(_, _) -> "FAILURE12345";
                             
  (** Process the initializer of "struct pci driver" and extract functions of 
   * interest. Study different fields and add root labels appropriately
   * 
   * Note: This section is highly specific to the 2.6.29 kernel.  Future
   * kernels may add/remove fields from these structures, and this section
   * will need to be updated correspondingly.
   *)
  method process_initializer_index (type_str: string) (offset_cur: offset) (init_cur: init) : unit =
    match offset_cur with
      | Index(exp, NoOffset) ->
          begin
            match init_cur with
              | SingleInit(_) -> ()
              | CompoundInit(t, offsetinitlist) ->
                  self#process_compoundinit_list type_str offsetinitlist;
              | _ -> ()
          end
      | Field(finfo, NoOffset) -> 
          (* Extract the function name that is passed as the initializer *)
          let funcname = self#extract_fn_name(init_cur) in
          let add_fn kernel_name suffix =
            if (String.compare finfo.fname kernel_name) = 0 &&
              Hashtbl.mem annots funcname = false
            then
              Hashtbl.add annots funcname (kernel_name ^ suffix)
          in
          if (String.compare funcname "") <> 0 then
            if Str.string_match (Str.regexp "struct pci_driver") type_str 0 then
              begin
                add_fn "probe" "_pci";
                (*add_fn "suspend" "_pci";*)
                add_fn "resume" "_pci";
                add_fn "shutdown" "_pci";
                add_fn "remove" "_pci";
              end
            else if Str.string_match (Str.regexp "struct usb_driver") type_str 0 then
              begin
                add_fn "probe" "_usb";
                add_fn "disconnect" "_usb";
                add_fn "ioctl" "_usb";
                (*add_fn "suspend" "_usb";*)
                add_fn "resume" "_usb";
                add_fn "reset_resume" "_usb";
                add_fn "pre_reset" "_usb";
                add_fn "post_reset" "_usb";
              end
            else if Str.string_match (Str.regexp "struct net_device_ops") type_str 0 then
              begin
                add_fn "ndo_init" "";
                add_fn "ndo_uninit" "";
                add_fn "ndo_open" "";
                add_fn "ndo_stop" "";
                add_fn "ndo_start_xmit" "";
                add_fn "ndo_select_queue" "";
                add_fn "ndo_change_rx_flags" "";
                add_fn "ndo_set_rx_mode" "";
                add_fn "ndo_set_multicast_list" "";
                add_fn "ndo_set_mac_address" "";
                add_fn "ndo_validate_addr" "";
                add_fn "ndo_do_ioctl" "";
                add_fn "ndo_set_config" "";
                add_fn "ndo_change_mtu" "";
                add_fn "ndo_neigh_setup" "";
                add_fn "ndo_tx_timeout" "";
                add_fn "ndo_get_stats" "";
                add_fn "ndo_vlan_rx_register" "";
                add_fn "ndo_vlan_rx_add_vid" "";
                add_fn "ndo_vlan_rx_kill_vid" "";
                add_fn "ndo_poll_controller" "";
              end
            else if Str.string_match (Str.regexp "struct ethtool_ops") type_str 0 then
              begin
                add_fn "get_settings" "";
                add_fn "set_settings" "";
                add_fn "get_drvinfo" "";
                add_fn "get_regs_len" "";
                add_fn "get_regs" "";
                add_fn "get_wol" "";
                add_fn "set_wol" "";
                add_fn "get_msglevel" "";
                add_fn "set_msglevel" "";
                add_fn "nway_reset" "";
                add_fn "get_link" "";
                add_fn "get_eeprom_len" "";
                add_fn "get_eeprom" "";
                add_fn "set_eeprom" "";
                add_fn "get_coalesce" "";
                add_fn "set_coalesce" "";
                add_fn "get_ringparam" "";
                add_fn "set_ringparam" "";
                add_fn "get_pauseparam" "";
                add_fn "set_pauseparam" "";
                add_fn "get_rx_csum" "";
                add_fn "set_rx_csum" "";
                add_fn "get_tx_csum" "";
                add_fn "set_tx_csum" "";
                add_fn "get_sg" "";
                add_fn "set_sg" "";
                add_fn "get_tso" "";
                add_fn "set_tso" "";
                add_fn "self_test" "";
                add_fn "get_strings" "";
                add_fn "phys_id" "";
                add_fn "get_ethtool_stats" "";
                add_fn "begin" "";
                add_fn "complete" "";
                add_fn "get_ufo" "";
                add_fn "set_ufo" "";
                add_fn "get_flags" "";
                add_fn "set_flags" "";
                add_fn "get_priv_flags" "";
                add_fn "set_priv_flags" "";
                add_fn "get_sset_count" "";
                add_fn "self_test_count" "";
                add_fn "get_stats_count" "";
                add_fn "get_rxhash" "";
                add_fn "set_rxhash" "";
              end
            else if Str.string_match (Str.regexp "struct file_operations") type_str 0 then
              begin
                add_fn "llseek" "_fop";
                add_fn "read" "_fop";
                add_fn "write" "_fop";
                add_fn "aio_read" "_fop";
                add_fn "aio_write" "_fop";
                add_fn "readdir" "_fop";
                add_fn "poll" "_fop";
                add_fn "ioctl" "_fop";
                add_fn "unlocked_ioctl" "_fop";
                add_fn "compat_ioctl" "_fop";
                add_fn "mmap" "_fop";
                add_fn "open" "_fop";
                add_fn "flush" "_fop";
                add_fn "release" "_fop";
                add_fn "fsync" "_fop";
                add_fn "aio_fsync" "_fop";
                add_fn "fasync" "_fop";
                add_fn "lock" "_fop";
                add_fn "sendpage" "_fop";
                add_fn "get_unmapped_area" "_fop";
                add_fn "check_flags" "_fop";
                add_fn "flock" "_fop";
                add_fn "splice_write" "_fop";
                add_fn "splice_read" "_fop";
                add_fn "setlease" "_fop";
              end
            else if Str.string_match (Str.regexp "struct vm_operations_struct") type_str 0 then
              begin
                add_fn "open" "_vmop";
                add_fn "close" "_vmop";
                add_fn "fault" "_vmop";
                add_fn "page_mkwrite" "_vmop";
                add_fn "access" "_vmop";
              end
            else if Str.string_match (Str.regexp "struct bus_type") type_str 0 then
              begin
                add_fn "match" "_bustype";
                add_fn "uevent" "_bustype";
                add_fn "probe" "_bustype";
                add_fn "remove" "_bustype";
                add_fn "shutdown" "_bustype";
                (*add_fn "suspend" "_bustype";*)
                (*add_fn "suspend_late" "_bustype";*)
                add_fn "resume_early" "_bustype";
                add_fn "resume" "_bustype";
              end
            else
              Printf.fprintf stderr "ROOT 5 failed: %s\n" type_str;
      | _ -> ();

  method process_compoundinit_list (type_str: string) (offinitlist: (offset * init) list) : unit =
    for j = 0 to (List.length offinitlist) - 1 do
      let (offset_cur, init_cur) = List.nth offinitlist j in
      self#process_initializer_index type_str offset_cur init_cur;
    done;
    
  method process_initializer (i: init) (type_str : string) : unit = 
    match i with
      | SingleInit(_) -> ();
      | CompoundInit(_,offinitlist) ->
          self#process_compoundinit_list type_str offinitlist;
          
  (** Process the initialization of globals and extract root annotations 
   * of interest.
   * 
   * Note: This strategy could yield false positives if, for some bizarre
   * reason, the driver allocates other instances of these structures but
   * does not subsequently pass them to the kernel via the appropriate call,
   * e.g. pci_register_driver or by setting dev->netdev_ops = &driver_netdev_ops;
   *)
  method process_glob_inits (g: global) : unit = 
    match g with 
      | GVar(v,initopt,_) ->
          let struct_type_str = typ_tostring v.vtype in
          begin
            match initopt.init with
              | Some(initalizer) -> 
                  self#process_initializer initalizer struct_type_str;
              | None -> ();
          end
      | _ -> ()
                
  (* Function visitor *)
  method vfunc (fdec: fundec) : fundec visitAction = 
    begin
      (** Generic annotations **)
      (* Interrupt handler *)
      if (self#is_interrupt_handler fdec)
      then begin
        (Hashtbl.add annots fdec.svar.vname "interrupt");
        (* (Hashtbl.add annots fdec.svar.vname "Critical"); *)
        (*(Hashtbl.add annots fdec.svar.vname "kern"); *)
      end;
      (* Startup function *)
      if (self#is_funcname_inputstring fdec "init_module") 
      then begin
        (Hashtbl.add annots fdec.svar.vname "init_module");
        (* (Hashtbl.add annots fdec.svar.vname "StartStop"); *)
        (*(Hashtbl.add annots fdec.svar.vname "user");*)
      end;
      (* Cleanup function *)
      if (self#is_funcname_inputstring fdec "cleanup_module")
      then begin
        (Hashtbl.add annots fdec.svar.vname "cleanup_module");
        (* (Hashtbl.add annots fdec.svar.vname "StartStop"); *)
        (*(Hashtbl.add annots fdec.svar.vname "user");*)
      end;

      DoChildren;
    end

  (**************************************************************************)
  (* Top-level function *)
  method find_roots (f: file) : (string, string) Hashtbl.t =
    begin
      visitCilFile (self :> cilVisitor) f;
      List.iter self#process_glob_inits f.globals;
      Hashtbl.copy annots;
    end
end

(*---------------------------------------------------------------------------*)
let print_annotations (out: out_channel) 
    (annot: (string, string) Hashtbl.t) : unit =
  let kb = (remove_repeats (list_keybindings annot)) in
  for i = 0 to (List.length kb) - 1 do
    let (ithfirst,ithsecond) = (List.nth kb i) in
    Printf.fprintf out "%s %s\n%!" ithfirst ithsecond;
  done
  
(*---------------------------------------------------------------------------*)
(** Main function to call the root-annotation extractor *)
let do_extract_rootannot (f: file) (outfile: string) : unit =
  let obj : find_driver_roots = new find_driver_roots in
  let annot = obj#find_roots f in
  try
    let outstream = (open_out outfile) in
    print_annotations outstream annot;
    close_out outstream;
    print_annotations stderr annot;
  with Sys_error(error) -> fatal ["Output file not found:\n%!"; outfile];
    
