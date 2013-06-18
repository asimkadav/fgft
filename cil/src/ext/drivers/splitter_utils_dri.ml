open Cil
open Str
open Scanf
open Utils_dri
open Cgcomp_dri    (* Callgraph: we need this to find interface functions *)
open Marshannot_dri
open Splitter_globals_dri

(* Function ID mapping *)
let function_id_mappings : (string, int) Hashtbl.t = Hashtbl.create 117

(* Globals to register *)
let lvals_to_register = ref []

(* Create and return the fundec of the disp_user/disp_kern function *)
let get_disp_userkern_fundec (fname : string) : (fundec * varinfo * varinfo) =
  begin
    let disp_userkern_fundec = emptyFunction fname in
    let disp_userkern_function_typ = TFun(TInt(IInt, []), Some([]), false, []) in
    setFunctionType disp_userkern_fundec disp_userkern_function_typ;
    let arg1 = makeFormalVar disp_userkern_fundec
      "function_name"
      (TPtr(TInt(IChar, []), [])) in
    let arg2 = 
      makeFormalVar disp_userkern_fundec
        "rqargs"
        (TPtr(struct_reqargs_typ, [])) in
    disp_userkern_fundec.svar.vstorage <- Extern;
    (disp_userkern_fundec, arg1, arg2);
  end

let disp_userkern_function_typ (fname : string) : typ =
  begin
    let (disp_userkern_fundec, _, _) = get_disp_userkern_fundec fname in
    disp_userkern_fundec.svar.vtype;
  end

(* Get a varinfo from an lval. Do this only if the lval has no offset.
 * Error otherwise *)
let obtain_varinfo_from_lval (lv: lval) : varinfo =
  begin
    let lvstr = (lval_tostring lv) in
    let (lh, off) = lv in
    match off with
      | NoOffset ->
          (match lh with
             | Var(v) -> v;
             | Mem(_) -> fatal ["obtain_varinfo_from_lval error 1"; lvstr];
          );
      | _ -> fatal ["obtain_varinfo_from_lval error 2"; lvstr];
  end

(* Create an expression list from formal parameters *)
let expify_varinfos (vi_list: varinfo list) : exp list =
  begin
    let expify_varinfo (v: varinfo) = expify_lval (lvalify_varinfo v) in
    List.map expify_varinfo vi_list;
  end

let lvalify_varinfos (vi_list: varinfo list) : lval list =
  begin
    List.map lvalify_varinfo vi_list
  end

let expify_lvals (lval_list: lval list) : exp list =
  begin
    List.map expify_lval lval_list
  end

(* Ensure that the formals in an input fundec have names *)
let ensure_formals_have_names (fdec: fundec) : unit =
  begin
    let formals = fdec.sformals in
    let newformals = ref [] in
    let need_to_replace = ref false in
    for i = 0 to (List.length formals) - 1 do
      let ith = (List.nth formals i) in
      if (String.compare ith.vname "") = 0
      then begin
        ith.vname <- ("arg" ^ (itoa i));
        need_to_replace := true;
      end;
      newformals := (List.append !newformals [ith]);
    done;
    if (!need_to_replace = true)
    then (setFormals fdec !newformals);
  end

(* True if the string contains marshwrap_prefix, false otherwise. *)
let contains_marshwrap_prefix (s: string) : bool =
  begin
    let marshwrap_prefix_regexp = (Str.regexp marshwrap_prefix) in
    (Str.string_match marshwrap_prefix_regexp s 0);
  end

(* Strip the marshwrap_prefix from an input string *)
let strip_marshwrap_prefix (s: string) : string =
  begin
    let marshwrap_prefix_len = (String.length marshwrap_prefix) in
    let s_len = (String.length s) in
    let marshwrap_prefix_regexp = (Str.regexp marshwrap_prefix) in
    if (Str.string_match marshwrap_prefix_regexp s 0) = false
    then fatal["Cannot strip marshwrap_prefix in "; s];
    try
      (String.sub s marshwrap_prefix_len (s_len - marshwrap_prefix_len));
    with Invalid_argument(_) -> fatal["strip_marshwrap_prefix failure"];
  end

(*----------------------------------------------------------------------------*)
let kernfn_already_accounted_for (funclist_defn: fundec list) (fn: varinfo) : bool =
  begin
    let fdef_matches_decl fdec = (fdec.svar.vname = fn.vname) in
    let fn_defd = (* Given a fundec, tell us if it's been declared *)
      (List.exists fdef_matches_decl funclist_defn) in
    fn_defd;
  end

let strip_stub_prefix_pair (orig_name : string) : (string * string) =
  begin
    if (String.length stub_prefix) > (String.length orig_name) then
      ("", orig_name)
    else
      begin
        let test_prefix = String.sub orig_name 0 (String.length stub_prefix) in
        if
          (String.compare test_prefix stub_prefix) = 0 ||
          (String.compare test_prefix stubmarsh_prefix) = 0 ||
          (String.compare test_prefix stubdemarsh_prefix) = 0 ||
          (String.compare test_prefix marshwrap_prefix) = 0
        then
          let length = (String.length orig_name) - (String.length stub_prefix) in
          let pos = String.length stub_prefix in
          try
            let substring = String.sub orig_name pos length in
            (test_prefix, substring);
          with Invalid_argument(_) ->
            fatal["strip_stub_prefix_pair failure"];
        else
          ("", orig_name);
      end
  end

(* What should I prefix the unpacking functions with? *)
let strip_stub_prefix (orig_name : string) : string =
  begin
    let (result_prefix, result_name) = strip_stub_prefix_pair orig_name in
    result_name;
  end

(**---------------------------------------------------------------------------*)
(** Helper function, used for detecting comments *)
let is_comment (str: string) : bool =
  begin
    if (str = "") || (String.get str 0) = '#'
    then true else false;
  end

(** Utilities *)
let create_array_typ (t: typ) (l: int) : typ =
  begin
    TArray(t, Some((integer l)), []);
  end
    
(** Get a hash value for a function name. This is used as its ID *)
let get_function_id (fname: string) : int =
  begin
    Hashtbl.find function_id_mappings fname;
  end

(** Get a hash value for a variable name *)
let get_lval_id (lv: lval) : int =
  begin
    Hashtbl.find function_id_mappings (lval_tostring lv);
  end

(** Populate entry points using the entries from the provided file *)
let populate_entry_points
    (filename: string)
    (entry_points: (string, string) Hashtbl.t)
    : unit =
  begin
    let enter_function (func: string) (annot: string) =
      Hashtbl.add entry_points func annot;
      Printf.fprintf stderr "%s\n" func;
    in
    if filename = "" then fatal ["Filename not given\n%!"];
    Printf.fprintf stderr "** Reading entry points from file %s **\n" filename;
    try
      let instream = open_in_gen [Open_rdonly] 0 filename in
      try
        while true do
          let currline = (input_line instream) in
          if (is_comment currline) = false then
            try
              Scanf.sscanf currline "%s %s" enter_function;
            with Scan_failure(s) -> warning ["Error populating java functions"];
        done;
      with End_of_file ->
        close_in instream;
    with Sys_error(error) ->
      fatal ["Cannot open java functions file\n%!"];
  end

(*---------------------------------------------------------------------------*)
(** Populate the list of functions that can exist separately in user
 * and kernel space. These include built-in GCC functions, library
 * functions and any other functions that can be implemented in
 * user-space. These functions will not be added to called_fun_varinfo
 * of split-user analysis, and so will not be considered as candidates
 * for which a new fundec is introduced (as a stub) *)
let populate_nonstubbed_functions
    (nonstubbed_functions              : (string, bool) Hashtbl.t)
    (nonstubbed_functions_delete_proto : (string, bool) Hashtbl.t)
    (override_interface                : (string, bool) Hashtbl.t)
    (only_gcc                          : int)
    : unit =
  begin
    let make_nonstub_raw (fnnm: string) (mode: int) =
      begin
        (*Printf.fprintf stderr "make nonstub: %s %b\n" fnnm delete_proto;*)
        if mode = 0 then
          add_if nonstubbed_functions fnnm true
        else if mode = 1 then
          begin
            add_if nonstubbed_functions fnnm true;
            add_if nonstubbed_functions_delete_proto fnnm true;
          end
        else if mode = 2 then (* Override false means we're a non-interface *)
          add_if override_interface fnnm false
        else if mode = 3 then (* Override true means we're an interface function *)
          add_if override_interface fnnm true
      end
    in
    let make_nonstub_keep_proto (fnnm: string) = make_nonstub_raw fnnm 0 in
    let make_nonstub_delete_proto (fnnm: string) = make_nonstub_raw fnnm 1 in
    let make_override_interface_no (fnnm: string) = make_nonstub_raw fnnm 2 in
    (*let make_override_interface_yes (fnnm: string) = make_nonstub_raw fnnm 3 in*)

    (* <GCC BUILT-IN FUNCTIONS> *)
    let builtins = (list_keys gccBuiltins) in
    for i = 0 to (List.length builtins) - 1
    do
      let ith = (List.nth builtins i) in
      make_nonstub_keep_proto ith;
    done;
    (* </GCC BUILT-IN FUNCTIONS> *)

    (* <MORE BUILT-IN FUNCTIONS> *)
    make_nonstub_keep_proto "__builtin_expect";
    make_nonstub_keep_proto "__builtin_memcmp";
    (* </MORE BUILT-IN FUNCTIONS> *)
    
    if (only_gcc == 0) then         (

    (* <FUNCTIONS IMPLEMENTED IN LIBRARIES> *)
    (* string functions *)
    make_nonstub_keep_proto "strcpy";
    make_nonstub_keep_proto "strncpy";
    make_nonstub_keep_proto "strcat";
    make_nonstub_keep_proto "strncat";
    make_nonstub_keep_proto "strcmp";
    make_nonstub_keep_proto "strncmp";
    make_nonstub_keep_proto "strchr";
    make_nonstub_keep_proto "strrchr";
    make_nonstub_keep_proto "strlen";
    make_nonstub_keep_proto "strnlen";
    make_nonstub_keep_proto "strstr";
    make_nonstub_keep_proto "strlcpy";
    make_nonstub_keep_proto "strlcat";
    make_nonstub_keep_proto "strnicmp";
    make_nonstub_keep_proto "strnchr";
    make_nonstub_keep_proto "strpbrk";
    make_nonstub_keep_proto "strsep";
    make_nonstub_keep_proto "strspn";
    make_nonstub_keep_proto "strcspn";
    make_nonstub_delete_proto "strtoul";
    (* Some extra functions that we use only in user-mode *)
    make_nonstub_delete_proto "MJR_strlcpy";
    make_nonstub_delete_proto "MJR_strlcat";

    (* Basic bit operations *)
    (* These have atomic semantics FIXME *)
    (* 
    make_nonstub_delete_proto "variable_test_bit";
    make_nonstub_delete_proto "set_bit";
    make_nonstub_delete_proto "clear_bit";
    make_nonstub_delete_proto "change_bit";
    make_nonstub_delete_proto "test_and_set_bit";
    make_nonstub_delete_proto "test_and_clear_bit";
    make_nonstub_delete_proto "test_and_change_bit";
    make_nonstub_delete_proto "constant_test_bit";
    make_nonstub_delete_proto "ffs";
    make_nonstub_delete_proto "fls";
    make_nonstub_delete_proto "__xchg";
    *)

    (* printing and scanning functions *)
    make_nonstub_delete_proto "uprintk";  (* LLVM workaround *)
    make_nonstub_keep_proto "printf";
    make_nonstub_delete_proto "fprintf";
    make_nonstub_delete_proto "sprintf";
    make_nonstub_keep_proto "vsprintf";
    make_nonstub_keep_proto "snprintf";
    make_nonstub_keep_proto "vsnprintf";
    make_nonstub_keep_proto "scnprintf";
    make_nonstub_keep_proto "vscnprintf";
    make_nonstub_keep_proto "sscanf";
    make_nonstub_keep_proto "vsscanf";
    (* memory functions 
    make_nonstub_keep_proto "memcpy";
    make_nonstub_keep_proto "memset";
    make_nonstub_keep_proto "memcmp";
    make_nonstub_delete_proto "__memcpy";
    make_nonstub_delete_proto "memscan";
    make_nonstub_delete_proto "__constant_memcpy";
    make_nonstub_delete_proto "__constant_c_memset";
    make_nonstub_delete_proto "__constant_c_and_count_memcpy";
    make_nonstub_delete_proto "__constant_c_and_count_memset";
    make_nonstub_delete_proto "__memset_generic";
    make_nonstub_delete_proto "bzero";
    *)
    
    (* </FUNCTIONS IMPLEMENTED IN LIBRARIES> *)

    (* Synchronization 
    make_nonstub_delete_proto "__spin_lock_init";
    make_nonstub_delete_proto "spin_lock_init";
    make_nonstub_delete_proto "_spin_lock";
    make_nonstub_delete_proto "scpin_lock";
    make_nonstub_delete_proto "_spin_lock_bh";
    make_nonstub_delete_proto "spin_lock_bh";
    make_nonstub_delete_proto "_spin_lock_irqsave";
    make_nonstub_delete_proto "spin_lock_irqsave";
    make_nonstub_delete_proto "_spin_lock_irq";
    make_nonstub_delete_proto "spin_lock_irq";
    make_nonstub_delete_proto "_spin_unlock";
    make_nonstub_delete_proto "spin_unlock";
    make_nonstub_delete_proto "_spin_unlock_bh";
    make_nonstub_delete_proto "spin_unlock_bh";
    make_nonstub_delete_proto "_spin_unlock_irqrestore";
    make_nonstub_delete_proto "spin_unlock_irqrestore";
    make_nonstub_delete_proto "_spin_unlock_irq";
    make_nonstub_delete_proto "spin_unlock_irq";
    make_nonstub_delete_proto "__mutex_init";
    make_nonstub_delete_proto "mutex_init";
    make_nonstub_delete_proto "mutex_lock";
    make_nonstub_delete_proto "mutex_lock_interruptible";
    make_nonstub_delete_proto "mutex_unlock";
    make_nonstub_delete_proto "__init_rwsem";
    make_nonstub_delete_proto "up_write";
    make_nonstub_delete_proto "up_read";
    make_nonstub_delete_proto "down_write";
    make_nonstub_delete_proto "down_read";

    make_nonstub_delete_proto "__init_rw_sem";
    make_nonstub_delete_proto "_write_lock_irq";
    make_nonstub_delete_proto "_write_unlock_irq";
    make_nonstub_delete_proto "_read_lock_irq";
    make_nonstub_delete_proto "_read_unlock_irq";
    make_nonstub_delete_proto "__rwlock_init";
    make_nonstub_delete_proto "_read_lock";
    make_nonstub_delete_proto "_read_unlock";
    make_nonstub_delete_proto "_write_lock_irqsave";
    make_nonstub_delete_proto "_write_unlock_irqrestore";
    make_nonstub_delete_proto "_read_lock_irqsave";
    make_nonstub_delete_proto "_read_unlock_irqrestore";
    make_nonstub_delete_proto "_spin_trylock";
    make_nonstub_delete_proto "get_signals";
    make_nonstub_delete_proto "set_signals";
    make_nonstub_delete_proto "get_signals";
    make_nonstub_delete_proto "unblock_signals";
    make_nonstub_delete_proto "block_signals";
    *)
    (*make_nonstub_delete_proto "atomic_inc";
      make_nonstub_delete_proto "atomic_dec";
    *)

    (* TCP functions -- use inline ASM 
    make_nonstub_delete_proto "csum_tcpudp_magic";
    make_nonstub_delete_proto "csum_ipv6_magic";
    *)

    (* Sleeping *)
    make_nonstub_keep_proto "__udelay"; (* Non-blocking *)
    (*
    make_nonstub_keep_proto "msleep"; (* Blocking *)
    make_nonstub_keep_proto "msleep_interruptible";
    *)

    (* BOGUS FUNCTIONS *)
    (*make_nonstub_keep_proto "MICRODRIVERS__DUMMY";*)

    make_nonstub_keep_proto "malloc";
    make_nonstub_keep_proto "free";

    (* For User-mode Linux/symbolic execution *)
    make_nonstub_delete_proto "klee_define_fixed_object";
    make_nonstub_delete_proto "klee_make_symbolic";
    make_nonstub_delete_proto "klee_range";
    make_nonstub_delete_proto "klee_int";
    make_nonstub_delete_proto "klee_silent_exit";
    make_nonstub_delete_proto "klee_abort";
    make_nonstub_delete_proto "klee_get_obj_size";
    make_nonstub_delete_proto "klee_print_expr";
    make_nonstub_delete_proto "klee_choose";
    make_nonstub_delete_proto "klee_is_symbolic";
    make_nonstub_delete_proto "klee_assume";
    make_nonstub_delete_proto "klee_warning";
    make_nonstub_delete_proto "klee_warning_once";
    make_nonstub_delete_proto "klee_prefer_cex";
    make_nonstub_delete_proto "klee_mark_global";
    make_nonstub_delete_proto "klee_get_value";
    make_nonstub_delete_proto "klee_check_memory_access";
    make_nonstub_delete_proto "klee_set_forking";
    make_nonstub_delete_proto "klee_alias_function";

    (* Functions that we handle via allocation wrappers *)
    make_nonstub_keep_proto "__get_free_pages";
    make_nonstub_keep_proto "free_pages";
    (*
    make_nonstub_keep_proto "dma_alloc_coherent";
    make_nonstub_keep_proto "dma_free_coherent";
    make_nonstub_keep_proto "pci_alloc_consistent";
    make_nonstub_keep_proto "pci_free_consistent";
    make_nonstub_keep_proto "pci_map_single";
    make_nonstub_keep_proto "pci_unmap_single";
    make_nonstub_keep_proto "pci_map_page";
    make_nonstub_keep_proto "pci_unmap_page";
    *)
    make_nonstub_keep_proto "skb_dma_map";
    make_nonstub_keep_proto "skb_dma_unmap";
    make_nonstub_keep_proto "alloc_etherdev_mq";
    make_nonstub_keep_proto "__kmalloc";
    make_nonstub_keep_proto "mod_timer";
    make_nonstub_keep_proto "disable_irq";
    make_nonstub_keep_proto "synchronize_irq";
    make_nonstub_keep_proto "enable_irq";
    make_nonstub_keep_proto "msleep";
    make_nonstub_keep_proto "msleep_interruptible";
    make_nonstub_keep_proto "netif_receive_skb";
    make_nonstub_keep_proto "__alloc_skb";
    make_nonstub_keep_proto "__rtnl_is_locked";
    make_nonstub_keep_proto "rtnl_is_locked";
    make_nonstub_keep_proto "__cond_resched";
    make_nonstub_keep_proto "mii_link_ok";
    make_nonstub_keep_proto "mii_ethtool_gset";
    
    make_nonstub_keep_proto "_spin_lock_irqsave";
    make_nonstub_keep_proto "_spin_trylock";
    make_nonstub_keep_proto "_spin_lock_irq";
    make_nonstub_keep_proto "_spin_lock";
    make_nonstub_keep_proto "_spin_unlock";
    make_nonstub_keep_proto "__spin_lock_init";
    (* UNCOMMENT FOR FAULT INJECTION *)	
    (* make_nonstub_keep_proto "__raw_spin_unlock"; *) 
    make_nonstub_keep_proto "odft_insert_range_hash";
    make_nonstub_keep_proto "_spin_unlock_irq";
    make_nonstub_keep_proto "_spin_unlock_irqrestore";

    make_nonstub_keep_proto "iowrite32";
    make_nonstub_keep_proto "ioread32";
    make_nonstub_keep_proto "__phys_addr";
    make_nonstub_keep_proto "__pskb_pull_tail";
    make_nonstub_keep_proto "skb_put";
    make_nonstub_keep_proto "skb_copy_and_csum_dev";
    make_nonstub_keep_proto "netif_msg_tx_queued";
    make_nonstub_keep_proto "memset";
    make_nonstub_keep_proto "memcpy";
    make_nonstub_keep_proto "pskb_expand_head";
    make_nonstub_keep_proto "__memcpy_fromio";
    make_nonstub_keep_proto "__memcpy";
    make_nonstub_keep_proto "vmalloc";
    (* make_nonstub_keep_proto "kfree"; *)
    make_nonstub_delete_proto "kfree"; 
    make_nonstub_keep_proto "vfree";
    make_nonstub_keep_proto "dev_alloc_skb";
    make_nonstub_keep_proto "__netdev_alloc_skb";
    make_nonstub_keep_proto "dev_kfree_skb_any";
    make_nonstub_keep_proto "kfree_skb";
    make_nonstub_keep_proto "kstrdup";
    make_nonstub_keep_proto "INIT_WORK";
    make_nonstub_keep_proto "INIT_DELAYED_WORK";
    make_nonstub_keep_proto "kmemdup";

    
    make_nonstub_keep_proto "logStackFrame";
    make_nonstub_keep_proto "logStackVar";
    make_nonstub_keep_proto "logRead";
    make_nonstub_keep_proto "logAlloc";
    make_nonstub_keep_proto "logWrite";
    make_nonstub_keep_proto "heapifyfree";
    make_nonstub_keep_proto "heapifymalloc";
    

    (* Related functions *)
    make_nonstub_keep_proto "copy_to_user";
    make_nonstub_keep_proto "copy_from_user";
    make_nonstub_keep_proto "current_thread_info";
    make_nonstub_keep_proto "virt_to_page";

    (* Sound core -- implemented in host-ud.c *)
    make_nonstub_delete_proto "shutdown_sound_core";
    make_nonstub_delete_proto "init_sound_core";

    (* Functions we aren't handling properly anyway *)
    (* Handled via wrappers *)
    make_nonstub_delete_proto "try_module_get";
    make_nonstub_delete_proto "module_put";
    (*make_nonstub_delete_proto "mmiowb";*)

    (* Also handled like allocation wrappers, since these
       are so rare, and supporting varargs adds significant
       complexity *)
    make_nonstub_keep_proto "printk";
    make_nonstub_keep_proto "__const_udelay";
    make_nonstub_keep_proto "snd_iprintf";
    make_nonstub_keep_proto "warn_slowpath";


    make_nonstub_delete_proto "init_module"; 
    make_nonstub_keep_proto "logFree";
    (* These functions can be included in user space once we get permissions to
       read and write to the device using the iopl system call. We should
       potentially include all the functions from arch/i386/kernel/io.c 
    make_nonstub_delete_proto "inb";
    make_nonstub_delete_proto "outb";
    make_nonstub_delete_proto "inw";
    make_nonstub_delete_proto "outw";
    make_nonstub_delete_proto "inl";
    make_nonstub_delete_proto "outl";

    make_nonstub_delete_proto "readb";
    make_nonstub_delete_proto "writeb";
    make_nonstub_delete_proto "readw";
    make_nonstub_delete_proto "writew";
    make_nonstub_delete_proto "readl";
    make_nonstub_delete_proto "writel";
    make_nonstub_delete_proto "readq";
    make_nonstub_delete_proto "writeq";

    make_nonstub_delete_proto "ioread8";
    make_nonstub_delete_proto "ioread16";
    make_nonstub_delete_proto "ioread16be";
    make_nonstub_delete_proto "ioread32";
    make_nonstub_delete_proto "ioread32be";

    make_nonstub_delete_proto "iowrite8";
    make_nonstub_delete_proto "iowrite16";
    make_nonstub_delete_proto "iowrite16be";
    make_nonstub_delete_proto "iowrite32";
    make_nonstub_delete_proto "iowrite32be";

    
    make_nonstub_delete_proto "pci_bus_read_config_byte";
    make_nonstub_delete_proto "pci_bus_read_config_word";
    make_nonstub_delete_proto "pci_bus_read_config_dword";
    make_nonstub_delete_proto "pci_bus_write_config_byte";
    make_nonstub_delete_proto "pci_bus_write_config_word";
    make_nonstub_delete_proto "pci_bus_write_config_dword";
    
    make_nonstub_delete_proto "pci_read_config_byte";
    make_nonstub_delete_proto "pci_read_config_word";
    make_nonstub_delete_proto "pci_read_config_dword";
    make_nonstub_delete_proto "pci_write_config_byte";
    make_nonstub_delete_proto "pci_write_config_word";
    make_nonstub_delete_proto "pci_write_config_dword";
    

    make_nonstub_delete_proto "pci_iounmap";
    make_nonstub_delete_proto "memcpy_fromio";

    make_nonstub_delete_proto "request_irq";
    make_nonstub_delete_proto "free_irq";
    *)
    (* USB-specific functions *)
    make_nonstub_keep_proto "uhci_to_hcd";
    make_nonstub_keep_proto "hcd_to_uhci";

    (* Ugh *)
    (*make_nonstub_delete_proto "current_thread_info";*)


    (* 8390-specific functions *)
    (*make_nonstub_keep_proto "netdev_priv";*)
    (*make_nonstub_keep_proto "netdev_priv_mjr";*)

    (* Forcedeth-specific functions *)
    make_nonstub_keep_proto "pci_push";
    make_nonstub_keep_proto "get_hwbase";
    make_nonstub_keep_proto "get_nvpriv";

    (* psmouse-base-specific functions *)
    (* make_nonstub_keep_proto "serio_get_drvdata";*) 

    (* snd_ library *)
    (*make_nonstub_keep_proto "snd_pcm_hw_rule_add";*)

  (* </ADD MORE FUNCTIONS OF YOUR CHOICE HERE> *)
  (*
    let not_renaming_list = (list_keys nonstubbed_functions) in
    (announcemsg
    (List.append ["Not renaming the following functions"] not_renaming_list);
  *)
    
    (************************************************************************)
    (* Next, we'll also create some non-interface functions.  These
       are functions incorrectly identified by the analysis as interface
       functions.  TODO:  make the analysis better and get rid of this
    *)
    
    (************************************************************************)
    (* Sound library:  These functions are incorrectly identified because their
       addresses are taken, and the analysis currently assumes that any
       function whose address is taken is an interface function
    *)

(*
    make_override_interface_no "atomic_dec";
    make_override_interface_no "atomic_inc";
    make_override_interface_no "bytes_to_frames";
    make_override_interface_no "bytes_to_samples";
    make_override_interface_no "choose_default_id";
    make_override_interface_no "cleanup_module";
    make_override_interface_no "compound_head";
    make_override_interface_no "cond_resched";
    make_override_interface_no "constant_test_bit";
    make_override_interface_no "constrs_interval";
    make_override_interface_no "constrs_mask";
    make_override_interface_no "copy_from_user_toio";
    make_override_interface_no "copy_to_user_fromio";
    make_override_interface_no "current_thread_info";
    make_override_interface_no "dec_snd_pages";
    make_override_interface_no "dev_get_drvdata";
    make_override_interface_no "dev_set_drvdata";
    make_override_interface_no "div32";
    make_override_interface_no "div64_32";
    make_override_interface_no "div_down";
    make_override_interface_no "div_up";
    make_override_interface_no "dma_alloc_coherent";
    make_override_interface_no "dma_free_coherent";
    make_override_interface_no "es1371_quirk_lookup";
    make_override_interface_no "ffs";
    make_override_interface_no "find_snd_minor";
    make_override_interface_no "frame_aligned";
    make_override_interface_no "frames_to_bytes";
    make_override_interface_no "gcd";
    make_override_interface_no "get_order";
    make_override_interface_no "get_page";
*)
    make_override_interface_no "hw_is_interval";
    make_override_interface_no "hw_is_mask";
    make_override_interface_no "hw_param_interval";
    make_override_interface_no "hw_param_interval_c";
    make_override_interface_no "hw_param_mask";
    make_override_interface_no "hw_param_mask_c";
(*
    make_override_interface_no "imajor";
    make_override_interface_no "iminor";
    make_override_interface_no "inb";
*)
    make_override_interface_no "inc_snd_pages";
    make_override_interface_no "init_info_for_card";
(*
    make_override_interface_no "INIT_LIST_HEAD";
    make_override_interface_no "init_module";
    make_override_interface_no "init_utsname";
    make_override_interface_no "init_waitqueue_entry";
    make_override_interface_no "IS_ERR";
    make_override_interface_no "kcalloc";
    make_override_interface_no "kmalloc";
    make_override_interface_no "kzalloc";
    make_override_interface_no "ld2";
    make_override_interface_no "__list_add";
    make_override_interface_no "list_add";
    make_override_interface_no "list_add_tail";
    make_override_interface_no "__list_del";
    make_override_interface_no "list_del";
    make_override_interface_no "list_del_init";
    make_override_interface_no "list_empty";
    make_override_interface_no "list_move_tail";
    make_override_interface_no "local_inc";
*)
    make_override_interface_no "master_free";
    make_override_interface_no "master_get";
    make_override_interface_no "master_info";
    make_override_interface_no "master_init";
    make_override_interface_no "master_put";
(*
    make_override_interface_no "module_is_live";
    make_override_interface_no "__module_ref_addr";
    make_override_interface_no "module_slot_match";
    make_override_interface_no "mul";
    make_override_interface_no "muldiv32";
    make_override_interface_no "outb";
    make_override_interface_no "outl";
    make_override_interface_no "PageTail";
    make_override_interface_no "pci_enable_device";
    make_override_interface_no "pci_get_drvdata";
    make_override_interface_no "pci_register_driver";
    make_override_interface_no "pci_request_regions";
    make_override_interface_no "pci_set_drvdata";
    make_override_interface_no "pci_set_master";
    make_override_interface_no "pci_set_power_state";
    make_override_interface_no "pci_unregister_driver";
*)
    make_override_interface_no "pcm_release_private";
    make_override_interface_no "pcm_sanity_check";
(*
    make_override_interface_no "PDE";
    make_override_interface_no "period_to_usecs";
    make_override_interface_no "poll_wait";
*)
    make_override_interface_no "preallocate_info_init";
    make_override_interface_no "preallocate_pcm_pages";
(*
    make_override_interface_no "PROC_I";
    make_override_interface_no "PTR_ERR";
*)
    make_override_interface_no "release_and_free_resource";
    make_override_interface_no "relink_to_local";
    make_override_interface_no "resize_info_buffer";
    make_override_interface_no "samples_to_bytes";
    make_override_interface_no "show_pcm_class";
(*
    make_override_interface_no "signal_pending";
*)

    make_override_interface_no "slave_free";
    make_override_interface_no "slave_get";
    make_override_interface_no "slave_get_val";
    make_override_interface_no "slave_info";
    make_override_interface_no "slave_init";
    make_override_interface_no "slave_put";
    make_override_interface_no "slave_put_val";
    make_override_interface_no "slave_tlv_cmd";
    make_override_interface_no "snd_add_device_sysfs_file";
    make_override_interface_no "__snd_bug_on";
    make_override_interface_no "snd_card_disconnect";
    make_override_interface_no "snd_card_do_free";
    make_override_interface_no "snd_card_file_add";
    make_override_interface_no "snd_card_file_remove";
    make_override_interface_no "snd_card_free";
    make_override_interface_no "snd_card_free_when_closed";
    make_override_interface_no "snd_card_get_device_link";
    make_override_interface_no "snd_card_id_read";
    make_override_interface_no "snd_card_info_done";
    make_override_interface_no "snd_card_info_init";
    make_override_interface_no "snd_card_info_read";
    make_override_interface_no "snd_card_info_read_oss";
    make_override_interface_no "snd_card_locked";
    make_override_interface_no "snd_card_module_info_read";
    make_override_interface_no "snd_card_new";
    make_override_interface_no "snd_card_proc_new";
    make_override_interface_no "snd_card_register";
    make_override_interface_no "snd_component_add";
    make_override_interface_no "snd_create_proc_entry";
    make_override_interface_no "snd_ctl_add";
    make_override_interface_no "snd_ctl_add_slave";
    make_override_interface_no "snd_ctl_boolean_mono_info";
    make_override_interface_no "snd_ctl_boolean_stereo_info";
    make_override_interface_no "snd_ctl_build_ioff";
    make_override_interface_no "snd_ctl_card_info";
    make_override_interface_no "snd_ctl_create";
    make_override_interface_no "snd_ctl_dev_disconnect";
    make_override_interface_no "snd_ctl_dev_free";
    make_override_interface_no "snd_ctl_dev_register";
    make_override_interface_no "snd_ctl_elem_add";
    make_override_interface_no "snd_ctl_elem_add_user";
    make_override_interface_no "snd_ctl_elem_info";
    make_override_interface_no "snd_ctl_elem_info_user";
    make_override_interface_no "snd_ctl_elem_list";
    make_override_interface_no "snd_ctl_elem_lock";
    make_override_interface_no "snd_ctl_elem_read";
    make_override_interface_no "snd_ctl_elem_read_user";
    make_override_interface_no "snd_ctl_elem_remove";
    make_override_interface_no "snd_ctl_elem_unlock";
    make_override_interface_no "snd_ctl_elem_user_free";
    make_override_interface_no "snd_ctl_elem_user_get";
    make_override_interface_no "snd_ctl_elem_user_info";
    make_override_interface_no "snd_ctl_elem_user_put";
    make_override_interface_no "snd_ctl_elem_user_tlv";
    make_override_interface_no "snd_ctl_elem_write";
    make_override_interface_no "snd_ctl_elem_write_user";
    make_override_interface_no "snd_ctl_empty_read_queue";
    (*make_override_interface_no "snd_ctl_fasync";*)
    make_override_interface_no "snd_ctl_find_hole";
    make_override_interface_no "snd_ctl_find_id";
    make_override_interface_no "snd_ctl_find_numid";
    make_override_interface_no "snd_ctl_free_one";
    make_override_interface_no "snd_ctl_get_ioff";
    make_override_interface_no "snd_ctl_get_ioffidx";
    make_override_interface_no "snd_ctl_get_ioffnum";
    make_override_interface_no "snd_ctl_hole_check";
    (*make_override_interface_no "snd_ctl_ioctl";*)
    make_override_interface_no "snd_ctl_make_virtual_master";
    make_override_interface_no "snd_ctl_new";
    make_override_interface_no "snd_ctl_new1";
    make_override_interface_no "snd_ctl_notify";
    (*make_override_interface_no "snd_ctl_open";*)
    (*make_override_interface_no "snd_ctl_poll";*)
    (*make_override_interface_no "snd_ctl_read";*)
    make_override_interface_no "_snd_ctl_register_ioctl";
    make_override_interface_no "snd_ctl_register_ioctl";
    (*make_override_interface_no "snd_ctl_release";*)
    make_override_interface_no "snd_ctl_remove";
    make_override_interface_no "snd_ctl_remove_id";
    make_override_interface_no "snd_ctl_remove_unlocked_id";
    make_override_interface_no "snd_ctl_rename_id";
    make_override_interface_no "snd_ctl_subscribe_events";
    make_override_interface_no "snd_ctl_tlv_ioctl";
    make_override_interface_no "_snd_ctl_unregister_ioctl";
    make_override_interface_no "snd_ctl_unregister_ioctl";
    make_override_interface_no "snd_device_disconnect";
    make_override_interface_no "snd_device_disconnect_all";
    make_override_interface_no "snd_device_free";
    make_override_interface_no "snd_device_free_all";
    make_override_interface_no "snd_device_new";
    make_override_interface_no "snd_device_register";
    make_override_interface_no "snd_device_register_all";
    make_override_interface_no "snd_device_type_name";
    (*make_override_interface_no "snd_disconnect_fasync";*)
    (*make_override_interface_no "snd_disconnect_ioctl";*)
    (*make_override_interface_no "snd_disconnect_llseek";*)
    (*make_override_interface_no "snd_disconnect_mmap";*)
    (*make_override_interface_no "snd_disconnect_poll";*)
    (*make_override_interface_no "snd_disconnect_read";*)
    (*make_override_interface_no "snd_disconnect_release";*)
    (*make_override_interface_no "snd_disconnect_write";*)
    make_override_interface_no "snd_dma_alloc_pages";
    make_override_interface_no "snd_dma_alloc_pages_fallback";
    make_override_interface_no "snd_dma_free_pages";
    make_override_interface_no "snd_dma_get_reserved_buf";

    make_override_interface_no "snd_hwdep_dev_register";
    make_override_interface_no "snd_hwdep_dsp_load";
    make_override_interface_no "snd_hwdep_dsp_status";
    make_override_interface_no "snd_hwdep_free";
    make_override_interface_no "snd_hwdep_info";
    (*make_override_interface_no "snd_hwdep_ioctl";*)
    (*make_override_interface_no "snd_hwdep_llseek";*)
    (*make_override_interface_no "snd_hwdep_mmap";*)
    make_override_interface_no "snd_hwdep_new";
    (*make_override_interface_no "snd_hwdep_open";*)
    (*make_override_interface_no "snd_hwdep_poll";*)
    (*make_override_interface_no "snd_hwdep_read";*)
    (*make_override_interface_no "snd_hwdep_release";*)
    make_override_interface_no "snd_hwdep_search";
    (*make_override_interface_no "snd_hwdep_write";*)
    make_override_interface_no "snd_info_card_create";
    make_override_interface_no "snd_info_card_disconnect";
    make_override_interface_no "snd_info_card_free";
    make_override_interface_no "snd_info_card_id_change";
    make_override_interface_no "snd_info_card_register";
    make_override_interface_no "snd_info_check_reserved_words";
    make_override_interface_no "snd_info_create_card_entry";
    make_override_interface_no "snd_info_create_entry";
    make_override_interface_no "snd_info_create_module_entry";
    make_override_interface_no "snd_info_dev_free_entry";
    make_override_interface_no "snd_info_dev_register_entry";
    make_override_interface_no "snd_info_disconnect";
    make_override_interface_no "snd_info_done";
    (*make_override_interface_no "snd_info_entry_ioctl";*)
    (*make_override_interface_no "snd_info_entry_llseek";*)
    (*make_override_interface_no "snd_info_entry_mmap";*)
    (*make_override_interface_no "snd_info_entry_open";*)
    (*make_override_interface_no "snd_info_entry_poll";*)
    make_override_interface_no "snd_info_entry_prepare";
    (*make_override_interface_no "snd_info_entry_read";*)
    (*make_override_interface_no "snd_info_entry_release";*)
    (*make_override_interface_no "snd_info_entry_write";*)
    make_override_interface_no "snd_info_free_entry";
    make_override_interface_no "snd_info_get_line";
    make_override_interface_no "snd_info_get_str";
    make_override_interface_no "snd_info_init";
    make_override_interface_no "snd_info_minor_register";
    make_override_interface_no "snd_info_minor_unregister";
    make_override_interface_no "snd_info_register";
    make_override_interface_no "snd_info_set_text_ops";
    make_override_interface_no "snd_info_version_done";
    make_override_interface_no "snd_info_version_init";
    make_override_interface_no "snd_info_version_read";
    make_override_interface_no "snd_interval_any";
    make_override_interface_no "snd_interval_checkempty";
    make_override_interface_no "snd_interval_div";
    make_override_interface_no "snd_interval_empty";
    make_override_interface_no "snd_interval_list";
    make_override_interface_no "snd_interval_mul";
    make_override_interface_no "snd_interval_muldivk";
    make_override_interface_no "snd_interval_mulkdiv";
    make_override_interface_no "snd_interval_none";
    make_override_interface_no "snd_interval_ratden";
    make_override_interface_no "snd_interval_ratnum";
    make_override_interface_no "snd_interval_refine";
    make_override_interface_no "snd_interval_refine_first";
    make_override_interface_no "snd_interval_refine_last";
    make_override_interface_no "snd_interval_setinteger";
    make_override_interface_no "snd_interval_single";
    make_override_interface_no "snd_interval_step";
    make_override_interface_no "snd_interval_value";
    make_override_interface_no "snd_iprintf";
    make_override_interface_no "snd_leave_user";
    make_override_interface_no "snd_lookup_minor_data";
    make_override_interface_no "snd_lookup_oss_minor_data";
    make_override_interface_no "snd_malloc_dev_pages";
    make_override_interface_no "snd_malloc_pages";
    make_override_interface_no "snd_malloc_sgbuf_pages";
    make_override_interface_no "snd_mask_any";
    make_override_interface_no "snd_mask_copy";
    make_override_interface_no "snd_mask_empty";
    make_override_interface_no "snd_mask_eq";
    make_override_interface_no "snd_mask_intersect";
    make_override_interface_no "snd_mask_leave";
    make_override_interface_no "snd_mask_max";
    make_override_interface_no "snd_mask_min";
    make_override_interface_no "snd_mask_none";
    make_override_interface_no "snd_mask_refine";
    make_override_interface_no "snd_mask_refine_first";
    make_override_interface_no "snd_mask_refine_last";
    make_override_interface_no "snd_mask_reset";
    make_override_interface_no "snd_mask_single";
    make_override_interface_no "snd_mask_test";
    make_override_interface_no "snd_mask_value";
    make_override_interface_no "snd_minor_info_done";
    make_override_interface_no "snd_minor_info_init";
    make_override_interface_no "snd_minor_info_oss_done";
    make_override_interface_no "snd_minor_info_oss_init";
    make_override_interface_no "snd_minor_info_oss_read";
    make_override_interface_no "snd_minor_info_read";
    make_override_interface_no "snd_oss_device_type_name";
    make_override_interface_no "snd_oss_info_register";
    make_override_interface_no "snd_oss_kernel_minor";
    make_override_interface_no "snd_pcm_access_name";
    make_override_interface_no "snd_pcm_action";

    make_override_interface_no "snd_pcm_action_group";
    make_override_interface_no "snd_pcm_action_lock_irq";
    make_override_interface_no "snd_pcm_action_nonatomic";
    make_override_interface_no "snd_pcm_action_single";
    make_override_interface_no "snd_pcm_add";
    (*make_override_interface_no "snd_pcm_aio_read";*)
    (*make_override_interface_no "snd_pcm_aio_write";*)
    make_override_interface_no "snd_pcm_attach_substream";
    make_override_interface_no "snd_pcm_capture_avail";
    make_override_interface_no "snd_pcm_capture_forward";
    make_override_interface_no "snd_pcm_capture_hw_avail";
    (*make_override_interface_no "snd_pcm_capture_ioctl";*)
    make_override_interface_no "snd_pcm_capture_ioctl1";
    (*make_override_interface_no "snd_pcm_capture_open";*)
    (*make_override_interface_no "snd_pcm_capture_poll";*)
    make_override_interface_no "snd_pcm_capture_rewind";
    make_override_interface_no "snd_pcm_channel_info";
    make_override_interface_no "snd_pcm_channel_info_user";
    make_override_interface_no "snd_pcm_common_ioctl1";
    make_override_interface_no "snd_pcm_default_mmap";
    make_override_interface_no "snd_pcm_delay";
    make_override_interface_no "snd_pcm_detach_substream";
    make_override_interface_no "snd_pcm_dev_disconnect";
    make_override_interface_no "snd_pcm_dev_free";
    make_override_interface_no "snd_pcm_dev_register";
    make_override_interface_no "snd_pcm_do_drain_init";
    make_override_interface_no "snd_pcm_do_pause";
    make_override_interface_no "snd_pcm_do_prepare";
    make_override_interface_no "snd_pcm_do_reset";
    make_override_interface_no "snd_pcm_do_start";
    make_override_interface_no "snd_pcm_do_stop";
    make_override_interface_no "snd_pcm_drain";
    make_override_interface_no "snd_pcm_drain_done";
    make_override_interface_no "snd_pcm_drop";
    (*make_override_interface_no "snd_pcm_fasync";*)
    make_override_interface_no "snd_pcm_file_fd";
    make_override_interface_no "snd_pcm_format_big_endian";
    make_override_interface_no "snd_pcm_format_linear";
    make_override_interface_no "snd_pcm_format_little_endian";
    make_override_interface_no "snd_pcm_format_name";
    make_override_interface_no "snd_pcm_format_physical_width";
    make_override_interface_no "snd_pcm_format_set_silence";
    make_override_interface_no "snd_pcm_format_signed";
    make_override_interface_no "snd_pcm_format_silence_64";
    make_override_interface_no "snd_pcm_format_size";
    make_override_interface_no "snd_pcm_format_unsigned";
    make_override_interface_no "snd_pcm_format_width";
    make_override_interface_no "snd_pcm_free";
    make_override_interface_no "snd_pcm_free_stream";
    make_override_interface_no "snd_pcm_gettime";
    make_override_interface_no "snd_pcm_hw_constraint_integer";
    make_override_interface_no "snd_pcm_hw_constraint_list";
    make_override_interface_no "snd_pcm_hw_constraint_mask";
    make_override_interface_no "snd_pcm_hw_constraint_mask64";
    make_override_interface_no "snd_pcm_hw_constraint_minmax";
    make_override_interface_no "snd_pcm_hw_constraint_msbits";
    make_override_interface_no "snd_pcm_hw_constraint_pow2";
    make_override_interface_no "snd_pcm_hw_constraint_ratdens";
    make_override_interface_no "snd_pcm_hw_constraint_ratnums";
    make_override_interface_no "snd_pcm_hw_constraints_complete";
    make_override_interface_no "snd_pcm_hw_constraints_init";
    make_override_interface_no "snd_pcm_hw_constraint_step";
    make_override_interface_no "snd_pcm_hw_convert_from_old_params";
    make_override_interface_no "snd_pcm_hw_convert_to_old_params";
    make_override_interface_no "snd_pcm_hw_free";
    make_override_interface_no "_snd_pcm_hw_param_any";
    make_override_interface_no "_snd_pcm_hw_param_first";
    make_override_interface_no "snd_pcm_hw_param_first";
    make_override_interface_no "_snd_pcm_hw_param_last";
    make_override_interface_no "snd_pcm_hw_param_last";
    make_override_interface_no "snd_pcm_hw_params";
    make_override_interface_no "_snd_pcm_hw_params_any";
    make_override_interface_no "snd_pcm_hw_params_choose";
    make_override_interface_no "_snd_pcm_hw_param_setempty";
    make_override_interface_no "snd_pcm_hw_params_old_user";
    make_override_interface_no "snd_pcm_hw_params_user";
    make_override_interface_no "snd_pcm_hw_param_value";
    make_override_interface_no "snd_pcm_hw_refine";
    make_override_interface_no "snd_pcm_hw_refine_old_user";
    make_override_interface_no "snd_pcm_hw_refine_user";
    make_override_interface_no "snd_pcm_hw_rule_add";
    make_override_interface_no "snd_pcm_hw_rule_buffer_bytes_max";
    make_override_interface_no "snd_pcm_hw_rule_div";
    make_override_interface_no "snd_pcm_hw_rule_format";

    make_override_interface_no "snd_pcm_hw_rule_list";
    make_override_interface_no "snd_pcm_hw_rule_msbits";
    make_override_interface_no "snd_pcm_hw_rule_mul";
    make_override_interface_no "snd_pcm_hw_rule_muldivk";
    make_override_interface_no "snd_pcm_hw_rule_mulkdiv";
    make_override_interface_no "snd_pcm_hw_rule_pow2";
    make_override_interface_no "snd_pcm_hw_rule_ratdens";
    make_override_interface_no "snd_pcm_hw_rule_rate";
    make_override_interface_no "snd_pcm_hw_rule_ratnums";
    make_override_interface_no "snd_pcm_hw_rule_sample_bits";
    make_override_interface_no "snd_pcm_hw_rule_step";
    make_override_interface_no "snd_pcm_hwsync";
    make_override_interface_no "snd_pcm_info";
    make_override_interface_no "snd_pcm_info_user";
    make_override_interface_no "snd_pcm_kernel_ioctl";
    make_override_interface_no "snd_pcm_lib_buffer_bytes";
    make_override_interface_no "snd_pcm_lib_free_pages";
    make_override_interface_no "snd_pcm_lib_ioctl";
    make_override_interface_no "snd_pcm_lib_ioctl_channel_info";
    make_override_interface_no "snd_pcm_lib_ioctl_reset";
    make_override_interface_no "snd_pcm_lib_malloc_pages";
    make_override_interface_no "snd_pcm_lib_period_bytes";
    make_override_interface_no "snd_pcm_lib_preallocate_dma_free";
    make_override_interface_no "snd_pcm_lib_preallocate_free";
    make_override_interface_no "snd_pcm_lib_preallocate_free_for_all";
    make_override_interface_no "snd_pcm_lib_preallocate_max_proc_read";
    make_override_interface_no "snd_pcm_lib_preallocate_pages";
    make_override_interface_no "snd_pcm_lib_preallocate_pages1";
    make_override_interface_no "snd_pcm_lib_preallocate_pages_for_all";
    make_override_interface_no "snd_pcm_lib_preallocate_proc_read";
    make_override_interface_no "snd_pcm_lib_preallocate_proc_write";
    make_override_interface_no "snd_pcm_lib_read";
    make_override_interface_no "snd_pcm_lib_read1";
    make_override_interface_no "snd_pcm_lib_read_transfer";
    make_override_interface_no "snd_pcm_lib_readv";
    make_override_interface_no "snd_pcm_lib_readv_transfer";
    make_override_interface_no "snd_pcm_lib_write";
    make_override_interface_no "snd_pcm_lib_write1";
    make_override_interface_no "snd_pcm_lib_write_transfer";
    make_override_interface_no "snd_pcm_lib_writev";
    make_override_interface_no "snd_pcm_lib_writev_transfer";
    make_override_interface_no "snd_pcm_limit_hw_rates";
    make_override_interface_no "snd_pcm_link";
    (*make_override_interface_no "snd_pcm_mmap";*)
    make_override_interface_no "snd_pcm_mmap_control";
    make_override_interface_no "snd_pcm_mmap_data";
    (*make_override_interface_no "snd_pcm_mmap_data_close";*)
    (*make_override_interface_no "snd_pcm_mmap_data_fault";*)
    (*make_override_interface_no "snd_pcm_mmap_data_open";*)
    make_override_interface_no "snd_pcm_mmap_status";
    make_override_interface_no "snd_pcm_new";
    make_override_interface_no "snd_pcm_new_stream";
    make_override_interface_no "snd_pcm_notify";
    make_override_interface_no "snd_pcm_open";
    make_override_interface_no "snd_pcm_open_file";
    make_override_interface_no "snd_pcm_open_substream";
    make_override_interface_no "snd_pcm_oss_format_name";
    make_override_interface_no "snd_pcm_pause";
    make_override_interface_no "snd_pcm_period_elapsed";
    make_override_interface_no "snd_pcm_playback_avail";
    make_override_interface_no "snd_pcm_playback_data";
    make_override_interface_no "snd_pcm_playback_empty";
    make_override_interface_no "snd_pcm_playback_forward";
    make_override_interface_no "snd_pcm_playback_hw_avail";
    (*make_override_interface_no "snd_pcm_playback_ioctl";*)
    make_override_interface_no "snd_pcm_playback_ioctl1";
    (*make_override_interface_no "snd_pcm_playback_open";*)
    make_override_interface_no "snd_pcm_playback_poll";
    make_override_interface_no "snd_pcm_playback_rewind";
    make_override_interface_no "snd_pcm_playback_silence";
    make_override_interface_no "snd_pcm_post_drain_init";
    make_override_interface_no "snd_pcm_post_pause";
    make_override_interface_no "snd_pcm_post_prepare";
    make_override_interface_no "snd_pcm_post_reset";
    make_override_interface_no "snd_pcm_post_start";
    make_override_interface_no "snd_pcm_post_stop";
    make_override_interface_no "snd_pcm_pre_drain_init";
    make_override_interface_no "snd_pcm_prepare";
    make_override_interface_no "snd_pcm_pre_pause";
    make_override_interface_no "snd_pcm_pre_prepare";
    make_override_interface_no "snd_pcm_pre_reset";
    make_override_interface_no "snd_pcm_pre_start";
    make_override_interface_no "snd_pcm_pre_stop";
    make_override_interface_no "snd_pcm_proc_info_read";
    make_override_interface_no "snd_pcm_rate_to_rate_bit";
    (*make_override_interface_no "snd_pcm_read";*)
    (*make_override_interface_no "snd_pcm_release";*)
    make_override_interface_no "snd_pcm_release_substream";
    make_override_interface_no "snd_pcm_reset";
    make_override_interface_no "snd_pcm_resume";
    make_override_interface_no "snd_pcm_running";
    make_override_interface_no "snd_pcm_set_ops";
    make_override_interface_no "snd_pcm_set_runtime_buffer";
    make_override_interface_no "snd_pcm_set_sync";
    make_override_interface_no "snd_pcm_sgbuf_get_chunk_size";
    make_override_interface_no "snd_pcm_sgbuf_ops_page";
    make_override_interface_no "snd_pcm_start";
    make_override_interface_no "snd_pcm_state_name";
    make_override_interface_no "snd_pcm_status";
    make_override_interface_no "snd_pcm_status_user";
    make_override_interface_no "snd_pcm_stop";
    make_override_interface_no "snd_pcm_stream_linked";
    make_override_interface_no "snd_pcm_stream_lock_irq";
    make_override_interface_no "snd_pcm_stream_name";

    make_override_interface_no "snd_pcm_stream_proc_done";
    make_override_interface_no "snd_pcm_stream_proc_info_read";
    make_override_interface_no "snd_pcm_stream_proc_init";
    make_override_interface_no "snd_pcm_stream_unlock_irq";
    make_override_interface_no "snd_pcm_subformat_name";
    make_override_interface_no "snd_pcm_substream_proc_done";
    make_override_interface_no "snd_pcm_substream_proc_hw_params_read";
    make_override_interface_no "snd_pcm_substream_proc_info_read";
    make_override_interface_no "snd_pcm_substream_proc_init";
    make_override_interface_no "snd_pcm_substream_proc_status_read";
    make_override_interface_no "snd_pcm_substream_proc_sw_params_read";
    make_override_interface_no "snd_pcm_sw_params";
    make_override_interface_no "snd_pcm_sw_params_user";
    make_override_interface_no "snd_pcm_sync_ptr";
    make_override_interface_no "snd_pcm_timer_done";
    make_override_interface_no "snd_pcm_timer_free";
    make_override_interface_no "snd_pcm_timer_init";
    make_override_interface_no "snd_pcm_timer_resolution";
    make_override_interface_no "snd_pcm_timer_resolution_change";
    make_override_interface_no "snd_pcm_timer_start";
    make_override_interface_no "snd_pcm_timer_stop";
    make_override_interface_no "snd_pcm_trigger_done";
    make_override_interface_no "snd_pcm_trigger_tstamp";
    make_override_interface_no "snd_pcm_tstamp";
    make_override_interface_no "snd_pcm_tstamp_mode_name";
    make_override_interface_no "snd_pcm_undo_pause";
    make_override_interface_no "snd_pcm_undo_start";
    make_override_interface_no "snd_pcm_unlink";
    make_override_interface_no "snd_pcm_update_hw_ptr";
    make_override_interface_no "snd_pcm_update_hw_ptr_interrupt";
    make_override_interface_no "snd_pcm_update_hw_ptr_pos";
    make_override_interface_no "snd_pcm_update_hw_ptr_post";
    (*make_override_interface_no "snd_pcm_write";*)
    make_override_interface_no "snd_pcm_xrun";
    make_override_interface_no "snd_power_wait";
    make_override_interface_no "snd_rawmidi_alloc_substreams";
    make_override_interface_no "snd_rawmidi_dev_disconnect";
    make_override_interface_no "snd_rawmidi_dev_free";
    make_override_interface_no "snd_rawmidi_dev_register";
    make_override_interface_no "snd_rawmidi_dev_seq_free";
    make_override_interface_no "snd_rawmidi_drain_input";
    make_override_interface_no "snd_rawmidi_drain_output";
    make_override_interface_no "snd_rawmidi_drop_output";
    make_override_interface_no "snd_rawmidi_file_flags";
    make_override_interface_no "snd_rawmidi_free";
    make_override_interface_no "snd_rawmidi_free_substreams";
    make_override_interface_no "snd_rawmidi_info";
    make_override_interface_no "snd_rawmidi_info_select";
    make_override_interface_no "snd_rawmidi_info_user";
    make_override_interface_no "snd_rawmidi_input_event_tasklet";
    make_override_interface_no "snd_rawmidi_input_params";
    make_override_interface_no "snd_rawmidi_input_status";
    make_override_interface_no "snd_rawmidi_input_trigger";
    (*make_override_interface_no "snd_rawmidi_ioctl";*)
    make_override_interface_no "snd_rawmidi_kernel_open";
    make_override_interface_no "snd_rawmidi_kernel_read";
    make_override_interface_no "snd_rawmidi_kernel_read1";
    make_override_interface_no "snd_rawmidi_kernel_release";
    make_override_interface_no "snd_rawmidi_kernel_write";
    make_override_interface_no "snd_rawmidi_kernel_write1";
    make_override_interface_no "snd_rawmidi_new";
    (*make_override_interface_no "snd_rawmidi_open";*)
    make_override_interface_no "snd_rawmidi_output_params";
    make_override_interface_no "snd_rawmidi_output_status";
    make_override_interface_no "snd_rawmidi_output_trigger";
    make_override_interface_no "snd_rawmidi_output_trigger_tasklet";
    (*make_override_interface_no "snd_rawmidi_poll";*)
    make_override_interface_no "snd_rawmidi_proc_info_read";
    (*make_override_interface_no "snd_rawmidi_read";*)
    make_override_interface_no "snd_rawmidi_ready";
    make_override_interface_no "snd_rawmidi_ready_append";
    make_override_interface_no "snd_rawmidi_receive";
    (*make_override_interface_no "snd_rawmidi_release";*)
    make_override_interface_no "snd_rawmidi_runtime_create";

    make_override_interface_no "snd_rawmidi_runtime_free";
    make_override_interface_no "snd_rawmidi_search";
    make_override_interface_no "snd_rawmidi_set_ops";
    make_override_interface_no "snd_rawmidi_transmit";
    make_override_interface_no "snd_rawmidi_transmit_ack";
    make_override_interface_no "snd_rawmidi_transmit_empty";
    make_override_interface_no "snd_rawmidi_transmit_peek";
    (*make_override_interface_no "snd_rawmidi_write";*)
    make_override_interface_no "snd_register_device";
    make_override_interface_no "snd_register_device_for_dev";
    make_override_interface_no "snd_register_oss_device";
    make_override_interface_no "snd_remove_proc_entry";
    make_override_interface_no "snd_request_card";
    make_override_interface_no "snd_sgbuf_aligned_pages";
    make_override_interface_no "snd_sndstat_proc_read";
    make_override_interface_no "snd_sndstat_show_strings";
    make_override_interface_no "snd_timer_check_master";
    make_override_interface_no "snd_timer_check_slave";
    make_override_interface_no "snd_timer_close";
    make_override_interface_no "snd_timer_continue";
    make_override_interface_no "snd_timer_dev_disconnect";
    make_override_interface_no "snd_timer_dev_free";
    make_override_interface_no "snd_timer_dev_register";
    make_override_interface_no "snd_timer_find";
    make_override_interface_no "snd_timer_free";
    make_override_interface_no "snd_timer_global_free";
    make_override_interface_no "snd_timer_global_new";
    make_override_interface_no "snd_timer_global_register";
    make_override_interface_no "snd_timer_instance_new";
    make_override_interface_no "snd_timer_interrupt";
    make_override_interface_no "snd_timer_new";
    make_override_interface_no "snd_timer_notify";
    make_override_interface_no "snd_timer_notify1";
    make_override_interface_no "snd_timer_open";
    make_override_interface_no "snd_timer_pause";
    make_override_interface_no "snd_timer_request";
    make_override_interface_no "snd_timer_reschedule";
    make_override_interface_no "snd_timer_resolution";
    make_override_interface_no "snd_timer_start";
    make_override_interface_no "snd_timer_start1";
    make_override_interface_no "snd_timer_start_slave";
    make_override_interface_no "_snd_timer_stop";
    make_override_interface_no "snd_timer_stop";
    make_override_interface_no "snd_timer_tasklet";
    make_override_interface_no "snd_unregister_device";
    make_override_interface_no "snd_unregister_oss_device";
    make_override_interface_no "tasklet_schedule";
    make_override_interface_no "test_and_set_bit";
    make_override_interface_no "test_ti_thread_flag";
    make_override_interface_no "test_tsk_thread_flag";
    make_override_interface_no "to_phys";
    make_override_interface_no "try_module_get";
    (*make_override_interface_no "variable_test_bit";*)
    make_override_interface_no "wait_for_avail_min";
    make_override_interface_no "xrun";
    
    (* More sound library functions -- post-static check *)
    make_override_interface_no "alsa_hwdep_exit";
    make_override_interface_no "alsa_hwdep_init";
    make_override_interface_no "alsa_pcm_exit";
    make_override_interface_no "alsa_pcm_init";
    make_override_interface_no "alsa_rawmidi_exit";
    make_override_interface_no "alsa_rawmidi_init";
    make_override_interface_no "alsa_sound_last_init";
    make_override_interface_no "alsa_sound_exit";
    make_override_interface_no "alsa_sound_init";
    make_override_interface_no "alsa_timer_exit";
    make_override_interface_no "alsa_timer_init";
    make_override_interface_no "snd_hrtimer_callback";
    make_override_interface_no "snd_hrtimer_close";
    make_override_interface_no "snd_hrtimer_exit";
    make_override_interface_no "snd_hrtimer_init";
    make_override_interface_no "snd_hrtimer_open";
    make_override_interface_no "snd_hrtimer_start";
    make_override_interface_no "snd_hrtimer_stop";
    make_override_interface_no "snd_hwdep_control_ioctl";
    make_override_interface_no "snd_hwdep_dev_disconnect";
    make_override_interface_no "snd_hwdep_dev_free";
    make_override_interface_no "snd_hwdep_proc_read";
    make_override_interface_no "snd_mem_exit";
    make_override_interface_no "snd_mem_init";
    (*make_override_interface_no "snd_mem_proc_open";*)
    make_override_interface_no "snd_mem_proc_read";
    (*make_override_interface_no "snd_open"; *) (* Called from the kernel *)
    make_override_interface_no "snd_pcm_control_ioctl";
    make_override_interface_no "snd_pcm_proc_read";
    make_override_interface_no "snd_rawmidi_control_ioctl";
    make_override_interface_no "snd_timer_free_system";
    make_override_interface_no "snd_timer_proc_read";
    make_override_interface_no "snd_timer_s_function";
    make_override_interface_no "snd_timer_s_start";
    make_override_interface_no "snd_timer_s_stop";
    make_override_interface_no "snd_timer_user_ccallback";
    (*make_override_interface_no "snd_timer_user_fasync";*)
    make_override_interface_no "snd_timer_user_interrupt";
    (*make_override_interface_no "snd_timer_user_ioctl";*)
    (*make_override_interface_no "snd_timer_user_open";*)
    (*make_override_interface_no "snd_timer_user_poll";*)
    (*make_override_interface_no "snd_timer_user_read";*)
    (*make_override_interface_no "snd_timer_user_release";*)
    make_override_interface_no "snd_timer_user_tinterrupt";

    (* Added for snd_ac97 files *)
    make_override_interface_no "snd_ac97_bus_proc_done";
    make_override_interface_no "snd_ac97_put_spsa";
    make_override_interface_no "snd_ac97_tune_hardware";
    make_override_interface_no "snd_ac97_pcm_close";
    make_override_interface_no "snd_ac97_change_volume_params2";
    make_override_interface_no "snd_ac97_ymf7x3_put_speaker";
    make_override_interface_no "snd_ac97_set_rate";
    make_override_interface_no "snd_ac97_determine_rates";
    make_override_interface_no "snd_ac97_read";
    make_override_interface_no "snd_ac97_add_vmaster";
    make_override_interface_no "snd_ac97_ad1986_lososel_get";
    make_override_interface_no "snd_ac97_proc_read_functions";
    make_override_interface_no "snd_ac97_ad1986_miclisel_get";
    make_override_interface_no "snd_ac97_cvol_new";
    make_override_interface_no "snd_ac97_spdif_default_put";
    make_override_interface_no "snd_ac97_ad18xx_pcm_put_bits";
    make_override_interface_no "snd_ac97_bus_proc_init";
    make_override_interface_no "snd_ac97_swap_ctl";
    make_override_interface_no "snd_ac97_ad198x_spdif_source_put";
    make_override_interface_no "snd_ac97_get_enum_double";
    make_override_interface_no "snd_ac97_get_short_name";
    make_override_interface_no "snd_ac97_cmedia_spdif_playback_source_get";
    make_override_interface_no "snd_ac97_update_bits";
    make_override_interface_no "snd_ac97_stac9708_put_bias";
    make_override_interface_no "snd_ac97_vt1618_UAJ_get";
    make_override_interface_no "snd_ac97_ymf7x3_spdif_source_info";
    make_override_interface_no "snd_ac97_ymf753_spdif_output_pin_put";
    make_override_interface_no "snd_ac97_proc_done";
    make_override_interface_no "snd_ac97_stac9758_output_jack_info";
    make_override_interface_no "snd_ac97_try_bit";
    make_override_interface_no "snd_ac97_cmix_new_stereo";
    make_override_interface_no "snd_ac97_stac9758_input_jack_info";
    make_override_interface_no "snd_ac97_cmute_new_stereo";
    make_override_interface_no "snd_ac97_ad18xx_pcm_info_bits";
    make_override_interface_no "snd_ac97_modem_build";
    make_override_interface_no "snd_ac97_update_power";
    make_override_interface_no "snd_ac97_ymf7x3_info_speaker";
    make_override_interface_no "snd_ac97_ad1986_spread_get";
    make_override_interface_no "snd_ac97_ymf753_spdif_output_pin_info";
    make_override_interface_no "snd_ac97_spdif_cmask_get";
    make_override_interface_no "snd_ac97_free";
    make_override_interface_no "snd_ac97_proc_init";
    make_override_interface_no "snd_ac97_mixer";
    make_override_interface_no "snd_ac97_try_volume_mix";
    make_override_interface_no "snd_ac97_dev_disconnect";
    make_override_interface_no "snd_ac97_vt1617a_smart51_get";
    make_override_interface_no "snd_ac97_write";
    make_override_interface_no "snd_ac97_ad1986_lososel_put";
    make_override_interface_no "snd_ac97_page_save";
    make_override_interface_no "snd_ac97_ad1986_miclisel_put";
    make_override_interface_no "snd_ac97_get_volsw";
    make_override_interface_no "snd_ac97_stac9758_phonesel_get";
    make_override_interface_no "snd_ac97_vt1618_UAJ_info";
    make_override_interface_no "snd_ac97_ad1986_vrefout_get";
    make_override_interface_no "snd_ac97_bus_free";
    make_override_interface_no "snd_ac97_powerdown";
    make_override_interface_no "snd_ac97_stac9758_phonesel_info";
    make_override_interface_no "snd_ac97_ad1985_vrefout_info";
    make_override_interface_no "snd_ac97_rename_vol_ctl";
    make_override_interface_no "snd_ac97_ad1888_downmix_info";
    make_override_interface_no "snd_ac97_cmedia_spdif_playback_source_put";
    make_override_interface_no "snd_ac97_vt1618_UAJ_put";
    make_override_interface_no "snd_ac97_spdif_pmask_get";
    make_override_interface_no "snd_ac97_info_enum_double";
    make_override_interface_no "snd_ac97_ad1888_downmix_get";
    make_override_interface_no "snd_ac97_info_volsw";
    make_override_interface_no "snd_ac97_put_enum_double";
    make_override_interface_no "snd_ac97_ad1985_vrefout_get";
    make_override_interface_no "snd_ac97_proc_read";
    make_override_interface_no "snd_ac97_ad18xx_pcm_get_bits";
    make_override_interface_no "snd_ac97_vt1617a_smart51_info";
    make_override_interface_no "snd_ac97_proc_read_main";
    make_override_interface_no "snd_ac97_ad1888_lohpsel_get";
    make_override_interface_no "snd_ac97_ad1986_spread_put";
    make_override_interface_no "snd_ac97_vt1617a_smart51_put";
    make_override_interface_no "snd_ac97_ad18xx_pcm_info_volume";
    make_override_interface_no "snd_ac97_stac9758_phonesel_put";
    make_override_interface_no "snd_ac97_ad1986_vrefout_put";
    make_override_interface_no "snd_ac97_get_name";
    make_override_interface_no "snd_ac97_stac9758_input_jack_get";
    make_override_interface_no "snd_ac97_pcm_assign";
    make_override_interface_no "snd_ac97_cnew";
    make_override_interface_no "snd_ac97_dev_register";
    make_override_interface_no "snd_ac97_vt1618_aux_get";
    make_override_interface_no "snd_ac97_ad18xx_pcm_get_volume";
    make_override_interface_no "snd_ac97_stac9758_output_jack_get";
    make_override_interface_no "snd_ac97_ad1888_downmix_put";
    make_override_interface_no "snd_ac97_page_restore";
    make_override_interface_no "snd_ac97_bus";
    make_override_interface_no "snd_ac97_ad1985_vrefout_put";
    make_override_interface_no "snd_ac97_update_bits_nolock";
    make_override_interface_no "snd_ac97_ymf7x3_spdif_source_get";
    make_override_interface_no "snd_ac97_dev_free";
    make_override_interface_no "snd_ac97_read_cache";
    make_override_interface_no "snd_ac97_rename_ctl";
    make_override_interface_no "snd_ac97_valid_reg";
    make_override_interface_no "snd_ac97_proc_regs_read";
    make_override_interface_no "snd_ac97_pcm_double_rate_rules";
    make_override_interface_no "snd_ac97_ad1888_lohpsel_put";
    make_override_interface_no "snd_ac97_remove_ctl";
    make_override_interface_no "snd_ac97_determine_spdif_rates";
    make_override_interface_no "snd_ac97_mixer_build";
    make_override_interface_no "snd_ac97_pcm_open";
    make_override_interface_no "snd_ac97_update";
    make_override_interface_no "snd_ac97_proc_regs_read_main";
    make_override_interface_no "snd_ac97_cmedia_spdif_playback_source_info";
    make_override_interface_no "snd_ac97_find_mixer_ctl";
    make_override_interface_no "snd_ac97_stac9758_input_jack_put";
    make_override_interface_no "snd_ac97_vt1618_aux_info";
    make_override_interface_no "snd_ac97_vt1618_aux_put";
    make_override_interface_no "snd_ac97_bus_dev_free";
    make_override_interface_no "snd_ac97_spdif_default_get";
    make_override_interface_no "snd_ac97_stac9758_output_jack_put";
    make_override_interface_no "snd_ac97_ymf7x3_get_speaker";
    make_override_interface_no "snd_ac97_test_rate";
    make_override_interface_no "snd_ac97_ad198x_spdif_source_get";
    make_override_interface_no "snd_ac97_ymf7x3_spdif_source_put";
    make_override_interface_no "snd_ac97_ad198x_spdif_source_info";
    make_override_interface_no "snd_ac97_ad18xx_update_pcm_bits";
    make_override_interface_no "snd_ac97_ymf753_spdif_output_pin_get";
    make_override_interface_no "snd_ac97_ad18xx_pcm_put_volume";
    make_override_interface_no "snd_ac97_spdif_mask_info";
    make_override_interface_no "snd_ac97_put_volsw";
    make_override_interface_no "snd_ac97_write_cache";

    (* Even more added for the new sound/seq directory of files *)
    make_override_interface_no "ac97_channel_mode_get";
    make_override_interface_no "ac97_channel_mode_info";
    make_override_interface_no "ac97_channel_mode_put";
    make_override_interface_no "ac97_device_release";
    make_override_interface_no "ac97_surround_jack_mode_get";
    make_override_interface_no "ac97_surround_jack_mode_info";
    make_override_interface_no "ac97_surround_jack_mode_put";
    make_override_interface_no "ad1888_update_jacks";
    make_override_interface_no "ad1985_update_jacks";
    make_override_interface_no "ad1986_update_jacks";
    make_override_interface_no "alc650_update_jacks";
    make_override_interface_no "alc655_iec958_route_get";
    make_override_interface_no "alc655_iec958_route_info";
    make_override_interface_no "alc655_iec958_route_put";
    make_override_interface_no "alc655_update_jacks";
    make_override_interface_no "alc850_update_jacks";
    make_override_interface_no "alsa_ac97_exit";
    make_override_interface_no "alsa_ac97_init";
    make_override_interface_no "alsa_seq_device_exit";
    make_override_interface_no "alsa_seq_device_init";
    make_override_interface_no "alsa_seq_dummy_exit";
    make_override_interface_no "alsa_seq_dummy_init";
    make_override_interface_no "alsa_seq_exit";
    make_override_interface_no "alsa_seq_init";
    make_override_interface_no "alsa_seq_midi_emul_exit";
    make_override_interface_no "alsa_seq_midi_emul_init";
    make_override_interface_no "alsa_seq_midi_event_exit";
    make_override_interface_no "alsa_seq_midi_event_init";
    make_override_interface_no "alsa_seq_midi_exit";
    make_override_interface_no "alsa_seq_midi_init";
    make_override_interface_no "alsa_virmidi_exit";
    make_override_interface_no "alsa_virmidi_init";
    make_override_interface_no "bind_hp_volsw_put";
    make_override_interface_no "cm9738_update_jacks";
    make_override_interface_no "cm9739_update_jacks";
    make_override_interface_no "cm9761_spdif_out_source_get";
    make_override_interface_no "cm9761_spdif_out_source_info";
    make_override_interface_no "cm9761_spdif_out_source_put";
    make_override_interface_no "cm9761_update_jacks";
    make_override_interface_no "copy_from_user_toio";
    make_override_interface_no "copy_to_user_fromio";
    make_override_interface_no "double_rate_hw_constraint_channels";
    make_override_interface_no "double_rate_hw_constraint_rate";
    make_override_interface_no "dummy_free";
    make_override_interface_no "dummy_input";
    make_override_interface_no "dummy_unuse";
    make_override_interface_no "dump_midi";
    make_override_interface_no "event_input_timer";
    make_override_interface_no "event_process_midi";
    make_override_interface_no "extra_decode_ctrl14";
    make_override_interface_no "extra_decode_xrpn";
    make_override_interface_no "hp_master_mute_sw_put";
    make_override_interface_no "it2646_update_jacks";
    make_override_interface_no "master_mute_sw_put";
    make_override_interface_no "midisynth_subscribe";
    make_override_interface_no "midisynth_unsubscribe";
    make_override_interface_no "midisynth_unuse";
    make_override_interface_no "midisynth_use";
    make_override_interface_no "mpatch_si3036";
    make_override_interface_no "note_decode";
    make_override_interface_no "note_event";
    make_override_interface_no "one_param_ctrl_event";
    make_override_interface_no "one_param_decode";
    make_override_interface_no "one_param_event";
    make_override_interface_no "patch_ad1819";
    make_override_interface_no "patch_ad1881";
    make_override_interface_no "patch_ad1885_specific";
    make_override_interface_no "patch_ad1885";
    make_override_interface_no "patch_ad1886_specific";
    make_override_interface_no "patch_ad1886";
    make_override_interface_no "patch_ad1888_specific";
    make_override_interface_no "patch_ad1888";
    make_override_interface_no "patch_ad1980_specific";

    make_override_interface_no "patch_ad1980";
    make_override_interface_no "patch_ad1981a_specific";
    make_override_interface_no "patch_ad1981a";
    make_override_interface_no "patch_ad1981b_specific";
    make_override_interface_no "patch_ad1981b";
    make_override_interface_no "patch_ad1985_specific";
    make_override_interface_no "patch_ad1985";
    make_override_interface_no "patch_ad1986_specific";
    make_override_interface_no "patch_ad1986";
    make_override_interface_no "patch_ad198x_post_spdif";
    make_override_interface_no "patch_alc203";
    make_override_interface_no "patch_alc650_specific";
    make_override_interface_no "patch_alc650";
    make_override_interface_no "patch_alc655_specific";
    make_override_interface_no "patch_alc655";
    make_override_interface_no "patch_alc850_specific";
    make_override_interface_no "patch_alc850";
    make_override_interface_no "patch_cirrus_build_spdif";
    make_override_interface_no "patch_cirrus_cs4299";
    make_override_interface_no "patch_cirrus_spdif";
    make_override_interface_no "patch_cm9738_specific";
    make_override_interface_no "patch_cm9738";
    make_override_interface_no "patch_cm9739_post_spdif";
    make_override_interface_no "patch_cm9739_specific";
    make_override_interface_no "patch_cm9739";
    make_override_interface_no "patch_cm9761_post_spdif";
    make_override_interface_no "patch_cm9761_specific";
    make_override_interface_no "patch_cm9761";
    make_override_interface_no "patch_cm9780_specific";
    make_override_interface_no "patch_cm9780";
    make_override_interface_no "patch_conexant_build_spdif";
    make_override_interface_no "patch_conexant";
    make_override_interface_no "patch_cx20551";
    make_override_interface_no "patch_it2646_specific";
    make_override_interface_no "patch_it2646";
    make_override_interface_no "patch_lm4550";
    make_override_interface_no "patch_si3036_specific";
    make_override_interface_no "patch_sigmatel_stac9700_3d";
    make_override_interface_no "patch_sigmatel_stac9700";
    make_override_interface_no "patch_sigmatel_stac9708_3d";
    make_override_interface_no "patch_sigmatel_stac9708_specific";
    make_override_interface_no "patch_sigmatel_stac9708";
    make_override_interface_no "patch_sigmatel_stac9721";
    make_override_interface_no "patch_sigmatel_stac9744";
    make_override_interface_no "patch_sigmatel_stac9756";
    make_override_interface_no "patch_sigmatel_stac9758_specific";
    make_override_interface_no "patch_sigmatel_stac9758";
    make_override_interface_no "patch_sigmatel_stac97xx_specific";
    make_override_interface_no "patch_tritech_tr28028";
    make_override_interface_no "patch_ucb1400_specific";
    make_override_interface_no "patch_ucb1400";
    make_override_interface_no "patch_vt1616_specific";
    make_override_interface_no "patch_vt1616";
    make_override_interface_no "patch_vt1617a";
    make_override_interface_no "patch_vt1618";
    make_override_interface_no "patch_wolfson03";
    make_override_interface_no "patch_wolfson04";
    make_override_interface_no "patch_wolfson05";
    make_override_interface_no "patch_wolfson11";
    make_override_interface_no "patch_wolfson13";
    make_override_interface_no "patch_wolfson_wm9703_specific";
    make_override_interface_no "patch_wolfson_wm9704_specific";
    make_override_interface_no "patch_wolfson_wm9705_specific";
    make_override_interface_no "patch_wolfson_wm9711_specific";
    make_override_interface_no "patch_wolfson_wm9713_3d";
    make_override_interface_no "patch_wolfson_wm9713_specific";
    make_override_interface_no "patch_yamaha_ymf743_build_spdif";
    make_override_interface_no "patch_yamaha_ymf743";
    make_override_interface_no "patch_yamaha_ymf753_post_spdif";
    make_override_interface_no "patch_yamaha_ymf753";
    make_override_interface_no "patch_yamaha_ymf7x3_3d";
    make_override_interface_no "pitchbend_ctrl_event";
    make_override_interface_no "pitchbend_decode";
    make_override_interface_no "seq_copy_in_kernel";
    make_override_interface_no "seq_copy_in_user";

    make_override_interface_no "snd_midi_channel_alloc_set";
    make_override_interface_no "snd_midi_channel_free_set";
    make_override_interface_no "snd_midi_channel_set_clear";
    make_override_interface_no "snd_midi_event_no_status";
    make_override_interface_no "snd_midi_input_event";
    make_override_interface_no "snd_midi_process_event";
    make_override_interface_no "snd_seq_device_dev_disconnect";
    make_override_interface_no "snd_seq_device_dev_free";
    make_override_interface_no "snd_seq_device_dev_register";
    make_override_interface_no "snd_seq_device_info";
    make_override_interface_no "snd_seq_event_port_attach";
    make_override_interface_no "snd_seq_info_clients_read";
    make_override_interface_no "snd_seq_info_queues_read";
    make_override_interface_no "snd_seq_info_timer_read";
    make_override_interface_no "snd_seq_ioctl_create_port";
    make_override_interface_no "snd_seq_ioctl_create_queue";
    make_override_interface_no "snd_seq_ioctl_delete_port";
    make_override_interface_no "snd_seq_ioctl_delete_queue";
    make_override_interface_no "snd_seq_ioctl_get_client_info";
    make_override_interface_no "snd_seq_ioctl_get_client_pool";
    make_override_interface_no "snd_seq_ioctl_get_named_queue";
    make_override_interface_no "snd_seq_ioctl_get_port_info";
    make_override_interface_no "snd_seq_ioctl_get_queue_client";
    make_override_interface_no "snd_seq_ioctl_get_queue_info";
    make_override_interface_no "snd_seq_ioctl_get_queue_status";
    make_override_interface_no "snd_seq_ioctl_get_queue_tempo";
    make_override_interface_no "snd_seq_ioctl_get_queue_timer";
    make_override_interface_no "snd_seq_ioctl_get_subscription";
    make_override_interface_no "snd_seq_ioctl_query_next_client";
    make_override_interface_no "snd_seq_ioctl_query_next_port";
    make_override_interface_no "snd_seq_ioctl_query_subs";
    make_override_interface_no "snd_seq_ioctl_remove_events";
    make_override_interface_no "snd_seq_ioctl_running_mode";
    make_override_interface_no "snd_seq_ioctl_set_client_info";
    make_override_interface_no "snd_seq_ioctl_set_client_pool";
    make_override_interface_no "snd_seq_ioctl_set_port_info";
    make_override_interface_no "snd_seq_ioctl_set_queue_client";
    make_override_interface_no "snd_seq_ioctl_set_queue_info";
    make_override_interface_no "snd_seq_ioctl_set_queue_tempo";
    make_override_interface_no "snd_seq_ioctl_set_queue_timer";
    (*make_override_interface_no "snd_seq_ioctl";*)
    make_override_interface_no "snd_seq_ioctl_subscribe_port";
    make_override_interface_no "snd_seq_ioctl_system_info";
    make_override_interface_no "snd_seq_ioctl_unsubscribe_port";
    make_override_interface_no "snd_seq_kernel_client_enqueue_blocking";
    make_override_interface_no "snd_seq_kernel_client_enqueue";
    make_override_interface_no "snd_seq_kernel_client_write_poll";
    make_override_interface_no "snd_seq_midisynth_register_port";
    make_override_interface_no "snd_seq_midisynth_unregister_port";
    (*make_override_interface_no "snd_seq_open";*)
    (*make_override_interface_no "snd_seq_poll";*)
    (*make_override_interface_no "snd_seq_read";*)
    (*make_override_interface_no "snd_seq_release";*)
    make_override_interface_no "snd_seq_timer_interrupt";
    (*make_override_interface_no "snd_seq_write";*)
    make_override_interface_no "snd_virmidi_dev_register";
    make_override_interface_no "snd_virmidi_dev_unregister";
    make_override_interface_no "snd_virmidi_event_input";
    make_override_interface_no "snd_virmidi_free";
    make_override_interface_no "snd_virmidi_input_close";
    make_override_interface_no "snd_virmidi_input_open";
    make_override_interface_no "snd_virmidi_input_trigger";
    make_override_interface_no "snd_virmidi_new";
    make_override_interface_no "snd_virmidi_output_close";
    make_override_interface_no "snd_virmidi_output_open";
    make_override_interface_no "snd_virmidi_output_trigger";
    make_override_interface_no "snd_virmidi_subscribe";
    make_override_interface_no "snd_virmidi_unsubscribe";
    make_override_interface_no "snd_virmidi_unuse";
    make_override_interface_no "snd_virmidi_use";
    make_override_interface_no "songpos_decode";
    make_override_interface_no "songpos_event";
    make_override_interface_no "tune_ad_sharing";
    make_override_interface_no "tune_alc_jack";
    make_override_interface_no "tune_hp_mute_led";
    make_override_interface_no "tune_hp_only";
    make_override_interface_no "tune_inv_eapd";
    make_override_interface_no "tune_mute_led";
    make_override_interface_no "tune_swap_hp";
    make_override_interface_no "tune_swap_surround";
    make_override_interface_no "two_param_ctrl_event";
    make_override_interface_no "two_param_decode";

    (* Added for sound_core.c and sound_firmware.c *)
    make_override_interface_no "ac97_can_amap";
    make_override_interface_no "ac97_enum_text_info";
    make_override_interface_no "ac97_is_audio";
    make_override_interface_no "ac97_is_modem";
    make_override_interface_no "ac97_is_rev22";
    make_override_interface_no "ac97_reset_wait";
    make_override_interface_no "ac97_update_bits_page";
    make_override_interface_no "alc850_is_aux_back_surround";
    make_override_interface_no "all_notes_off";
    make_override_interface_no "all_sounds_off";
    make_override_interface_no "patch_ad1881_chained";
    make_override_interface_no "patch_ad1881_chained1";
    make_override_interface_no "patch_ad1881_unchained";
    make_override_interface_no "patch_build_controls";
    make_override_interface_no "snd_midi_channel_init";
    make_override_interface_no "snd_midi_channel_init_set";
    make_override_interface_no "snd_midi_event_decode";
    make_override_interface_no "snd_midi_event_encode";
    make_override_interface_no "snd_midi_event_encode_byte";
    make_override_interface_no "snd_midi_event_free";
    make_override_interface_no "snd_midi_event_new";
    make_override_interface_no "snd_midi_event_reset_decode";
    make_override_interface_no "snd_midi_event_reset_encode";
    make_override_interface_no "snd_midi_reset_controllers";
    make_override_interface_no "snd_pcm_get";
    make_override_interface_no "snd_pcm_next";
    make_override_interface_no "snd_pcm_proc_done";
    make_override_interface_no "snd_pcm_proc_init";
    make_override_interface_no "snd_rawmidi_info_select_user";
    make_override_interface_no "snd_request_other";
    make_override_interface_no "snd_seq_autoload_lock";
    make_override_interface_no "snd_seq_autoload_unlock";
    make_override_interface_no "snd_seq_cell_alloc";
    make_override_interface_no "snd_seq_cell_free";
    make_override_interface_no "snd_seq_check_queue";
    make_override_interface_no "snd_seq_client_enqueue_event";
    make_override_interface_no "snd_seq_client_notify_subscription";
    make_override_interface_no "snd_seq_client_use_ptr";
    make_override_interface_no "snd_seq_compare_real_time";
    make_override_interface_no "snd_seq_compare_tick_time";
    make_override_interface_no "snd_seq_control_queue";
    make_override_interface_no "snd_seq_create_kernel_client";
    make_override_interface_no "snd_seq_create_port";
    make_override_interface_no "snd_seq_delete_all_ports";
    make_override_interface_no "snd_seq_delete_kernel_client";
    make_override_interface_no "snd_seq_delete_port";
    make_override_interface_no "snd_seq_deliver_event";
    make_override_interface_no "snd_seq_deliver_single_event";
    make_override_interface_no "snd_seq_device_free";
    make_override_interface_no "snd_seq_device_load_drivers";
    make_override_interface_no "snd_seq_device_new";
    make_override_interface_no "snd_seq_device_register_driver";
    make_override_interface_no "snd_seq_device_unregister_driver";
    make_override_interface_no "snd_seq_dispatch_event";
    make_override_interface_no "snd_seq_do_ioctl";
    make_override_interface_no "snd_seq_dump_var_event";
    make_override_interface_no "snd_seq_enqueue_event";
    make_override_interface_no "snd_seq_event_dup";
    make_override_interface_no "snd_seq_event_port_detach";
    make_override_interface_no "snd_seq_expand_var_event";
    make_override_interface_no "snd_seq_fifo_cell_out";
    make_override_interface_no "snd_seq_fifo_cell_putback";
    make_override_interface_no "snd_seq_fifo_clear";
    make_override_interface_no "snd_seq_fifo_delete";
    make_override_interface_no "snd_seq_fifo_event_in";
    make_override_interface_no "snd_seq_fifo_new";
    make_override_interface_no "snd_seq_fifo_poll_wait";
    make_override_interface_no "snd_seq_fifo_resize";
    make_override_interface_no "snd_seq_file_flags";
    make_override_interface_no "snd_seq_get_port_info";
    make_override_interface_no "snd_seq_inc_real_time";
    make_override_interface_no "snd_seq_inc_time_nsec";
    make_override_interface_no "snd_seq_info_done";
    make_override_interface_no "snd_seq_info_dump_ports";
    make_override_interface_no "snd_seq_info_dump_subscribers";
    make_override_interface_no "snd_seq_info_init";
    make_override_interface_no "snd_seq_info_pool";
    make_override_interface_no "snd_seq_kernel_client_ctl";
    make_override_interface_no "snd_seq_kernel_client_dispatch";
    make_override_interface_no "snd_seq_midisynth_delete";

    make_override_interface_no "snd_seq_midisynth_new";
    make_override_interface_no "snd_seq_output_ok";
    make_override_interface_no "snd_seq_pool_available";
    make_override_interface_no "snd_seq_pool_delete";
    make_override_interface_no "snd_seq_pool_done";
    make_override_interface_no "snd_seq_pool_init";
    make_override_interface_no "snd_seq_pool_new";
    make_override_interface_no "snd_seq_pool_poll_wait";
    make_override_interface_no "snd_seq_port_connect";
    make_override_interface_no "snd_seq_port_disconnect";
    make_override_interface_no "snd_seq_port_get_subscription";
    make_override_interface_no "snd_seq_port_query_nearest";
    make_override_interface_no "snd_seq_port_use_ptr";
    make_override_interface_no "snd_seq_prioq_avail";
    make_override_interface_no "snd_seq_prioq_cell_in";
    make_override_interface_no "snd_seq_prioq_cell_out";
    make_override_interface_no "snd_seq_prioq_cell_peek";
    make_override_interface_no "snd_seq_prioq_delete";
    make_override_interface_no "snd_seq_prioq_leave";
    make_override_interface_no "snd_seq_prioq_new";
    make_override_interface_no "snd_seq_prioq_remove_events";
    make_override_interface_no "snd_seq_queue_alloc";
    make_override_interface_no "snd_seq_queue_check_access";
    make_override_interface_no "snd_seq_queue_client_leave";
    make_override_interface_no "snd_seq_queue_client_leave_cells";
    make_override_interface_no "snd_seq_queue_client_termination";
    make_override_interface_no "snd_seq_queue_delete";
    make_override_interface_no "snd_seq_queue_find_name";
    make_override_interface_no "snd_seq_queue_get_cur_queues";
    make_override_interface_no "snd_seq_queue_is_used";
    make_override_interface_no "snd_seq_queue_process_event";
    make_override_interface_no "snd_seq_queue_remove_cells";
    make_override_interface_no "snd_seq_queues_delete";
    make_override_interface_no "snd_seq_queue_set_owner";
    make_override_interface_no "snd_seq_queues_init";
    make_override_interface_no "snd_seq_queue_timer_close";
    make_override_interface_no "snd_seq_queue_timer_open";
    make_override_interface_no "snd_seq_queue_timer_set_tempo";
    make_override_interface_no "snd_seq_queue_use";
    make_override_interface_no "snd_seq_sanity_real_time";
    make_override_interface_no "snd_seq_set_port_info";
    make_override_interface_no "snd_seq_set_queue_tempo";
    make_override_interface_no "snd_seq_system_broadcast";
    make_override_interface_no "snd_seq_system_client_done";
    make_override_interface_no "snd_seq_system_client_init";
    make_override_interface_no "snd_seq_system_notify";
    make_override_interface_no "snd_seq_timer_close";
    make_override_interface_no "snd_seq_timer_continue";
    make_override_interface_no "snd_seq_timer_defaults";
    make_override_interface_no "snd_seq_timer_delete";
    make_override_interface_no "snd_seq_timer_get_cur_tick";
    make_override_interface_no "snd_seq_timer_get_cur_time";
    make_override_interface_no "snd_seq_timer_new";
    make_override_interface_no "snd_seq_timer_open";
    make_override_interface_no "snd_seq_timer_reset";
    make_override_interface_no "snd_seq_timer_set_position_tick";
    make_override_interface_no "snd_seq_timer_set_position_time";
    make_override_interface_no "snd_seq_timer_set_ppq";
    make_override_interface_no "snd_seq_timer_set_skew";
    make_override_interface_no "snd_seq_timer_set_tempo";
    make_override_interface_no "snd_seq_timer_set_tick_resolution";
    make_override_interface_no "snd_seq_timer_start";
    make_override_interface_no "snd_seq_timer_stop";
    make_override_interface_no "snd_seq_timer_update_tick";
    make_override_interface_no "snd_seq_total_cells";
    make_override_interface_no "snd_sequencer_device_done";
    make_override_interface_no "snd_sequencer_device_init";
    make_override_interface_no "snd_sequencer_memory_done";
    make_override_interface_no "snd_sequencer_memory_init";
    make_override_interface_no "snd_seq_unused_cells";
    make_override_interface_no "snd_seq_write_pool_allocated";
    make_override_interface_no "snd_timer_proc_done";
    make_override_interface_no "snd_timer_proc_init";
    make_override_interface_no "snd_timer_register_system";
    make_override_interface_no "snd_timer_user_append_to_tqueue";
    make_override_interface_no "snd_timer_user_continue";
    make_override_interface_no "snd_timer_user_copy_id";
    make_override_interface_no "snd_timer_user_ginfo";
    make_override_interface_no "snd_timer_user_gparams";
    make_override_interface_no "snd_timer_user_gstatus";
    make_override_interface_no "snd_timer_user_info";
    make_override_interface_no "snd_timer_user_next_device";
    make_override_interface_no "snd_timer_user_params";
    make_override_interface_no "snd_timer_user_pause";
    make_override_interface_no "snd_timer_user_start";
    make_override_interface_no "snd_timer_user_status";
    make_override_interface_no "snd_timer_user_stop";
    make_override_interface_no "snd_timer_user_tselect";
    make_override_interface_no "snd_timer_user_zero_id";
    make_override_interface_no "snd_virmidi_dev_attach_seq";
    make_override_interface_no "snd_virmidi_dev_detach_seq";
    make_override_interface_no "snd_virmidi_dev_receive_event";
    make_override_interface_no "snd_virmidi_init_event";
    make_override_interface_no "soundcore_open";
    make_override_interface_no "__sound_insert_unit";
    make_override_interface_no "sound_insert_unit";
    make_override_interface_no "__sound_remove_unit";
    make_override_interface_no "sound_remove_unit";

    (* This looks like we need to initialize some stuff: *)
    make_override_interface_no "mod_firmware_load";
    make_override_interface_no "register_sound_dsp";
    make_override_interface_no "register_sound_midi";
    make_override_interface_no "register_sound_mixer";
    make_override_interface_no "register_sound_special";
    make_override_interface_no "init_soundcore";
    make_override_interface_no "cleanup_soundcore";
    make_override_interface_no "unregister_sound_dsp";
    make_override_interface_no "unregister_sound_midi";
    make_override_interface_no "unregister_sound_mixer";

    (* MJR - added for ens1371 *)
    make_override_interface_no "snd_pci_quirk_lookup";
    make_override_interface_no "snd_ensoniq_playback1_open";
    (* make_override_interface_no "snd_ensoniq_playback1_close"; *)
    make_override_interface_no "snd_ensoniq_playback2_open";
    make_override_interface_no "snd_ensoniq_playback2_close";
    make_override_interface_no "snd_pcm_lib_ioctl";
    make_override_interface_no "snd_ensoniq_hw_params";
    make_override_interface_no "snd_ensoniq_hw_free";
    make_override_interface_no "snd_ensoniq_playback1_prepare";
    make_override_interface_no "snd_ensoniq_playback2prepare";
    make_override_interface_no "snd_ensoniq_trigger";
    make_override_interface_no "snd_ensoniq_playback1_pointer";
    make_override_interface_no "snd_ensoniq_playback2_pointer";
    
    make_override_interface_no "snd_ensoniq_capture_open";
    make_override_interface_no "snd_ensoniq_capture_close";
    make_override_interface_no "snd_pcm_lib_ioctl";
    make_override_interface_no "snd_ensoniq_hw_params";
    make_override_interface_no "snd_ensoniq_hw_free";
    make_override_interface_no "snd_ensoniq_capture_prepare";
    make_override_interface_no "snd_ensoniq_trigger";
    make_override_interface_no "snd_ensoniq_capture_pointer";

(*
    make_override_interface_no "snd_es1371_codec_write";
    make_override_interface_no "snd_es1371_codec_read";
    make_override_interface_no "snd_es1371_codec_wait";
*)
(* Commented by asim
    make_override_interface_no "snd_ensoniq_playback2_prepare";
    make_override_interface_no "snd_ens1373_spdif_info";
    make_override_interface_no "snd_ens1373_spdif_default_get";
    make_override_interface_no "snd_ens1373_spdif_default_put";
    make_override_interface_no "snd_ens1373_spdif_mask_get";
    make_override_interface_no "snd_ens1373_spdif_stream_get";
    make_override_interface_no "snd_ens1373_spdif_stream_put";
    make_override_interface_no "snd_es1371_spdif_get";
    make_override_interface_no "snd_es1371_spdif_put";
    make_override_interface_no "snd_es1373_rear_get";
    make_override_interface_no "snd_es1373_rear_put";
    make_override_interface_no "snd_es1373_line_get";
    make_override_interface_no "snd_es1373_line_put";
    make_override_interface_no "snd_ensoniq_mixer_free_ac97";
    make_override_interface_no "snd_ensoniq_proc_read";  
    make_override_interface_no "snd_ensoniq_dev_free";
    make_override_interface_no "snd_ensoniq_midi_input_open";
    make_override_interface_no "snd_ensoniq_midi_input_close";
    make_override_interface_no "snd_ensoniq_midi_output_open";
    make_override_interface_no "snd_ensoniq_midi_output_close";
    make_override_interface_no "snd_ensoniq_midi_input_trigger";
     make_override_interface_no "snd_ensoniq_midi_output_trigger"; *)
    make_override_interface_no "snd_audiopci_interrupt";

    make_override_interface_no "snd_pcm_hw_rule_add_MJR0";
    make_override_interface_no "snd_pcm_hw_rule_add_MJR1";
    make_override_interface_no "snd_pcm_hw_rule_add_MJR2";
    make_override_interface_no "snd_pcm_hw_rule_add_MJR3";

    (* Added these manually -- noticed in ca0106. *)
    make_override_interface_no "ac97_bus_init";
    make_override_interface_no "ac97_bus_exit";

    (* Added for CA0106 *)
    make_override_interface_no "snd_ca0106_shared_spdif_get";
    make_override_interface_no "snd_ca0106_shared_spdif_get";
    make_override_interface_no "snd_ca0106_shared_spdif_put";
    make_override_interface_no "snd_ca0106_shared_spdif_put";
    make_override_interface_no "snd_ca0106_capture_source_info";
    make_override_interface_no "snd_ca0106_capture_source_info";
    make_override_interface_no "snd_ca0106_capture_source_get";
    make_override_interface_no "snd_ca0106_capture_source_get";
    make_override_interface_no "snd_ca0106_capture_source_put";
    make_override_interface_no "snd_ca0106_capture_source_put";
    make_override_interface_no "snd_ca0106_i2c_capture_source_info";
    make_override_interface_no "snd_ca0106_i2c_capture_source_info";
    make_override_interface_no "snd_ca0106_i2c_capture_source_get";
    make_override_interface_no "snd_ca0106_i2c_capture_source_get";
    make_override_interface_no "snd_ca0106_i2c_capture_source_put";
    make_override_interface_no "snd_ca0106_i2c_capture_source_put";
    make_override_interface_no "snd_ca0106_capture_line_in_side_out_info";
    make_override_interface_no "snd_ca0106_capture_line_in_side_out_info";
    make_override_interface_no "snd_ca0106_capture_mic_line_in_info";
    make_override_interface_no "snd_ca0106_capture_mic_line_in_info";
    make_override_interface_no "snd_ca0106_capture_mic_line_in_get";
    make_override_interface_no "snd_ca0106_capture_mic_line_in_get";
    make_override_interface_no "snd_ca0106_capture_mic_line_in_put";
    make_override_interface_no "snd_ca0106_capture_mic_line_in_put";
    make_override_interface_no "snd_ca0106_spdif_info";
    make_override_interface_no "snd_ca0106_spdif_info";
    make_override_interface_no "snd_ca0106_spdif_get_default";
    make_override_interface_no "snd_ca0106_spdif_get_default";
    make_override_interface_no "snd_ca0106_spdif_get_stream";
    make_override_interface_no "snd_ca0106_spdif_get_stream";
    make_override_interface_no "snd_ca0106_spdif_get_mask";
    make_override_interface_no "snd_ca0106_spdif_get_mask";
    make_override_interface_no "snd_ca0106_spdif_put_default";
    make_override_interface_no "snd_ca0106_spdif_put_default";
    make_override_interface_no "snd_ca0106_spdif_put_stream";
    make_override_interface_no "snd_ca0106_spdif_put_stream";
    make_override_interface_no "snd_ca0106_volume_info";
    make_override_interface_no "snd_ca0106_volume_info";
    make_override_interface_no "snd_ca0106_volume_get";
    make_override_interface_no "snd_ca0106_volume_get";
    make_override_interface_no "snd_ca0106_volume_put";
    make_override_interface_no "snd_ca0106_volume_put";
    make_override_interface_no "snd_ca0106_i2c_volume_info";
    make_override_interface_no "snd_ca0106_i2c_volume_info";
    make_override_interface_no "snd_ca0106_i2c_volume_get";
    make_override_interface_no "snd_ca0106_i2c_volume_get";
    make_override_interface_no "snd_ca0106_i2c_volume_put";
    make_override_interface_no "snd_ca0106_i2c_volume_put";
    make_override_interface_no "snd_ca0106_pcm_free_substream";
    make_override_interface_no "snd_ca0106_pcm_free_substream";
    make_override_interface_no "snd_ca0106_pcm_close_playback";
    make_override_interface_no "snd_ca0106_pcm_close_playback";
    make_override_interface_no "snd_ca0106_pcm_open_playback_front";
    make_override_interface_no "snd_ca0106_pcm_open_playback_front";
    make_override_interface_no "snd_ca0106_pcm_open_playback_center_lfe";
    make_override_interface_no "snd_ca0106_pcm_open_playback_center_lfe";
    make_override_interface_no "snd_ca0106_pcm_open_playback_unknown";
    make_override_interface_no "snd_ca0106_pcm_open_playback_unknown";
    make_override_interface_no "snd_ca0106_pcm_open_playback_rear";
    make_override_interface_no "snd_ca0106_pcm_open_playback_rear";
    make_override_interface_no "snd_ca0106_pcm_close_capture";
    make_override_interface_no "snd_ca0106_pcm_close_capture";
    make_override_interface_no "snd_ca0106_pcm_open_0_capture";
    make_override_interface_no "snd_ca0106_pcm_open_0_capture";
    make_override_interface_no "snd_ca0106_pcm_open_1_capture";
    make_override_interface_no "snd_ca0106_pcm_open_1_capture";
    make_override_interface_no "snd_ca0106_pcm_open_2_capture";
    make_override_interface_no "snd_ca0106_pcm_open_2_capture";
    make_override_interface_no "snd_ca0106_pcm_open_3_capture";
    make_override_interface_no "snd_ca0106_pcm_open_3_capture";
    make_override_interface_no "snd_ca0106_pcm_hw_params_playback";
    make_override_interface_no "snd_ca0106_pcm_hw_params_playback";
    make_override_interface_no "snd_ca0106_pcm_hw_free_playback";
    make_override_interface_no "snd_ca0106_pcm_hw_free_playback";
    make_override_interface_no "snd_ca0106_pcm_hw_params_capture";
    make_override_interface_no "snd_ca0106_pcm_hw_params_capture";
    make_override_interface_no "snd_ca0106_pcm_hw_free_capture";
    make_override_interface_no "snd_ca0106_pcm_hw_free_capture";
    make_override_interface_no "snd_ca0106_pcm_prepare_playback";
    make_override_interface_no "snd_ca0106_pcm_prepare_playback";
    make_override_interface_no "snd_ca0106_pcm_prepare_capture";
    make_override_interface_no "snd_ca0106_pcm_prepare_capture";
    make_override_interface_no "snd_ca0106_pcm_trigger_playback";
    make_override_interface_no "snd_ca0106_pcm_trigger_playback";
    make_override_interface_no "snd_ca0106_pcm_trigger_capture";
    make_override_interface_no "snd_ca0106_pcm_trigger_capture";
    make_override_interface_no "snd_ca0106_pcm_pointer_playback";
    make_override_interface_no "snd_ca0106_pcm_pointer_playback";
    make_override_interface_no "snd_ca0106_pcm_pointer_capture";
    make_override_interface_no "snd_ca0106_pcm_pointer_capture";
    make_override_interface_no "snd_ca0106_ac97_read";
    make_override_interface_no "snd_ca0106_ac97_read";
    make_override_interface_no "snd_ca0106_ac97_write";
    make_override_interface_no "snd_ca0106_ac97_write";
    make_override_interface_no "snd_ca0106_dev_free";
    make_override_interface_no "snd_ca0106_dev_free";
    make_override_interface_no "ca0106_midi_interrupt_enable";
    make_override_interface_no "ca0106_midi_interrupt_enable";
    make_override_interface_no "ca0106_midi_interrupt_disable";
    make_override_interface_no "ca0106_midi_interrupt_disable";
    make_override_interface_no "ca0106_midi_read";
    make_override_interface_no "ca0106_midi_read";
    make_override_interface_no "ca0106_midi_write";
    make_override_interface_no "ca0106_midi_write";
    make_override_interface_no "ca0106_dev_id_card";
    make_override_interface_no "ca0106_dev_id_card";
    make_override_interface_no "ca0106_dev_id_port";
    make_override_interface_no "ca0106_dev_id_port";
    make_override_interface_no "snd_ca0106_proc_iec958";
    make_override_interface_no "snd_ca0106_proc_iec958";
    make_override_interface_no "snd_ca0106_proc_reg_write32";
    make_override_interface_no "snd_ca0106_proc_reg_write32";
    make_override_interface_no "snd_ca0106_proc_reg_read32";
    make_override_interface_no "snd_ca0106_proc_reg_read32";
    make_override_interface_no "snd_ca0106_proc_reg_read16";
    make_override_interface_no "snd_ca0106_proc_reg_read16";
    make_override_interface_no "snd_ca0106_proc_reg_read8";
    make_override_interface_no "snd_ca0106_proc_reg_read8";
    make_override_interface_no "snd_ca0106_proc_reg_read1";
    make_override_interface_no "snd_ca0106_proc_reg_read1";
    make_override_interface_no "snd_ca0106_proc_reg_read2";
    make_override_interface_no "snd_ca0106_proc_reg_read2";
    make_override_interface_no "snd_ca0106_proc_reg_write";
    make_override_interface_no "snd_ca0106_proc_reg_write";
    make_override_interface_no "snd_ca0106_proc_i2c_write";
    make_override_interface_no "snd_ca0106_proc_i2c_write";
    make_override_interface_no "ca_midi_interrupt";
    make_override_interface_no "ca_midi_interrupt";
    make_override_interface_no "ca_midi_input_open";
    make_override_interface_no "ca_midi_input_open";
    make_override_interface_no "ca_midi_output_open";
    make_override_interface_no "ca_midi_output_open";
    make_override_interface_no "ca_midi_input_close";
    make_override_interface_no "ca_midi_input_close";
    make_override_interface_no "ca_midi_output_close";
    make_override_interface_no "ca_midi_output_close";
    make_override_interface_no "ca_midi_input_trigger";
    make_override_interface_no "ca_midi_input_trigger";
    make_override_interface_no "ca_midi_output_trigger";
    make_override_interface_no "ca_midi_output_trigger";
    make_override_interface_no "ca_rmidi_free";
    make_override_interface_no "ca_rmidi_free";
    make_override_interface_no "spi_mute_get";
    make_override_interface_no "spi_mute_put";

    (* usb-audio *)
    make_override_interface_no "snd_usb_audio_dev_free";
    make_override_interface_no "snd_usb_audio_pcm_free";
    make_override_interface_no "snd_usb_capture_close";
    make_override_interface_no "snd_usb_capture_open";
    make_override_interface_no "snd_usb_create_midi_interface";
    make_override_interface_no "snd_usb_hw_free";
    make_override_interface_no "snd_usb_hw_params";
    make_override_interface_no "snd_usbmidi_cme_input";
    make_override_interface_no "snd_usbmidi_disconnect";
    make_override_interface_no "snd_usbmidi_emagic_finish_out";
    make_override_interface_no "snd_usbmidi_emagic_init_out";
    make_override_interface_no "snd_usbmidi_emagic_input";
    make_override_interface_no "snd_usbmidi_emagic_output";
    make_override_interface_no "snd_usbmidi_error_timer";
    make_override_interface_no "snd_usbmidi_get_port_info";
    make_override_interface_no "snd_usbmidi_input_close";
    make_override_interface_no "snd_usbmidi_input_open";
    make_override_interface_no "snd_usbmidi_input_start";
    make_override_interface_no "snd_usbmidi_input_stop";
    make_override_interface_no "snd_usbmidi_input_trigger";
    make_override_interface_no "snd_usbmidi_in_urb_complete";
    make_override_interface_no "snd_usbmidi_maudio_broken_running_status_input";
    make_override_interface_no "snd_usbmidi_midiman_input";
    make_override_interface_no "snd_usbmidi_novation_input";
    make_override_interface_no "snd_usbmidi_novation_output";
    make_override_interface_no "snd_usbmidi_output_close";
    make_override_interface_no "snd_usbmidi_output_midiman_packet";
    make_override_interface_no "snd_usbmidi_output_open";
    make_override_interface_no "snd_usbmidi_output_standard_packet";
    make_override_interface_no "snd_usbmidi_output_trigger";
    make_override_interface_no "snd_usbmidi_out_tasklet";
    make_override_interface_no "snd_usbmidi_out_urb_complete";
    make_override_interface_no "snd_usbmidi_raw_input";
    make_override_interface_no "snd_usbmidi_rawmidi_free";
    make_override_interface_no "snd_usbmidi_raw_output";
    make_override_interface_no "snd_usbmidi_standard_input";
    make_override_interface_no "snd_usbmidi_standard_output";
    make_override_interface_no "snd_usbmidi_us122l_input";
    make_override_interface_no "snd_usbmidi_us122l_output";
    make_override_interface_no "snd_usb_mixer_dev_free";
    make_override_interface_no "snd_usb_mixer_status_complete";
    make_override_interface_no "snd_usb_pcm_capture_trigger";
    make_override_interface_no "snd_usb_pcm_playback_trigger";
    make_override_interface_no "snd_usb_pcm_pointer";
    make_override_interface_no "snd_usb_pcm_prepare";
    make_override_interface_no "snd_usb_playback_close";
    make_override_interface_no "snd_usb_playback_open";
    make_override_interface_no "snd_usb_sbrc_hwdep_open";
    make_override_interface_no "snd_usb_sbrc_hwdep_poll";
    make_override_interface_no "snd_usb_sbrc_hwdep_read";
    make_override_interface_no "snd_usb_sbrc_hwdep_release";
    make_override_interface_no "snd_usb_soundblaster_remote_complete";
    (*
      make_override_interface_no "usb_audio_disconnect";
      make_override_interface_no "usb_audio_probe";
    *)
    make_override_interface_no "usb_mixer_elem_free";
    make_override_interface_no "usb_mixer_selector_elem_free";

    make_override_interface_no "ignore_interface_quirk";
    make_override_interface_no "create_composite_quirk";
    make_override_interface_no "snd_usb_create_midi_interface";
    make_override_interface_no "snd_usb_create_midi_interface";
    make_override_interface_no "snd_usb_create_midi_interface";
    make_override_interface_no "snd_usb_create_midi_interface";
    make_override_interface_no "snd_usb_create_midi_interface";
    make_override_interface_no "snd_usb_create_midi_interface";
    make_override_interface_no "snd_usb_create_midi_interface";
    make_override_interface_no "snd_usb_create_midi_interface";
    make_override_interface_no "create_standard_audio_quirk";
    make_override_interface_no "create_fixed_stream_quirk";
    make_override_interface_no "create_ua1000_quirk";
    make_override_interface_no "create_ua101_quirk";
    make_override_interface_no "create_uaxx_quirk";

    make_override_interface_no "prepare_capture_sync_urb_hs";
    make_override_interface_no "prepare_capture_sync_urb";
    make_override_interface_no "prepare_capture_urb";
    make_override_interface_no "prepare_nodata_playback_urb";
    make_override_interface_no "prepare_playback_sync_urb_hs";
    make_override_interface_no "prepare_playback_sync_urb";
    make_override_interface_no "prepare_playback_urb";
    make_override_interface_no "retire_capture_sync_urb";
    make_override_interface_no "retire_capture_urb";
    make_override_interface_no "retire_paused_capture_urb";
    make_override_interface_no "retire_playback_sync_urb_hs_emu";
    make_override_interface_no "retire_playback_sync_urb_hs";
    make_override_interface_no "retire_playback_sync_urb";
    make_override_interface_no "retire_playback_urb";
    make_override_interface_no "snd_complete_sync_urb";
    make_override_interface_no "snd_complete_urb";

    (* For E1000 *)
    make_override_interface_no "e1000_alloc_rx_buffers";
    make_override_interface_no "e1000_clean_rx_irq";

    (* For CMIPCI 
    make_override_interface_no "snd_cmipci_hw_params";
    make_override_interface_no "snd_cmipci_playback2_hw_params";
    make_override_interface_no "snd_cmipci_hw_free";
    make_override_interface_no "snd_cmipci_playback_trigger";
    make_override_interface_no "snd_cmipci_playback_pointer";
    make_override_interface_no "snd_cmipci_capture_trigger";
    make_override_interface_no "snd_cmipci_capture_pointer";
    make_override_interface_no "snd_cmipci_spdif_default_info";
    make_override_interface_no "snd_cmipci_spdif_default_get";
    make_override_interface_no "snd_cmipci_spdif_default_put";
    make_override_interface_no "snd_cmipci_spdif_mask_info";
    make_override_interface_no "snd_cmipci_spdif_mask_get";
    make_override_interface_no "snd_cmipci_spdif_stream_info";
    make_override_interface_no "snd_cmipci_spdif_stream_get";
    make_override_interface_no "snd_cmipci_spdif_stream_put";
    make_override_interface_no "snd_cmipci_playback_prepare";
    make_override_interface_no "snd_cmipci_playback_spdif_prepare";
    make_override_interface_no "snd_cmipci_playback_hw_free";
    make_override_interface_no "snd_cmipci_playback2_hw_free";
    make_override_interface_no "snd_cmipci_capture_prepare";
    make_override_interface_no "snd_cmipci_capture_spdif_prepare";
    make_override_interface_no "snd_cmipci_capture_spdif_hw_free";
    make_override_interface_no "snd_cmipci_interrupt";
    make_override_interface_no "snd_cmipci_playback_open";
    make_override_interface_no "snd_cmipci_capture_open";
    make_override_interface_no "snd_cmipci_playback2_open";
    make_override_interface_no "snd_cmipci_playback_spdif_open";
    make_override_interface_no "snd_cmipci_capture_spdif_open";
    make_override_interface_no "snd_cmipci_playback_close";
    make_override_interface_no "snd_cmipci_capture_close";
    make_override_interface_no "snd_cmipci_playback2_close";
    make_override_interface_no "snd_cmipci_playback_spdif_close";
    make_override_interface_no "snd_cmipci_capture_spdif_close";
    make_override_interface_no "snd_cmipci_info_volume";
    make_override_interface_no "snd_cmipci_get_volume";
    make_override_interface_no "snd_cmipci_put_volume";
    make_override_interface_no "snd_cmipci_info_input_sw";
    make_override_interface_no "snd_cmipci_get_input_sw";
    make_override_interface_no "snd_cmipci_put_input_sw";
    make_override_interface_no "snd_cmipci_info_native_mixer";
    make_override_interface_no "snd_cmipci_get_native_mixer";
    make_override_interface_no "snd_cmipci_put_native_mixer";
    make_override_interface_no "snd_cmipci_get_native_mixer_sensitive";
    make_override_interface_no "snd_cmipci_put_native_mixer_sensitive";
    make_override_interface_no "snd_cmipci_uswitch_get";
    make_override_interface_no "snd_cmipci_uswitch_put";
    make_override_interface_no "snd_cmipci_spdout_enable_get";
    make_override_interface_no "snd_cmipci_spdout_enable_put";
    make_override_interface_no "snd_cmipci_line_in_mode_info";
    make_override_interface_no "snd_cmipci_line_in_mode_get";
    make_override_interface_no "snd_cmipci_line_in_mode_put";
    make_override_interface_no "snd_cmipci_mic_in_mode_info";
    make_override_interface_no "snd_cmipci_mic_in_mode_get";
    make_override_interface_no "snd_cmipci_mic_in_mode_put";
    make_override_interface_no "snd_cmipci_proc_read";
    make_override_interface_no "snd_cmipci_dev_free";
  make_override_interface_no "snd_cmipci_probe";
    make_override_interface_no "snd_cmipci_remove";
  *)
    make_override_interface_no "mpu401_write_port";
    make_override_interface_no "mpu401_read_port";
    make_override_interface_no "mpu401_write_mmio";
    make_override_interface_no "mpu401_read_mmio";
    make_override_interface_no "snd_mpu401_uart_interrupt";
    make_override_interface_no "snd_mpu401_uart_interrupt_tx";
    make_override_interface_no "snd_mpu401_uart_timer";
    make_override_interface_no "snd_mpu401_uart_input_open";
    make_override_interface_no "snd_mpu401_uart_output_open";
    make_override_interface_no "snd_mpu401_uart_input_close";
    make_override_interface_no "snd_mpu401_uart_output_close";
    make_override_interface_no "snd_mpu401_uart_input_trigger";
    make_override_interface_no "snd_mpu401_uart_output_trigger";
    make_override_interface_no "snd_mpu401_uart_free";
    make_override_interface_no "alsa_mpu401_uart_init";
    make_override_interface_no "alsa_mpu401_uart_exit";
    make_override_interface_no "snd_opl2_command";
    make_override_interface_no "snd_opl3_command";
    make_override_interface_no "snd_opl3_timer1_start";
    make_override_interface_no "snd_opl3_timer1_stop";
    make_override_interface_no "snd_opl3_timer2_start";
    make_override_interface_no "snd_opl3_timer2_stop";
    make_override_interface_no "snd_opl3_interrupt";
    make_override_interface_no "snd_opl3_dev_free";
    make_override_interface_no "snd_opl3_timer_new";
    make_override_interface_no "alsa_opl3_init";
    make_override_interface_no "alsa_opl3_exit";
    make_override_interface_no "snd_opl3_open";
    make_override_interface_no "snd_opl3_ioctl";
    make_override_interface_no "snd_opl3_release";

    (* For TG3 *)
    make_override_interface_no "tg3_read_indirect_mbox";
    make_override_interface_no "tg3_read_indirect_reg32";
    make_override_interface_no "tg3_read32_mbox_5906";
    make_override_interface_no "tg3_read32";
    make_override_interface_no "tg3_read32_mbox_5906";
    make_override_interface_no "tg3_read32";
    make_override_interface_no "tg3_read_indirect_mbox";
    make_override_interface_no "tg3_read_indirect_reg32";
    make_override_interface_no "tg3_write32_mbox_5906";
    make_override_interface_no "tg3_write32";
    make_override_interface_no "tg3_write32_tx_mbox";
    make_override_interface_no "tg3_write_flush_reg32";
    make_override_interface_no "tg3_write_indirect_mbox";
    make_override_interface_no "tg3_write_indirect_reg32";
    make_override_interface_no "tg3_write_mem";

    );
    (* MJR Testing ... *)
    make_override_interface_no "random_func";
  end

(*---------------------------------------------------------------------------*)
(** Populate the list of functions with no fundecs *)
let populate_funcs_with_no_fundecs
    (funcs_with_no_fundecs : (string, varinfo) Hashtbl.t) (* Out *)
    (vinfol: varinfo list) (* In *)
    : unit =
  begin
    for i = 0 to (List.length vinfol) - 1 do
      let ith = (List.nth vinfol i) in
      add_if funcs_with_no_fundecs ith.vname ith;
    done;
  end

(*---------------------------------------------------------------------------*)
(** Populate annotations using the input file given to us *)
let populate_annotations (annot: (string, string) Hashtbl.t)
    (filename: string) : unit =
  begin
    let linectr = ref 0 in
    let sanity_check (str: string) : unit =
      begin
        if (str = "") then
          fatal ["Parse err. No label: "; (Int32.to_string (Int32.of_int !linectr))];
        if (String.contains str ' ') then
          fatal ["Parse err. Improper label "; (Int32.to_string (Int32.of_int !linectr))];
      end in
    let enter_annotation (funname: string) (label: string)=
      begin
        sanity_check label;
        Hashtbl.add annot funname label;
      end in
    if filename = "" then (fatal ["Filename not given\n%!"]);
    Printf.fprintf stderr "** Reading file %s **\n%!" filename;
    try
      let instream = (open_in_gen [Open_rdonly] 0 filename) in
      try
        while true do
          let currline = input_line instream in
          linectr := !linectr + 1;
          if (is_comment currline) = false then
            begin
              try
                Scanf.sscanf currline "%s %s" enter_annotation;
              with Scan_failure(s) -> warning ["Error populating annotations: "; s];
            end
        done;
      with End_of_file -> close_in instream;
    with Sys_error(error) -> fatal ["Cannot open annotations file\n%!"];
  end

let populate_function_id_mapping
    (annotations: (string, string) Hashtbl.t)
    (fn_addr_taken_defn: fundec list)
    (fn_addr_taken_decl: varinfo list)
    (const_strings: varinfo list)
    : unit =
  begin
    Hashtbl.clear function_id_mappings;

    let unsorted_list1 = list_keys annotations in
    let fundec_to_str f =
      f.svar.vname
    in
    let unsorted_list2 = List.map fundec_to_str fn_addr_taken_defn in
    let varinfo_to_str vi =
      vi.vname
    in
    let unsorted_list3 = List.map varinfo_to_str fn_addr_taken_decl in
    let unsorted_list4 = List.map varinfo_to_str const_strings in
    let lval_to_str lv =
      (lval_tostring lv)
    in
    let unsorted_list5 = List.map lval_to_str !lvals_to_register in
    let unsorted_list = unsorted_list1 @
      unsorted_list2 @ unsorted_list3 @
      unsorted_list4 @ unsorted_list5
    in

    (* Fancy way to remove duplicates *)
    (* We have remove_repeats, but that doesn't have the right semantics *)
    let uniq lst =
      let unique_set = Hashtbl.create (List.length lst) in
      begin
        List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
        Hashtbl.fold (fun x () xs -> x :: xs) unique_set []
      end
    in
    let unsorted_no_dups = uniq unsorted_list in
    let sorted_list = List.sort String.compare unsorted_no_dups in
    let current_id = ref 1000 in
    let add_to_hash str =
      begin
        Printf.fprintf stderr "Function/global mapping %s -> %d\n" str !current_id;
        Hashtbl.add function_id_mappings str !current_id;
        current_id := !current_id + 1;
      end
    in
    List.iter add_to_hash sorted_list;
  end

let populate_execution_mode_map
    (annotations : (string, string) Hashtbl.t) (* In *)
    (interface_functions : (string, bool) Hashtbl.t) (* In *)
    : unit =
  begin
    Printf.fprintf stderr "execution_mode_map\n";
    let listkeys = list_keys interface_functions in
    for i = 0 to (List.length listkeys) - 1 do
      let ith = List.nth listkeys i in
      try(	
      let annotation = Hashtbl.find annotations ith in
      if ((String.compare annotation "kern") = 0) then
        Printf.fprintf stderr "%s      default_mode\n" ith
      else
        Printf.fprintf stderr "%s      symexec_na\n" ith
	);
	with Not_found -> (fatal [ "FAIL"] );
    done;
  end

(** Returns true if the function name is a "special" one
    that does not ever need mashaling code and should never be
    deleted
*)
let is_special_function (fnm: string) : bool =
  let java_prefix_regexp = Str.regexp "^Java_.*" in
  let nosfi_prefix_regexp = Str.regexp "^odft_insert_range_hash" in
  let nosfi_prefix_regexp2 = Str.regexp "^kadav" in
  let nonstub_prefix_regexp = Str.regexp "^Nonstub_.*" in
  let sym_prefix_regexp = Str.regexp "^Sym_.*" in
  let microdrivers_prefix_regexp = Str.regexp "^MICRODRIVERS__.*" in
  (Str.string_match java_prefix_regexp fnm 0) ||
    (Str.string_match nonstub_prefix_regexp fnm 0) ||
    (Str.string_match nosfi_prefix_regexp fnm 0) ||
    (Str.string_match nosfi_prefix_regexp2 fnm 0) ||
    (Str.string_match sym_prefix_regexp fnm 0) ||
    (Str.string_match microdrivers_prefix_regexp fnm 0)

(** Return true if this is an interface function *)
let is_interface_function
    (interface_functions : (string, bool) Hashtbl.t)
    (fnm: string) : bool =
  if is_special_function fnm then
    false
  else
    try
      begin
        ignore (Hashtbl.find interface_functions fnm);
        true
      end
    with Not_found ->
      false

(* TODO: improve analysis *)
(* Checks ONLY those functions that we're overriding.
   E.g. sound library.  This is a hack.  The right way is to
   improve the analysis so that we can tell if it's
   an interface function or not *)
(* Returns:
   - 0 if we're not overriding anything.
   - 1 if we're overriding to non-interface
   - 2 if we're overriding to interface
*)
let is_override_interface
    (override_interface : (string, bool) Hashtbl.t)
    (fnm: string) : int =
  begin
    (* First see if the function is in the hash table *)
    (* If it is, then it's definitely not an interface function *)
    try
      begin
        let result = Hashtbl.find override_interface fnm in
        let retval =
          if result = true then 2 (* Interface *)
          else 1 (* Non-interface *)
        in
        Printf.fprintf stderr "is_override_interface: %s %d\n" fnm retval;
        retval;
      end
    with Not_found ->
      begin
        Printf.fprintf stderr "is_override_interface: %s 0\n" fnm;
        0;
      end
  end
     
(** Populate the list of interface functions. An interface function is a
 * function
 * - with "user" annotation that is called by a function with "kern"
 *   annotation, or
 * - with "kern" annotation that is called by a function with "user"
 *   annotation, or
 * - with no callers (roots), AND a "user" annotation, or
 * - whose addresse is taken, AND have "user" annotation (these denote
 *   functions exported to the kernel), or
 * - that does not have a "fundec". These functions are always interface
 *   functions because they must be "kern" functions and could potentially
 *   be called by "user" functions.
 *
 * Identifying interface functions serves two goals:
 * (1) Only these functions need a stub generated for them.
 * (2) Only these functions need be exported.
 *)
(*
let contains_nosfi_prefix (s: string) : bool =
  begin
    Printf.fprintf stderr "CHECKING NOSFI FOR %s. \n\n " s;	
    let nosfi_prefix_regexp = (Str.regexp "odft_nosfi_") in
    (Str.string_match nosfi_prefix_regexp s 0);
  end 
*)
let populate_interface_functions
    (f: file) (* In *)
    (annotations : (string, string) Hashtbl.t) (* In *)
    (funcs_with_no_fundecs : (string, varinfo) Hashtbl.t) (* In *)
    (override_interface : (string, bool) Hashtbl.t) (* In *)
    (interface_functions : (string, bool) Hashtbl.t) (* Out *)
    : unit =
  begin
    let cgbuild = (Cgcomp_dri.callgraph_build f false) in
    let fullcg = cgbuild.fullcg in
    let nofundecs = cgbuild.funcs_with_no_fundecs in
    populate_funcs_with_no_fundecs funcs_with_no_fundecs nofundecs;

    (try
       let cgnodenames = (list_keys fullcg) in
       for i = 0 to (List.length cgnodenames) - 1 do
         let ith = (List.nth cgnodenames i) in
         let ithnode = (Hashtbl.find fullcg ith) in
         let ithcallers = ithnode.cnCallers in
         let ithcallernames = (list_keys ithcallers) in
         let ithannot= ref "" in
         (try
	    (* if (is_special_function ith == false) then *)
            ithannot := (Hashtbl.find annotations ith);
          with Not_found -> (fatal ["Annotation not found 1:"; ith]));
         let override = is_override_interface override_interface ith in
         if override = 0 then
           begin
             (* Address taken? Then i/f function *)
             if (ithnode.cnInfo.vaddrof = true) &&
               (String.compare !ithannot "user") = 0
             then (add_if interface_functions ith true);
             (* No parents, and user annotation? Then i/f function *)
             if ((List.length ithcallernames) = 0) &&
               (String.compare !ithannot "user") = 0
             then (add_if interface_functions ith true);
             (* Checking parents *)
             for j = 0 to (List.length ithcallernames) - 1 do
               let jth = (List.nth ithcallernames j) in

               Printf.fprintf stderr "Asim disabled interface function.\n";
               (* COMMENTED BY ASIM
               (try
                  let ithannot = (Hashtbl.find annotations ith) in
                  let jthannot = (Hashtbl.find annotations jth) in
                  (* If ithannot is not equal to it's callee's annot,
                     i.e., jthannot, then ith is an interface function *)
                  if (String.compare ithannot jthannot) <> 0
                  then (add_if interface_functions ith true);
                with Not_found -> ((fatal ["Annotation not found 2: "; jth; ", other: "; ith])));
                *)
		(* if (is_special_function ith == false) then *)
               (try
                  let ithannot = (Hashtbl.find annotations ith) in
                  let jthannot = (Hashtbl.find annotations jth) in
                  (* If ithannot is not equal to it's callee's annot,
                     i.e., jthannot, then ith is an interface function *)
                  if ((String.compare ithannot jthannot) <> 0 && (String.compare
                  ithannot "user") = 0)
                  then (add_if interface_functions ith true);
                with Not_found -> ((fatal ["Annotation not found 2: "; jth; ", other: "; ith])));

             done;
           end
         else if override = 1 then
           (* Override to non-interface function *)
           ()
         else if override = 2 then
           (* Override to interface function *)
           add_if interface_functions ith true
         else
           begin
             Printf.fprintf stderr "override: %d\n" override;
             fatal ["Failure in populate_interface_functions.  Unexpected override"];
           end
       done;
     with Not_found -> (
       (fatal ["Failure in Hashtbl.find in for loop"]);
     ));

    (* Functions with no fundecs become interface functions, provided
     * that they are not one of the supported vararg functions. *)
    for i = 0 to (List.length nofundecs) - 1 do
      let ith = (List.nth nofundecs i) in
      if (is_interface_function interface_functions ith.vname) = false then
        (add_if interface_functions ith.vname true);
    done;

    let listkeys = list_keys interface_functions in
    for i = 0 to (List.length listkeys) - 1 do
      let ith = (List.nth listkeys i) in
  Printf.fprintf stderr "Interface function %s: Annotation is %s\n"
        ith (Hashtbl.find annotations ith);
    
    done;
  end

(* A hashtable that stores temprorary variables that have been generated so
   far *)
let temporary_variables: (string, varinfo) Hashtbl.t = (Hashtbl.create 117)

(* A hashtable that stores global variables that have been generated so far *)
let genglobal_variables: (string, varinfo) Hashtbl.t = (Hashtbl.create 117)

(** Get temporary variable: Both the marshaler and demarshaler may need
 * temporary variables. Get the temporary variable corresponding to an
 * lval. If no such temporary exists, manufacture one and return it
 * The first argument denotes the function's fundec, and the second
 * argument denotes the lval corresponding to which we want a temporary
 * The third one denotes the type of the lval. An optional HINT is to
 * be supplied to generate a variable name. *)
let get_temporary_variable (fdec: fundec)
    (lvopt: lval option)
    (t: typ)
    (hint: string) : varinfo =
  begin
    let lvalname = ref "" in
    (match lvopt with
       | Some(lv) -> lvalname := (lval_tostring lv);
       | None -> ();
    );
    let hashkey = fdec.svar.vname ^ hint ^ (typ_tostring_noattr t) ^ !lvalname in
    (try
       (Hashtbl.find temporary_variables hashkey);
     with Not_found -> (
       let newtempvar = (makeTempVar fdec ~name:(hint) t) in
       (Hashtbl.add temporary_variables hashkey newtempvar);
       newtempvar;
     ));
  end

(** Returns the variable with the exact name specified that's already
    a parameter to the function *)
let get_formal_variable (fdec: fundec) (name: string) : varinfo =
  begin
    let vi_list = fdec.sformals in
    let predicate vi = ((String.compare vi.vname name) = 0) in
    let matching_vi = List.find predicate vi_list in
    matching_vi;
  end

(** Get local variable: Like get_temporary_variable, but exact name is used *)
let get_local_variable (fdec: fundec)
    (t: typ)
    (name: string) : varinfo =
  begin
    let hashkey = fdec.svar.vname ^ name ^ (typ_tostring_noattr t) in
    try
      Hashtbl.find temporary_variables hashkey;
    with Not_found -> 
      begin
        let newtempvar = makeLocalVar fdec name t in
        Hashtbl.add temporary_variables hashkey newtempvar;
        newtempvar;
      end
  end

let remove_local_variable (fdec: fundec) 
    (t: typ)
    (name: string) : unit =
  begin
    let hashkey = fdec.svar.vname ^ name ^ (typ_tostring_noattr t) in
    try
      ignore (Hashtbl.find temporary_variables hashkey);
      Hashtbl.remove temporary_variables hashkey;
      let local_filter vi =
        begin
          (*Printf.fprintf stderr "%s %s\n" vi.vname name;*)
          if (String.compare vi.vname name) = 0 &&
            ((Cil.typeSig vi.vtype) = (Cil.typeSig t)) then
              false
          else
            true;
        end
      in
      let new_locals = List.filter local_filter fdec.slocals in
      fdec.slocals <- new_locals;
    with Not_found -> ();
  end

(** Get global variable *)
let get_global_variable (t: typ)
    (hint: string) : varinfo =
  begin
    let hashkey = hint ^ (typ_tostring_noattr t) in
    (try
       (Hashtbl.find genglobal_variables hashkey);
     with Not_found -> (
       let newglobal = (makeGlobalVar hint t) in
       (Hashtbl.add genglobal_variables hashkey newglobal);
       newglobal;
     ));
  end
   
(** There is a list of functions that need not be split. That is, the user
 * space and kernel space each get a copy of this function. There is no need
 * to rename such functions. Should I rename this function?  The answer here
 * is true if it is not contained in nonstubbed_functions *)
let should_split
    (nonstubbed_functions : (string, bool) Hashtbl.t)
    (fnm: string) : bool =
  begin
    (* First see if the function is in the hash table *)
    try (ignore (Hashtbl.find nonstubbed_functions fnm)); false;
    with Not_found ->
      (* If it's not, then this is probably a normal function,
         unless it has a Java_ or Nonstub_ prefix, which means we
         added it specifically to deal with some Java problem, e.g.
         bit-field manipulations which XDR doesn't support, or
         for rich annotations, e.g. exp(test("blah"))
      *)
      (* So, we return true if we are supposed to generate
         marshaling code, false otherwise.  If the function
         begins with Java_/Nonstub, then we don't want to to
         generate marshaling code *)
      not (is_special_function fnm);
  end

(*---------------------------------------------------------------------------*)
(* Utility functions that are used by both the marshaler and the demarshaler *)

(** Resolve an opaque formal parameter of a function. This function will check
 * that there is only one resolved type for the formal parameter. If there is
 * more than one resolved type, then it will emit a warning. This is simply a
 * helper function to traverse resform and return the list of types.
 *
 * NOTE: If there is a warning, this means that we must inspect it to resolve
 * the parameter appropriately, possibly replicating the function, specializing
 * it to each callsite.
 *)
let resolve_opaque_formal (v: varinfo)
    (fdec: fundec)
    (resform: (string, (int * varinfo * typ)) Hashtbl.t)
    : typ list =
  begin
    let ret = ref [] in
    let fname = fdec.svar.vname in
    let resl = (Hashtbl.find_all resform fname) in
    for i = 0 to (List.length resl) - 1 do
      let (ithloc,ithvinfo,ithtyp) = (List.nth resl i) in
      ret := ithtyp::!ret;
    done;
    (* If the pointer does not resolve to exactly one target, check annotations *)
    if (List.length !ret) <> 1 then begin
      if (Marshannot_dri.is_opaque v.vtype) then
        ret := [(Marshannot_dri.resolve_opaque_with_annot v.vtype)];
    end;
    (* Still okay *)
    if (List.length !ret) > 1
    then (addwarn ["Formal"; v.vname; "of"; fname; "resolves to >1 type"]);
    (* Not okay. This means that we potentially cannot generate any marshaling
     * code. At all places where a formal does not resolve, we must still
     * generate code to marshal/unmarshal the pointer itself. *)
    if (List.length !ret) = 0
    then (addwarn ["Formal"; v.vname; "of"; fname; "does not resolve!"]);
    !ret;
  end

(** Resolve an opaque field of a struct. This will check that there is only one
 * resolved type for the field. If there are multiple, a warning will be
 * emitted. This is simply a helper function to traverse resfld and return the
 * list of types.
 *
 * NOTE: If there is a warning, this means that we must inspect it to resolve
 * the parameter appropriately, possibly replicating the function, specializing
 * it to each callsite. *)
let resolve_opaque_field (fdec: fundec)
    (resfld: (string, (typ * fieldinfo * typ)) Hashtbl.t)
    (t_comp: typ)
    (field: fieldinfo) : typ list =
  begin
    let ret = ref [] in
    let resl = (list_bindings resfld) in
    for i = 0 to (List.length resl) - 1 do
      let (ithtyp,ithfieldinfo,ithrestyp) = (List.nth resl i) in
      if (String.compare (typ_tostring_noattr t_comp) (typ_tostring_noattr ithtyp)) = 0 &&
        (String.compare field.fname ithfieldinfo.fname) = 0
      then ret := ithrestyp::!ret;
    done;
    (* If the pointer resolves to <> 1 target, check annotations *)
    if (List.length !ret) <> 1 then begin
      if (Marshannot_dri.is_opaque field.ftype) then
        ret := [(Marshannot_dri.resolve_opaque_with_annot field.ftype)];
    end;
    (* This case is unacceptable.  It means we have a void pointer that
       resolves to more than one type.  We can generate marshaling code,
       but this almost certainly represents an error, especially if the types
       are of a different size. *)
    (*
      MJR:  We now check this error case in splitter_marshcode when we generate marshaling
      code.  The idea is that this error is only a problem if we're actually generating the
      marshaling code.  If we're not, then we don't actually care.

    if (List.length !ret) > 1
    then fatal (List.append
                  ["Field"; field.fname; "of"; (typ_tostring_noattr t_comp);
                   "resolved to"; (Printf.sprintf "%d" (List.length !ret));
                   "types in"; fdec.svar.vname; ".  Types include"]
                  (List.map (function param -> (Printf.sprintf "\"%s\"" param)) (List.map (typ_tostring) !ret))
               );
    *)
    (* Vinod says:
     * Not okay. This means that we potentially cannot generate any marshaling
     * code. We MUST resolve fields of data structures, either automatically,
     * or via annotations.
     *
     * Matt says:
     * What if it's IOMEM annotated?  We shouldn't be dereferencing it then.
     * What if it really is opaque, and we never dereference it?
     *)
    if (List.length !ret) = 0
    then (addwarn ["CHECK: PREVIOUSLY FATAL ERROR Field"; field.fname; "of"; (typ_tostring_noattr t_comp);
                 "did not resolve in"; fdec.svar.vname; ".  ";
                 "This means we have a void * that did not resolve to a type.  ";
                 "Are you using this variable anywhere?  No marshaling code is generated for it."]);
    !ret;
  end

(** Just get the names of the fields accessed. Return value will not
 * have repeated entries. It includes the type of access by the
 * function, either read, write, or arith.  If a given variable is
 * accessed via read and write, we include only the "write" access.
 * Only if a variable is "read" only, do we return "read."
 *
 * Note that we return a mixture of "write" and "arith."  We'll assume
 * these are equivalent in terms of what needs to be done.
 *)
let get_fields_accessed (ptg_query_result: (typ * string * string) list) :
    (typ * string * string) list =
  begin
    let nh : (string, (typ * string * string)) Hashtbl.t = (Hashtbl.create 5) in
    for i = 0 to (List.length ptg_query_result) - 1 do
      let ith = (List.nth ptg_query_result i) in
      let (ithtyp, ithfield, ithaccess) = ith in
      let hashkey = (typ_tostring_noattr ithtyp) ^ ithfield in

      (* Printf.fprintf stderr "[get_fields_accessed]: type %s name %s access
       * %s.\n" 
      (typ_tostring ithtyp) ithfield ithaccess;
                 *)
      if (String.compare ithaccess "read ") = 0 then
          (* Only add a "read" access if there is no access
             already present in the return *)
        add_if nh hashkey ith
      else
         
        begin
          (* Always add a "write", "arith", or any other
             exotic access (e.g. res_deref).

             Delete any existing entry if it's present.
          *)
          if (Hashtbl.mem nh hashkey) = true then
            (Hashtbl.remove nh hashkey);
          (Hashtbl.add nh hashkey ith)
        end
    done;
    let unordered = (list_bindings nh) in
    let ordered = (order_field_accesses unordered) in
    ordered;
  end

(** underef_lval: Take an LVAL of the form *p, and extract p. Only supports
 * *p's where p is an LVAL. Abort otherwise. No offsets allowed, as that does
 * not make sense for the context in which this function is supposed to be
 * used. *)
let underef_lval (lv: lval) : lval option =
  begin
    let retval = ref None in
    let (lh, off) = lv in
    (match lh with
       | Var(_) ->
           retval := None;
           (addwarn ["underef_lval: Nothing to underef"; (lval_tostring lv)]);
       | Mem(e) ->
           (match e with
              | Lval(elv) -> retval := Some(elv);
              | _ ->  retval := None;
                  let lval_str = (lval_tostring lv) in
                  (addwarn ["underef_lval: You passed me an exp that I don't support"; lval_str]);
           );
    );
    (* If the offset is NoOffset, we return a meaninful value. Else None *)
    (match off with
       | NoOffset -> ();
       | _ -> retval := None;
           let lval_str = (lval_tostring lv) in
           ((addwarn ["underef_lval: Passed an LVAL with an offset.";
                      "I don't understand what this means"; lval_str]));
    );
    !retval;
  end

(*---------------------------------------------------------------------------*)
(* Examine the list of globals and emit a list of lvals, representing global
 * names to be registered. Don't include functions here.
 * Propagate the "globreg" attribute so gen_registerglob generates the necessary
 * statements.
 *)
let get_globals_to_register () : lval list = !lvals_to_register

(** glob_dfs:
 * Given a global variable as input: Traverse it using DFS (and RECURSE annots
 * to break recursion) and emit a list of lvals reachable from the top-level
 * global. Examine if this is a globreg global, and only then register it *)
(*
let rec glob_dfs (glob: lval)
    (stkdpth: int) : lval list =
  begin
    let retval = ref [] in
    if (stkdpth > !threshold_stackdepth)
    then begin
      Printf.fprintf stderr "Potentially infinite loop in glob_dfs %s\n" (lval_tostring glob);
      !retval;
    end
    else begin
      let globtyp = typeOfLval glob in
      retval := !retval @ [glob];
      if (isCompoundType globtyp)
      then begin
        let globcomp = (tcomp_compinfo globtyp) in
        (match globcomp with
           | Some(globcompinfo) ->
               let fieldinfolist = globcompinfo.cfields in
               for i = 0 to (List.length fieldinfolist) - 1 do
                 let ith = (List.nth fieldinfolist i) in
                 let newlval = (add_field_to_lval glob ith) in
                 retval := (List.append !retval (glob_dfs newlval (stkdpth + 1)));
               done;
           | None -> ();
        );
      end else if (isPointerType globtyp)
      then begin
        (* Dereference and recurse, unless of course we have a recurse type *)
        if ((Marshannot_dri.is_recursive globtyp) = false)
        then begin
          let deref_lv = (Mem((expify_lval glob)), NoOffset) in
          retval := (List.append !retval (glob_dfs deref_lv (stkdpth + 1)));
        end;
      end;
      !retval;
    end;
  end
*)

let populate_globals_to_register
    (glist : global list)
    (modif_consts : varinfo list)
    : unit =
  begin
    let globals_to_register = ref [] in
    let reg_glob vi =
      begin
        Printf.fprintf stderr "Global to register: %s\n" vi.vname;
        globals_to_register := !globals_to_register @ [vi];
      end
    in
    for i = 0 to (List.length glist) - 1 do
      let ith = (List.nth glist i) in
      (match ith with
         | GVarDecl(v,_) ->
             if (isFunctionType v.vtype) = false && v.vaddrof then reg_glob v
         | GVar(v,_,_) ->
             if (isFunctionType v.vtype) = false && v.vaddrof then reg_glob v
         | _ -> ();
      );
    done;

    globals_to_register := !globals_to_register @ modif_consts;

    (* Register list of global variables *)
    for i = 0 to (List.length !globals_to_register) - 1 do
      let ith = List.nth !globals_to_register i in
      lvals_to_register := !lvals_to_register @ [lvalify_varinfo ith];
    done;

    let uniq lst =
      let unique_set = Hashtbl.create (List.length lst) in
      begin
        List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
        Hashtbl.fold (fun x () xs -> x :: xs) unique_set []
      end
    in
    let nodups = uniq !lvals_to_register in
    lvals_to_register := nodups;
  end
    
    
(*---------------------------------------------------------------------------*)
(* No global variables in the slave should be extern *)
let make_globals_nonextern (glist: global list) : global list =
  begin
    let retval = ref [] in
    for i = 0 to (List.length glist) - 1 do
      let ith = (List.nth glist i) in
      (match ith with
         | GVarDecl(v,_) ->
             if (v.vstorage = Extern) &&
               (String.compare v.vname "_ctype" <> 0) &&
               (String.compare v.vname "__mod_pci_device_table" <> 0) &&
               (String.compare v.vname "__mod_usb_device_table" <> 0) 
             then begin
               addwarn ["Converting to NoStorage"; v.vname;];
               v.vstorage <- NoStorage;
             end;
         | GVar(v,_,_) ->
             if (v.vstorage = Extern) &&
               (String.compare v.vname "_ctype" <> 0) &&
               (String.compare v.vname "__mod_pci_device_table" <> 0) &&
               (String.compare v.vname "__mod_usb_device_table" <> 0)
             then begin
               addwarn ["Converting to NoStorage"; v.vname;];
               v.vstorage <- NoStorage;
             end;
         | _ -> ();
      );
      retval := (List.append !retval [ith]);
    done;
    !retval;
  end

(* Delete all the specified function bodies from the list of globals.
   The idea is that these functions are defined elsewhere in user mode,
   i.e. part of the userdaemon, so it's not necessary to keep their function
   definitions in the CIL output.  This is particularly important for UML,
   because UML defines some functions like inb/outb in header files, but
   we want to use our own implementations. *)
let delete_nonstubs
    (glist: global list)
    (nonstubbed_functions : (string, bool) Hashtbl.t)
    (nonstubbed_functions_delete_proto : (string, bool) Hashtbl.t) :
    global list =
  begin
    let retval = ref [] in
    for i = 0 to (List.length glist) - 1 do
      let ith = List.nth glist i in
      (match ith with
         | GFun(fdec, loc) ->
             begin
               Printf.fprintf stderr "Considering function body for deletion: %s\n" fdec.svar.vname;
               try (ignore (Hashtbl.find nonstubbed_functions fdec.svar.vname));
                 begin
                   (* In this case, the function was on the list, so we delete it *)
                   (* The idea is to keep the prototype, delete any extra "fluff"
                      that will screw things up, and continue.  We don't want
                      to keep the body *)

                   Printf.fprintf stderr "kernel function proto: %s\n" fdec.svar.vname;

                   try (ignore (Hashtbl.find nonstubbed_functions_delete_proto fdec.svar.vname));
                     begin
                       (* In this case, we simply delete the prototype *)
                       Printf.fprintf stderr "  deleting proto %s\n" fdec.svar.vname;
                     end
                   with Not_found ->
                     begin
                       (* In this case, we have to keep the prototype. *)
                       fdec.svar.vstorage <- NoStorage; (* Get rid of inline specifically *)
                       fdec.svar.vattr <- []; (* Get rid of __always_inline__ specifically *)
                       fdec.svar.vtype <- strip_typ_attribs fdec.svar.vtype;
                       let newproto = GVarDecl(fdec.svar, loc) in
                       retval := List.append !retval [newproto];
                     end
                 end
               with Not_found ->
                 (* In this case, the function was not on the list, so we keep it *)
                 retval := List.append !retval [ith];
             end
         | GVarDecl(vi, loc) ->
             (match vi.vtype with
                | TFun (_, _, _, _) ->
                    begin
                      (*Printf.fprintf stderr "Considering function proto: %s\n" vi.vname;*)
                      try (ignore (Hashtbl.find nonstubbed_functions vi.vname));
                        begin
                          (* In this case, the function was on the list, so we delete it *)
                          (* The idea is to keep the prototype, delete any extra "fluff"
                             that will screw things up, and continue.  We don't want
                             to keep the body *)
                          Printf.fprintf stderr "kernel function proto: %s\n" vi.vname;

                          try (ignore (Hashtbl.find nonstubbed_functions_delete_proto vi.vname));
                            begin
                              (* In this case, we simply delete the prototype *)
                              Printf.fprintf stderr "  deleting proto %s\n" vi.vname;
                            end
                          with Not_found ->
                            begin
                              (* In this case, we have to keep the prototype. *)
                              vi.vstorage <- NoStorage; (* Get rid of inline specifically *)
                              vi.vattr <- []; (* Get rid of __always_inline__ specifically *)
                              vi.vtype <- strip_typ_attribs vi.vtype;
                              let newproto = GVarDecl(vi, loc) in
                              retval := List.append !retval [newproto];
                            end
                        end
                      with Not_found ->
                        (* In this case, the function was not on the list, so we keep it *)
                        retval := List.append !retval [ith];
                    end
                | _ ->
                    retval := List.append !retval [ith];
             );
         | _ ->
             retval := List.append !retval [ith];
      );
    done;
    !retval;
  end

let make_params (f : fundec) : varinfo list =
  begin
    let param_vars = f.sformals in
    let return_list = ref [] in
    for i = 0 to (List.length param_vars) - 1 do
      let ith = List.nth param_vars i in
      let newvar = get_formal_variable f ith.vname in
      return_list := !return_list @ [newvar];
    done;
    !return_list;
  end

let gen_function_id_map () : global list =
  begin
    let offset_init_list_full_unsorted = ref [] in
    for i = 0 to 1000 - 1 do
      let str = Printf.sprintf "%d" i in
      let sing_init = SingleInit(Const(CStr(str))) in
      offset_init_list_full_unsorted :=
        !offset_init_list_full_unsorted @
          [(i, Index(integer(i), NoOffset), sing_init)]
    done;
    let iter_fn key value =
      begin
        let sing_init = SingleInit (Const(CStr(key))) in
        offset_init_list_full_unsorted :=
          !offset_init_list_full_unsorted @
            [(value, Index(integer(value - 1000), NoOffset), sing_init)];
      end
    in
    Hashtbl.iter iter_fn function_id_mappings;
    let offset_init_list_full_sorted = List.sort compare !offset_init_list_full_unsorted in
    let offset_map orig =
      let (value, i, init) = orig in
      (i, init)
    in
    let offset_init_list_sorted = List.map offset_map offset_init_list_full_sorted in
    let array_len = List.length offset_init_list_sorted in
    let array_len_int = integer(array_len) in
    let array_type = TArray(TInt(IChar, []), Some(integer(128)), []) in
    let array_array_type = TArray(array_type, Some(array_len_int), []) in
    let new_global_compound_init = CompoundInit (array_array_type, offset_init_list_sorted) in
    let new_global_varinfo = get_global_variable array_array_type "function_id_map" in
    let new_global_initinfo : initinfo = { init = Some(new_global_compound_init); } in
    let new_global = GVar(new_global_varinfo, new_global_initinfo, locUnknown) in
    let new_global_len_init = SingleInit (Const(CInt64(Int64.of_int array_len, IInt, None))) in
    let new_global_len_initinfo : initinfo = { init = Some(new_global_len_init); } in
    let new_global_len_varinfo = get_global_variable (TInt(IInt, [])) "function_id_map_len" in
    let new_global_len = GVar(new_global_len_varinfo, new_global_len_initinfo, locUnknown) in
    [new_global; new_global_len]
  end
