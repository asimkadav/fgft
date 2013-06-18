open Cil

(* Feature-mode we're using
   PLEASE TRY TO MINIMIZE THE USE OF THIS
*)
let feature_mode = ref ""

(* True if we want to generate _check calls, false otherwise.
   Useful for compiling the driver without the corresponding
   test framework
*)
let do_symdriver_test = ref false

(* The name of the function to lookup pointers in the hashtable *)
(* This will have to be set to different values, based upon whether
 * we're generating code in the user side or in the kernel side *)
let nooks_ptrlookup_fn = ref "DUMMY_pointer_lookup_function"

(* The name of the function used to explicitly allocate memory for
 * arrays. This again will have to be set to different values based
 * upon whether we're generating the kerndriver or the userdriver *)
let nooks_arrayalloc_fn = ref "DUMMY_array_alloc_function"

(* The name of the function used to register functions whose addresses
 * are taken with the object tracker *)
let nooks_registerfn_fn = ref "DUMMY_registerfn_function"

(* The name of the function used to store addresses of offsets into
 * structures. *)
let nooks_storeoffset_fn = ref "DUMMY_store_offset"

(* Declare the C struct for struct marshret_struct, the structure that we
 * use as a return value from our marshwrap functions. It contains a voidptr,
 * pointing to the buffer that stores the marshaled information, and an int,
 * which is a length field indicating the length of the voidptr. The variable
 * below is just the compinfo. We've to add this to the globals of the program
 * using GCompTags *)
let struct_marshret_buf = "buf"
let struct_marshret_len = "len"

(* Declare the C struct for struct req_args, the structure that is used by
 * the disp_user/disp_kern functions. It contains a function_id (int), a pointer
 * to the buffer that stores marshaled information (voidptr), and a length
 * field (int) *)
let struct_reqargs_funcid = "function_id"
let struct_reqargs_data = "data"
let struct_reqargs_length = "length"

let func_cons_struct_marshret (c: compinfo) =
  begin
    let field1 = (struct_marshret_buf, voidPtrType, None, [], locUnknown) in
    let field2 = (struct_marshret_len, intType, None, [], locUnknown) in
    [field1; field2]
  end

let struct_marshret_compinfo = mkCompInfo true "marshret_struct" func_cons_struct_marshret []

let marshret_buf = getCompField struct_marshret_compinfo struct_marshret_buf
let marshret_len = getCompField struct_marshret_compinfo struct_marshret_len

let func_cons_struct_reqargs (c: compinfo) =
  begin
    let field1 = (struct_reqargs_funcid, intType, None, [], locUnknown) in
    let field2 = (struct_reqargs_data, voidPtrType, None, [], locUnknown) in
    let field3 = (struct_reqargs_length, intType, None, [], locUnknown) in
    [field1; field2; field3]
  end

let struct_reqargs_compinfo = mkCompInfo true "req_args" func_cons_struct_reqargs []
let struct_reqargs_typ = TComp(struct_reqargs_compinfo, [])

let reqargs_funcid = getCompField struct_reqargs_compinfo struct_reqargs_funcid
let reqargs_data =   getCompField struct_reqargs_compinfo struct_reqargs_data
let reqargs_length = getCompField struct_reqargs_compinfo struct_reqargs_length

(* Threshold_stackdepth: is the threshold of recursion of the marshaling
 * function that we can see before we report an infinite loop
 * ath_pci really does require a depth of 25 in order to marshal everything
 * without falsely detecting a loop.
 *)
let threshold_stackdepth = ref 26

(* 64-bit Zero Integer *)
let zero64 = (Int64.of_int 0)

(* The same thing in expression form for unsigned zero64 *)
let zero64Uexp = Const(CInt64(zero64,IUInt,None))

(* The same thing in expression form, this time, signed *)
let zero64Sexp = integer 0 (* Const(CInt64(zero64,IInt,None)) *)
  
(* Minus One *)
let minusone_exp = integer (0 - 1)

let marshwrap_prefix =   "__MARSH_WRAP__"
(* What should I prefix stub functions with? *)
let stub_prefix =        "__USER_SPACE__"
let stubmarsh_prefix =   "MARSH___STUB__"
let stubdemarsh_prefix = "DEMARSH_STUB__"

