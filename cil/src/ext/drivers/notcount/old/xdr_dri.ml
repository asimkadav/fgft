(*===========================================================================*)
(*
 * CIL module to generate XDR specification automatically.
 *
 * Matt Renzelmann <mjr@cs.wisc.edu> Feb 18 2008
 * 
 * The following list details the known shortcomings of this tool.
 * 
 * - Unions.  These are particularly problematic, because their XDR
 *   syntax is completely different from a standard C union.  At
 *   present, we simply delete all unions.  There are relatively few, so
 *   this is reasonable for testing.  Initially, it seemed like
 *   converting them to structs for XDR would work.  The problem here is
 *   that once the code is integrated with the driver, conflicts arise
 *   (union vs. struct type issues).  I suspect this can be resolved
 *   with some more work.
 * 
 * - It remains to be seen how well variable-sized arrays are handled.
 *   These are currently being represented in XDR as pointers, but I'm
 *   afraid XDR will only marshal a single element if it's not NULL,
 *   rather than the correct size of array.  Fixing this will require
 *   annotations of the original code--fortunately we should be able
 *   to re-use the annotations that we have, since they supply the
 *   missing information.
 * 
 * - Arrays declared like "int myarray[sizeof (asdf)]" aren't
 *   supported, since it would require computing the constant
 *   expression inside the array bounds and outputting that instead.
 *   It currently replaces these weird expressions with the constant
 *   1.
 * 
 * - Function pointers.  This is a tricky one, and the tool currently
 *   just converts them to unsigned long.  Handling these properly
 *   between C and Java will take additional thought.
 * 
 * - Empty structures.  If a structure is defined to contain no
 *   elements, we have to delete it.  These arise after preprocessing
 *   the CONFIG_* kernel macros.  Additionally, it was necessary to
 *   recursively delete other structures that became empty once the
 *   initial ones were deleted.
 * 
 * - Reserved word variable names.  This was amusing: having a variable
 *   like "int class;" works in C, but not in Java.  These fields are
 *   currently deleted.  Several possible solutions exist, but the most
 *   obvious is simply to rename the variable in the original code.
 * 
 * - Bitfields are deleted.  As we discussed earlier, the fix is simply
 *   to remove them from the original code.
 * 
 * - void * is converted to char *.
 *)
(*===========================================================================*)

open Cil
open Scanf
open Utils_dri            (* General utilities *)
open Marshannot_dri       (* For handling array size annotation *)

(* Sometimes, those nasty C programmers will name variables the same
   as Java reserved words.  Bad, bad!  We simply delete them from the
   interface, so you can't use these in Java land.  Additionally, rpcgen
   seems to have trouble with some field names, like "version" and "string"??
   Reasons are unknown.
*)
let bad_field_names : string list = [ "version"; "private"; "class"; "string"; "interface" ];;

(*---------------------------------------------------------------------------*)
(** Extract-xdr.  This class deals with analyzing kernel headers and driver
    source code to extract all relevant structures and translate them to
    XDR format.
 *)
class extract_xdr = object (self)
  inherit nopCilVisitor  

  (* Argument passed in from command line.  Either "java" or "c" depending
     on what we are generating XDR for.
  *)
  val mutable java_or_c : string = "";

  (* This list stores the names of structures for which we have a
     complete declaration, i.e. not incomplete.  That means the
     struct must have a body defined in the input.  If the struct
     does not have a body, it won't appear on this list, and we
     won't attempt to generate marhsaling code for it.
  *)
  val mutable complete_declarations : string list = [];

  (* Used to address the array of pointers issue.
     rpcgen requires all declarations of the form
     struct mystruct *blah[100]
     to be replaced with a typedef.
  *)
  val mutable new_typedefs : global list = [];

  (* Used when creating new structs that contain only one element.
     This hack is used to deal with the case that we have an annotated
     fixed length array, e.g. int * __attribute__((exp(5))) blah;
     so we create a separate struct to hold it.
  *)
  val mutable new_globals : global list = [];

  (* Stores a list of all structures that we simply deleted.
     Unfortunately, there are a preponderance (OK, a few) structures
     that, when all the kernel CONFIG_BLAH preprocessing is done,
     end up being empty.  We can't generate marshaling code
     for such structures, and RPCGEN doesn't like empty declarations
     so we have to handle these carefully.  We'll store the names
     of such structs in this list.  *)
  val mutable empty_structs : string list = [];

  (* Keeps only type definitions like structs.
     Returns:
     true = keep
     false = discard
  *)
  method initial_filter (glob: global) : bool =
    begin
      (match glob with
         | GType(t, _) -> true; (* A typedef *)
         | GCompTag(c, _) -> (* A structure definition *)
             (* Empty structures not allowed by rpcgen *)
             (* TODO we are discaring all unions! :( *)
             if (List.length c.cfields = 0) or (c.cstruct = false) then
               begin
                 empty_structs <- c.cname :: empty_structs;
                 false
               end
             else
               true;
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

  (* Builds a list of names of structs for which we have a complete definition.
     Later, we will convert pointers to structs for which we don't have a definition
     to unsigned longs.
  *)
  method find_complete_decl (glob: global) : string =
    begin
      (match glob with
         | GCompTag (c, _) -> c.cname;
         | _ -> "";
      );
    end

  method remove_empty_decl (name: string) : bool = 
    if name <> "" then true else false;

  (* This doesn't actually create any forward declarations.  Instead,
     it creates comments of the form:
     % struct blah;
     which are emitted by rpcgen verbatim, sans % sign.
     These are needed to placate GCC.
  *)
  method create_forward_decl (glob: global) : global =
    begin
      if java_or_c = "c" then
        begin
          let temp_decl = GText ("/* No forward decl for global */") in
          let new_decl = ref temp_decl in
          (match glob with
             | GCompTag (c, loc) ->
                 (* let new_forward_decl = GCompTagDecl (c, loc) in
                    new_decl := new_forward_decl;
                 *)
                 new_decl := GText ("% struct " ^ (self#get_new_name c.cname) ^ ";");
             | _ -> ();
          );
          !new_decl;
        end
      else
        GText ("/* No forward decls for Java XDR file */");
    end

  (* Part of the general type modification scheme.  Gets rid of
     "signed char" type and replaces it with "char"
  *)
  method typ_modify_TInt (orig_typ: typ) (k: ikind) (a: attributes) : typ =
    begin
      let new_typ = ref orig_typ in
      if k = ISChar then
        begin
          new_typ := TInt (IChar, a);
          Printf.fprintf stderr "Fixing %s -> %s\n"
            (typ_tostring orig_typ) (typ_tostring !new_typ);
          !new_typ;
        end
      else
        !new_typ;
    end

  (* Constructs a structure containing a single member:  an array of
     some kind of simple thing, like unsigned char array[100];
     Returns a typedef for a pointer to one of these structures.
  *)
  method construct_fixedarray (struct_name : string)
    (typedef_name : string) (t : typ) (len: exp) : typeinfo =
    begin
      let func_cons_fixedarray (c: compinfo) =
        begin
          let field = ("array_" ^ (exp_tostring len), t, None, [], locUnknown) in
          [field]
        end in
      let new_ci = mkCompInfo true struct_name func_cons_fixedarray [] in
      let new_glob = GCompTag (new_ci, locUnknown) in
      if (List.mem new_glob new_globals) = false then
        new_globals <- new_globals @ [new_glob];
      let new_typ = TComp (new_ci, []) in
      self#construct_typedef_ptr typedef_name new_typ;
    end

  (* Called in typ_modify_TPtr to handle is_blob structure members *)
  method handle_is_blob (orig_typ: typ) (sub_typ: typ) (base_typedef_name: string) (a: attributes): typ = 
    begin
      let len = Marshannot_dri.get_fixed_array_length orig_typ in
      let struct_name = "is_blob_" ^ (makevarname (typ_tostring sub_typ)) in
      let typedef_name = base_typedef_name ^ "_" ^ (exp_tostring len) in
      let new_typeinfo_ptr = self#construct_fixedarray
        struct_name typedef_name (TArray (sub_typ, Some (len), a)) len in
      (*let new_typeinfo_ptr = self#construct_typedef_ptr name (TNamed (new_typeinfo, [])) in*)
      let new_typ = TNamed (new_typeinfo_ptr, a) in
      Printf.fprintf stderr "Fixing is_blob %s -> %s\n"
        (typ_tostring orig_typ) (typ_tostring new_typ);
      new_typ;
    end

  (* A complex one.  How to handle pointers to various things. *)
  method typ_modify_TPtr (orig_typ: typ) (sub_typ: typ) (a: attributes) : typ =
    begin
      let new_typ = ref orig_typ in
      (match sub_typ with
         | TVoid(a) ->
             (* Convert void * to unsigned long *)
             new_typ := TInt (IULong, a);
             Printf.fprintf stderr "Fixing TVoid %s -> %s\n"
               (typ_tostring orig_typ) (typ_tostring !new_typ);
         | TPtr (_, _) ->
             (* Convert struct blah ** or void ** to unsigned long *)
             new_typ := TInt (IULong, a);
             Printf.fprintf stderr "Fixing TPtr %s -> %s\n"
               (typ_tostring orig_typ) (typ_tostring !new_typ);
         | TFun (sub_typ, l, b, _) ->
             (* Convert function ptrs to unsigned long *)
             new_typ := TInt (IULong, a);
             Printf.fprintf stderr "Fixing TFun %s -> %s\n"
               (typ_tostring orig_typ) (typ_tostring !new_typ);
         | TNamed (typei, a) -> (* A typedef, e.g. gid_t *array *)
             if (Marshannot_dri.is_iomem orig_typ) = false then
               if Marshannot_dri.is_blob orig_typ = false then
                 begin
                   (* Pointer to a single object *)
                   new_typ := TPtr (self#modify_typ sub_typ false, a);

                   Printf.fprintf stderr "Fixing TNamed %s -> %s\n"
                     (typ_tostring orig_typ) (typ_tostring !new_typ);
                 end
               else
                 begin
                   (* Pointer to multiple objects *)
                   let base_typedef_name = typei.tname in
                   new_typ := self#handle_is_blob orig_typ sub_typ base_typedef_name a;
                   
                   Printf.fprintf stderr "Fixing TNamed (blob) %s -> %s\n"
                     (typ_tostring orig_typ) (typ_tostring !new_typ);
                 end
             else
               begin
                 (* Pointer to IOMEM *)
                 new_typ := TInt (IULong, a);
                 Printf.fprintf stderr "Fixing TNamed (iomem) %s -> %s\n"
                   (typ_tostring orig_typ) (typ_tostring !new_typ);
               end
         | TArray (sub_typ, e, a) ->
             Printf.fprintf stderr "!!!!!!!! Check this one %s\n"
               (typ_tostring orig_typ);
         | TComp (compi, a) ->
             if Marshannot_dri.is_iomem orig_typ = false then
               if Marshannot_dri.is_blob orig_typ = false then
                 if List.mem compi.cname complete_declarations = false then
                   (* Not IOMEM, single pointer, no structure definition *)
                   new_typ := TInt (IULong, a)
                 else
                   (* Not IOMEM, single pointer, we have the structure definition *)
                   new_typ := TPtr (self#modify_typ sub_typ true, a)
               else
                 if List.mem compi.cname complete_declarations = false then
                   (* Not IOMEM, pointer to array, no structure definition *)
                   new_typ := TInt (IULong, a)
                 else
                   (* Not IOMEM, pointer to array, we have the structure definition *)
                   let base_typedef_name = compi.cname ^ "_" in
                   new_typ := self#handle_is_blob orig_typ sub_typ base_typedef_name a
             else
               begin
                 (* IOMEM: don't do anything (int only) *)
                 new_typ := TInt (IULong, a);
                 Printf.fprintf stderr "Fixing TComp (iomem) %s -> %s\n"
                   (typ_tostring orig_typ) (typ_tostring !new_typ);
               end
         | TInt (ikind, a) ->
             (* Convert pointers to primitives to typedefs *)
             if (Marshannot_dri.is_iomem orig_typ) = false then
               if Marshannot_dri.is_blob orig_typ then
                 let base_typedef_name = self#get_primitive_typedef ikind in
                 new_typ := self#handle_is_blob orig_typ sub_typ base_typedef_name a
               else
                 begin
                   let name = self#get_primitive_typedef ikind in
                   let new_typeinfo = self#construct_typedef
                     name (TInt (ikind, [])) in
                   let new_typeinfo_ptr = self#construct_typedef_ptr
                     name (TNamed (new_typeinfo, [])) in
                   new_typ := TNamed (new_typeinfo_ptr, a);
                   Printf.fprintf stderr "Fixing TInt (not blob) %s -> %s\n"
                     (typ_tostring orig_typ) (typ_tostring !new_typ);
                 end
             else
               begin
                 new_typ := TInt (IULong, a);
                 Printf.fprintf stderr "Fixing TPtr (iomem) %s -> %s\n"
                   (typ_tostring orig_typ) (typ_tostring !new_typ);
               end
         | _ ->
             new_typ := TPtr (self#modify_typ sub_typ true, a);
      );
      !new_typ
    end

  (* Creates a typedef of the given name and type.
     Returns only the typeinfo. *)
  method construct_typedef (name : string) (t : typ) : typeinfo = 
    begin
      let new_typeinfo : typeinfo = {
        tname = name;
        ttype = t;
        treferenced = false;
      } in
      let new_glob = GType (new_typeinfo, locUnknown) in
      if (List.mem new_glob new_typedefs) = false then
        new_typedefs <- new_typedefs @ [new_glob];
      new_typeinfo;
    end

  method construct_typedef_ptr (name: string) (t: typ) : typeinfo =
    begin
      self#construct_typedef (name ^ "_ptr") (TPtr (t, []));
    end

  method get_primitive_typedef (kind: ikind) : string = 
    match kind with
      | IChar -> "typedef_char";
      | ISChar -> "typedef_signedchar";
      | IUChar -> "typedef_unsignedchar";
      | IInt -> "typedef_int";
      | IUInt -> "typedef_unsignedint";
      | IShort -> "typedef_short";
      | IUShort -> "typedef_unsignedshort";
      | ILong -> "typedef_long";
      | IULong -> "typedef_unsignedlong";
      | ILongLong -> "typedef_longlong";
      | IULongLong -> "typedef_unsignedlonglong";
      | IBool -> "typedef_bool";

  (* Arrays of pointers.  To make RPCGEN happy, we have to use a typedef.
     This routine converts code like this:
     struct mystruct *blah[10]
     to code like this:
     typedef struct mystruct mystruct_typedef;
     typedef mystruct_typedef *mystruct_typedef_ptr;
     mystruct_typedef_ptr blah[10];
  *)
  method typ_modify_TArray_of_TPtr
    (orig_typ: typ)
    (sub_typ: typ)
    (ptr_typ: typ)
    (e: exp option)
    (a: attributes) : typ =
    begin
      let new_typ = ref orig_typ in
      (match ptr_typ with
         | TComp (compi, a) -> (* A struct, e.g. struct my_struct *array[10] *)
             if (Marshannot_dri.is_iomem sub_typ) = false then
               if (List.mem compi.cname complete_declarations) = false then
                 (* This is an array of pointers to a struct for which we do
                    not have the definition.  e.g. struct page *blah[10]; where
                    page is incomplete or deleted because it's empty. *)
                 new_typ := TInt (IULong, a)
               else
                 begin
                   (* The normal case, where we have a definition for the struct *)
                   let new_typeinfo = self#construct_typedef_ptr
                     compi.cname (TComp (compi, [])) in
                   new_typ := TArray (TNamed (new_typeinfo, a), e, []);
                   Printf.fprintf stderr "Fixing array of structure pointers %s -> %s\n"
                     (typ_tostring orig_typ) (typ_tostring !new_typ);
                 end
             else
               begin
                 new_typ := TArray (TInt (IULong, a), e, []);
                 Printf.fprintf stderr "Fixing array of structure pointers (iomem) %s -> %s\n"
                   (typ_tostring orig_typ) (typ_tostring !new_typ);
               end
         | TNamed (typei, a) -> (* A typedef, e.g. gid_t *array[10] *)
             let new_typeinfo = self#construct_typedef_ptr
               typei.tname (TNamed (typei, [])) in
             new_typ := TArray (TNamed (new_typeinfo, a), e, []);
             Printf.fprintf stderr "Fixing array of typedef pointers %s -> %s\n"
               (typ_tostring orig_typ) (typ_tostring !new_typ);
         | TInt(kind, a) -> (* Primitive type *)
             if (Marshannot_dri.is_iomem sub_typ) = false then
               begin
                 let name = self#get_primitive_typedef kind in
                 let new_typeinfo = self#construct_typedef
                   name (TInt (kind, [])) in
                 let new_typeinfo_ptr = self#construct_typedef_ptr
                   name (TNamed (new_typeinfo, [])) in
                 new_typ := TArray (TNamed (new_typeinfo_ptr, a), e, []);
                 Printf.fprintf stderr "Fixing array of primitive pointers %s -> %s\n"
                   (typ_tostring orig_typ) (typ_tostring !new_typ);
               end
             else
               begin
                 new_typ := TArray (TInt (IULong, a), e, []);
                 Printf.fprintf stderr "Fixing array of primitive pointers (iomem) %s -> %s\n"
                   (typ_tostring orig_typ) (typ_tostring !new_typ);
               end
         | TVoid (a) -> (* Void.  e.g. void *blah[10]; *)
             let kind = IChar in
             let name = self#get_primitive_typedef kind in (* Convert to char * ptr *)
             let new_typeinfo = self#construct_typedef
               name (TInt (kind, [])) in
             let new_typeinfo_ptr = self#construct_typedef_ptr
               name (TNamed (new_typeinfo, [])) in
             new_typ := TArray (TNamed (new_typeinfo_ptr, a), e, []);
             Printf.fprintf stderr "Fixing array of void pointers %s -> %s\n"
               (typ_tostring orig_typ) (typ_tostring !new_typ);
         | _ ->
             Printf.fprintf stderr "!!!!!!! Error check this case %s\n" (typ_tostring ptr_typ);
      );
      !new_typ
    end
        
  (* Fixes arrays.  This includes weird situations like arrays of pointers, which we need to
     convert to typedefs.  It also includes arrays declared with sizes defined by expressions.
  *)
  method typ_modify_TArray
    (orig_typ: typ)
    (sub_typ: typ)
    (e: exp option)
    (a: attributes) : typ =
    begin
      let new_typ = ref orig_typ in
      (* Convert arrays of pointers to typedefs *)
      (match sub_typ with
         | TPtr(ptr_typ, a) ->
             new_typ := self#typ_modify_TArray_of_TPtr orig_typ sub_typ ptr_typ e a;
         | _ ->
             (match e with
                | Some (Const (c)) ->
                    new_typ := TArray (self#modify_typ sub_typ true, e, a);
                | None ->
                    new_typ := TArray (self#modify_typ sub_typ true, e, a);
                | _ ->
                    (* Weird case:  Arrays with interesting expressions like this:
                       [(int )(sizeof(uid_t ) - sizeof(int ))]
                       rpcgen won't support that.
                    *)
                    new_typ := TArray (sub_typ, Some(Cil.integer 1), a);
                    Printf.fprintf stderr "Fixing %s -> %s\n"
                      (typ_tostring orig_typ) (typ_tostring !new_typ);
             );
      );
      
      !new_typ;
    end

  (* The purpose of this method is to simplify typedefs in cases where
     it doesn't matter.  In particular, we have lots of typedefs like
     typedef uint32 unsigned long.  In this case, it's necessary
     to reference the value by adding ".value" in the Java field.
     This is cumbersome, so we convert typedefs like this to the
     primitive type.

     Note that in some instances, we do NOT want to simplify typedefs.
     The one case currently known is as follows:
        u64 *blah;
     In this case, u64 is a typedef that resolves to unsigned long long,
     which becomes:
        unsigned hyper *blah;
     in the RPC output.  The trouble is that JRPCGEN does not deal with
     this case properly: it's buggy, and translates this to "long blah"
     in the generated class.  In contrast, RPCGEN handles the pointer
     appropriately.
  *)
  method typ_modify_TNamed (orig_typ: typ) (modify_typedefs: bool): typ =
    match orig_typ with
      | TNamed (tinfo, a) ->
          (match tinfo.ttype with
             | TVoid(a) -> if (modify_typedefs) then tinfo.ttype else orig_typ;
             | TInt(k,a) -> if (modify_typedefs) then tinfo.ttype else orig_typ;
             | TFloat(k,_) -> if (modify_typedefs) then tinfo.ttype else orig_typ;
             | TPtr(sub_typ,a) -> orig_typ;
             | TArray(sub_typ,e,a) -> orig_typ;
             | TFun(sub_typ,_,_,_) -> orig_typ;
             | TNamed(tinfo,a) -> self#typ_modify_TNamed tinfo.ttype modify_typedefs;
             | TComp(cinfo,_) -> orig_typ;
             | TEnum(einfo,_) -> if (modify_typedefs) then tinfo.ttype else orig_typ;
             | TBuiltin_va_list(_) -> orig_typ;
          )
      | _ -> Printf.fprintf stderr "!!!!!!! Error check this case %s\n" (typ_tostring orig_typ);
          orig_typ;
          
  method typ_modify_TFun (orig_typ: typ) (sub_typ: typ) : typ = 
    begin
      (* This is a weird one.  e.g:
         typedef void __signalfn_t(int); // Matches the current TFun type
         This typedef might then be used as follows:
         typedef __signalfn_t *__sighandler_t;
      *)
      let new_typ = ref orig_typ in
      new_typ := TInt (IULong, []); (* Convert function ptrs to unsigned long *)
      !new_typ;
    end

(*
  method typ_modify_TEnum (orig_typ: typ) : typ =
    TInt (IULong, []);
*)

  (* Generic function that converts a weird typ into one that
     rpcgen is more likely able to handle.  Note that this process
     inherently loses information, so some things cannot be marshaled
     precisely
  *)
  method modify_typ (orig_typ: typ) (modify_primitive_typedefs: bool): typ = 
    let new_typ = ref orig_typ in
    (match orig_typ with
       | TVoid(a) ->
           TInt (IChar, a);
       | TInt(k,a) ->
           self#typ_modify_TInt orig_typ k a;
       | TFloat(k,_) -> !new_typ;
       | TPtr(sub_typ,a) ->
           self#typ_modify_TPtr orig_typ sub_typ a;
       | TArray(sub_typ,e,a) -> 
           self#typ_modify_TArray orig_typ sub_typ e a;
       | TFun(sub_typ,_,_,_) ->
           self#typ_modify_TFun orig_typ sub_typ;
       | TNamed(tinfo,a) ->
           self#typ_modify_TNamed orig_typ modify_primitive_typedefs;
       | TComp(cinfo,_) -> !new_typ;
       | TEnum(einfo,_) -> !new_typ;
           (*self#typ_modify_TEnum orig_typ;*)
       | TBuiltin_va_list(_) -> !new_typ;
    );

  (* Compare the type of each field, and return false if the type
     is on the empty_struct list.  Used to delete fields that are
     declared as an empty struct. *)
  method remove_empty_structs (finfo: fieldinfo) : bool =
    match finfo.ftype with
      | TComp (ci, _) -> (* It's an inline struct e.g. struct mystruct x;
                            Thus, it need to be deleted *)
          if (List.mem ci.cname empty_structs) = true then
            false
          else
            true;
      | _ -> true;

  (* Given a typ, returns true if it's not a union or a typedef of a union,
     false if it is a union or a typedef of a union *)
  method typ_is_not_union (t: typ) : bool =
    begin
      (match t with
         | TComp(c, a) -> c.cstruct
         | TNamed(ti, a) -> self#typ_is_not_union ti.ttype;
         | _ -> true
      )
    end

  method type_is_not_2d_array (t: typ) : bool =
    begin
      (match t with
         | TArray(t2,_,_) ->
             (match t2 with
                | TArray (_,_,_) -> false
                | _ -> true
             )
         | _ -> true
      )
    end

  (* Re-generate each global, after making a number of changes as needed
     to ensure XDR compatibility. *)
  method recreate_glob (glob: global) : global = 
    begin
      let new_glob = ref glob in
      (match glob with
         | GType(t, a) -> (* A typedef *)
             begin
               t.ttype <- self#modify_typ t.ttype true;
               new_glob := GType (t, a)
             end
         | GCompTag(c, loc) -> (* A structure definition *)
             begin
               (* Examine all the fields.  If any are of the form
                  struct mystruct x;
                  where mystruct is an empty struct, then delete
                  the field entirely *)
               c.cfields <- List.filter self#remove_empty_structs c.cfields;

               (* Discard all union fields *)
               c.cfields <- List.filter (function x -> self#typ_is_not_union x.ftype) c.cfields;

               (* Now we instead rename the bad fields.  This requires a 
                  subsequent pass with a perl script, to remove "_bad_field_name"
                  from the C half of things.  In Java, we can keep the suffix
                  because we're writing that code from scratch anyway.
                  Besides, there's no fundamental reason that "version" shouldn't be
                  allowed on the C half, which is why it's present in some structures.
               *)
               List.iter (function finfo ->
                            if (List.mem finfo.fname bad_field_names) then
                              finfo.fname <- finfo.fname ^ "_bad_field_name"
                         ) c.cfields;
               
               (* Fix bitfields by deleting them altogether.  This may cause
                  problems.  The fix is to re-write the C code so as not to use
                  bit fields!  We can't simply remove the bit fields because
                  the marshaling code will choke when we substitute the XDR
                  structure definitions with the actual ones (which would include
                  the bitfields).  It's simply not possible to marshal bitfields
                  in XDR.
               *)
               c.cfields <- List.filter (function finfo ->
                                           (match finfo.fbitfield with
                                              | Some (_) -> false
                                              | None -> true
                                           );
                                        ) c.cfields;

               (* Delete all multi-dimensional arrays.  This could be fixed,
                  but we don't need to now so forget it :)
               *)
               c.cfields <- List.filter (function x -> self#type_is_not_2d_array x.ftype) c.cfields;

               (* Fix field types, like void * etc *)
               List.iter (function finfo ->
                            finfo.ftype <- (self#modify_typ finfo.ftype true)
                         ) c.cfields;

               (* TODO: We are just converting unions to structs.
                  Unions are hard to deal with because the XDR specification
                  requires them to be in a special format.  The easy fix is to
                  treat them as structs, but this doesn't work when it comes
                  time to merge the marshaling code with the original because
                  all our types are structs but the original types are unions.
               *)
               (*c.cstruct <- true;*)

               (* Construct the new struct, or delete it as the case may be *)
               if List.length c.cfields = 0 then
                 new_glob := GText ("/* Struct deleted " ^ c.cname ^ " because it was empty */")
               else
                 new_glob := GCompTag (c, loc);
             end
         | GEnumTag(e, loc) -> (* An enum *)
             begin
               new_glob := glob;
             end
         | GCompTagDecl(c, loc) -> (* A forward declaration that we added *)
             begin
               new_glob := glob
             end
         | _ ->
             begin
               (Printf.fprintf stderr "Error in extract_xdr.recreate_glob.  Fix it\n");
               new_glob := glob;
             end
      );
      !new_glob;
    end

  (* Removes all __attribute__ GCC keywords, because these
     are not allowed in .X files *)
  method remove_attrs (glob: global) : unit = 
    begin
      (match glob with
         | GType(t, _) -> (* A typedef *)
             ();
         | GCompTag(c, _) -> (* A structure definition *)
             (List.iter (function finfo -> (finfo.fattr <- [])) c.cfields);
             (List.iter (function finfo ->
                           finfo.ftype <- (strip_typ_attribs finfo.ftype)
                        ) c.cfields);
             c.cattr <- [];
         | GEnumTag(e, _) -> (* An enum *)
             ();
         | GText (_) -> (* A comment *)
             ();
         | GCompTagDecl(c, loc) -> (* A forward declaration, that we added *)
             ();
         | _ -> (Printf.fprintf stderr "Error in extract_xdr.remove_attrs.  Fix it\n");
      );
    end

  (* Append the suffix to the type name only if it's not been appended already.
     This is important, because the CIL data structures are so screwed up by now
     that we have references and copies of types all over the place.  We'll just
     traverse all typs, and append the string if we haven't already. *)
  method get_new_name (oldname: string) : string = 
    begin
      let suffix = if java_or_c = "c" then "_autoxdr_c" else "_autoxdr" in
      let suffix_len = (String.length suffix) in
      let oldname_len = (String.length oldname) in
      if (String.length oldname) < suffix_len then
        oldname ^ suffix
      else
        let end_chars = (String.sub oldname (oldname_len - suffix_len) suffix_len) in
        if end_chars = suffix then
          oldname
        else 
          oldname ^ suffix;
    end

  (* Given a type, rename it with the appropriate suffix so as not
     to conflict with rpc.h *)
  method rename_typ (t: typ) : unit =
    match t with
      | TVoid(_) -> ();
      | TInt(_,_) -> ();
      | TFloat(_,_) -> ();
      | TPtr(sub_typ,_) -> self#rename_typ sub_typ;
      | TArray(sub_typ,_,_) -> self#rename_typ sub_typ;
      | TFun(sub_typ,_,_,_) -> self#rename_typ sub_typ;
      | TNamed(tinfo,_) -> tinfo.tname <- self#get_new_name tinfo.tname;
      | TComp(cinfo,_) -> cinfo.cname <- self#get_new_name cinfo.cname;
      | TEnum(einfo,_) ->
          begin
            einfo.ename <- self#get_new_name einfo.ename;
          end
      | TBuiltin_va_list(_) -> ();

  (* Rename the type of a single field in a structure *)
  method rename_field (finfo: fieldinfo) : unit =
    self#rename_typ finfo.ftype;

  (* Renames all types to _autoxdr to prevent any conflicts with 
     the included rpc.h header file *)
  method rename_everything (glob: global) : unit =
    match glob with
      | GType(t, _) -> (* A typedef *)
          t.tname <- self#get_new_name t.tname;
      | GCompTag(c, _) -> (* A structure definition *)
          c.cname <- self#get_new_name c.cname;
          List.iter self#rename_field c.cfields;
      | GCompTagDecl(c, _) -> (* A forward structure declaration *)
          c.cname <- self#get_new_name c.cname;
      | GEnumTag(e, _) -> (* An enum *)
          (* Renames an enum value *)
          e.ename <- self#get_new_name e.ename;

          (* Renames all the values inside the enum *)
          let get_new_item = (function (value: (string * exp * location)) ->
                                let (s, e, l) = value in
                                ((self#get_new_name s), e, l)) in
          e.eitems <- List.map get_new_item e.eitems;
      | GEnumTagDecl(e, _) -> (* A forward enum declaration *)
          e.ename <- self#get_new_name e.ename;
      | GText (_) ->
          ();
      | _ -> Printf.fprintf stderr "Error in rename_everything\n";

  (* Map a global to a typedef.  The idea is to examine all the globals
     and return only those which are typedefs.  Other non-typedefs are replaced by comments. *)
  method extract_orig_typedefs (glob: global) : global =
    match glob with
      | GType (t, _) -> (* A typedef *)
          if self#typ_is_not_union t.ttype = true then
            glob
          else
            GText ("/* Union typedef deleted " ^ t.tname ^ "*/");
      | _ -> GText ("/* Ignored non-typedef */");

  (* Map a global to a bool.  The idea is to examine all the globals
     and return true only if it's NOT a typedef.  We are relocating all typedefs
     to the top of the file, and by the time we get here, we've made all 
     the necessary copies. *)
  method filter_orig_typedefs (glob: global) : bool =
    match glob with
      | GType (t, _) -> (* A typedef *)
          false;
      | _ -> true;
            
  (**************************************************************************)
  (* Top-level function.  This routine reorganizes the input C code to produce
     XDR/RPCGEN compatible output
  *)
  method generate_xdr (f: file) (args: string) : unit  =
    begin
      java_or_c <- args;
      let comment_forward_decls =  [ GText ("/* BEGIN FORWARD DECLARATIONS */") ] and
          comment_orig_typedefs =  [ GText ("/* BEGIN ORIGINAL TYPEDEFS */") ] and
          comment_new_typedefs =   [ GText ("/* BEGIN NEW TYPEDEFS */") ] and
          comment_filtered_globs = [ GText ("/* BEGIN FILTERED GLOBALS */") ] in
      let new_globs = List.filter self#initial_filter f.globals in
      let new_forward_decls = List.map self#create_forward_decl new_globs in
      complete_declarations <- List.filter self#remove_empty_decl (List.map self#find_complete_decl new_globs);
      let recreated_globs = List.map self#recreate_glob new_globs in
      let orig_typedefs = List.map self#extract_orig_typedefs recreated_globs in
      let filtered_globs = List.filter self#filter_orig_typedefs recreated_globs in
      let new_forward_decls = new_forward_decls @ (List.map self#create_forward_decl new_globals) in
      let final_globs = comment_forward_decls @ new_forward_decls @ 
        comment_orig_typedefs @ orig_typedefs @
        comment_new_typedefs @ new_typedefs @
        comment_filtered_globs @ filtered_globs @
        new_globals in
      List.iter self#remove_attrs final_globs;
      List.iter self#rename_everything final_globs;
      List.iter (function x -> Printf.fprintf stderr "Empty struct not included: %s\n" x) empty_structs;
      f.globals <- final_globs;
    end
end



(* This class takes care of modifying the generated XDR file to support
 * circularly linked lists, list with cycles, and recursive cyclic data
 * structures.  As soon as recursion is detected, it breaks the cycle.
 * See xdr_structs.[ch] for more information.
*)
class rewrite_xdr = object (self)
  inherit nopCilVisitor  

  (* Contains a list of all the new varinfos that we've added, so
     we can update existing references appropriately. *)
  val mutable all_new_varinfos : varinfo list = [];

  (* Get rid of anything that's not a function. Since this is XDR code,
     which is highly stylized, we know that you just need to add back
     the header file.  This approach keeps the result half-way readable,
     and easier to deal with later.
  *)
  method initial_filter (glob: global) : bool =
    match glob with
      | GType(t, _) -> false; (* A typedef *)
      | GCompTag(c, _) -> false; (* A structure definition *)
      | GCompTagDecl(c, _) -> false; (* A forward structure declaration *)
      | GEnumTag(e, _) -> false; (* An enum *)
      | GEnumTagDecl(e, _) -> false; (* A forward enum declaration *)
      | GVarDecl(v, _) -> false; (* A variable declaration *)
      | GVar(v, i, _) -> false; 
      | GFun(f, _) -> true;
      | GAsm(s, _) -> false;
      | GPragma(a, _) -> false;
      | GText (t) -> false;

  method get_exps (v: varinfo) : exp =
    expify_lval (lvalify_varinfo v);

  (* Target function format.  The body is just the two
     statements inside
     bool_t xdr_mystruct(XDR *xdrs , mystruct *objp )
     {
         static mystruct empty;
         WRAPPER_CALL(xdr_mystruct_wrapper);
     }

     orig_f: The fundec of the original function
  *)
  method get_fn_body (orig_f: fundec) : stmt list = 
    begin
      let macro_func_name = "WRAPPER_CALL" in
      let macro_func = emptyFunction macro_func_name in
      let xdr_wrapper_param = expify_fundec orig_f in
      let macro_params = [xdr_wrapper_param] in
      let call_macro_func = Call (None, expify_fundec macro_func, macro_params, locUnknown) in
      let stmt_call_macro = mkStmt (Instr [call_macro_func]) in
      [stmt_call_macro];
    end

  (* Defines an extra "empty" structure to marshal.
     This is no longer used.
  *)
  method get_fn_locals (orig_f: fundec) : varinfo list =
    begin
      if (List.length orig_f.sformals != 2) then
        Printf.fprintf stderr "Failed get_fn_locals\n";
      let second_param = List.nth orig_f.sformals 1 in
      let new_type_ptr = second_param.vtype in
      (match new_type_ptr with
         | TPtr (new_type_noptr, attrs) ->
             let new_varinfo = makeVarinfo false "empty" new_type_noptr in
             new_varinfo.vstorage <- Static;
             [new_varinfo];
         | _ ->
             Printf.fprintf stderr "Failed get_fn_locals Error 2\n";
             [];
      );
    end

  (* Make copies of all functions, and modify the originals to be called
     "_orig".  Deletes everything else.  We'll add the necessary header
     file automatically later.
  *)
  method initial_map (glob: global) : global =
    match glob with
      | GFun (f, _) ->
          (* First rename the original XDR function.  The copy
             will be a wrapper.  The original will be called _orig, 
             and the wrapper will take on the same name as what
             the original used to be called.
          *)
          let new_varinfo : varinfo = makeGlobalVar f.svar.vname f.svar.vtype in
          let body_stmts = self#get_fn_body f in (* WRAPPER_CALL(blah) not WRAPPER_CALL(blah_orig) *)
          (*f.svar.vname <- f.svar.vname ^ "_orig"; *)
          let new_block : block = { 
            battrs = [];
            bstmts = body_stmts;
          } in
          let new_locals = [] in (* self#get_fn_locals f in *)
          let new_fundec : fundec = {
            svar = new_varinfo;
            sformals = f.sformals;
            slocals = new_locals;
            smaxid = 0;
            sbody = new_block;
            smaxstmtid = None;
            sallstmts = [];
          } in
          GFun (new_fundec, locUnknown);
      | _ ->
          Printf.fprintf stderr "Error in initial_map\n";
          glob;

  (* Finds the varinfo associated with the new function name *)
  method get_new_varinfo (name: string) : varinfo =
    let predicate = function x -> (x.vname (*^ "_orig"*) = name) in
    List.find predicate all_new_varinfos;

  (* Modifies the original (_orig) XDR functions to refer to the new function
     instead of the of the original.  This ensures that our wrappers are interposed
     between every recursive call.
  *)
  method update_call_itself
    (lval_option: lval option)
    (e1: exp)
    (e2_list : exp list)
    (loc: location)
    (i: instr) : instr =
    (* This is a pain because we have to modify the name of the function
       that's passed in as a parameter to another function *)
    match e1 with
      | Lval (lv) ->
          let (lh, off) = lv in
          begin
            match lh with
              | Var (vi) ->
                  begin
                    try
                      let old_varinfo = self#get_new_varinfo vi.vname in
                      let new_call = Call (lval_option,
                                           expify_lval (lvalify_varinfo old_varinfo),
                                           e2_list,
                                           loc) in
                      new_call;
                    with Not_found -> 
                      begin 
                        let new_params = List.map self#update_call_parameter e2_list in
                        let new_call = Call (lval_option, e1, new_params, loc) in
                        new_call;
                      end
                  end
              | _ -> i;
          end
      | _ -> i;

  method update_parameter_lval (lv: lval) : lval =
    let (lh, off) = lv in
    match lh with
      | Var (vi) ->
          begin
            try
              let old_varinfo = self#get_new_varinfo vi.vname in
              lvalify_varinfo old_varinfo;
            with Not_found ->
              lv;
          end
      | _ -> lv;
          
  method update_call_parameter (e: exp) : exp =
    match e with
      | Const (c) -> e;
      | Lval (lv) -> Lval (self#update_parameter_lval lv);
      | UnOp (u, e2, t) -> UnOp (u, self#update_call_parameter e2, t);
      | CastE (t, e2) -> CastE (t, self#update_call_parameter e2);
      | AddrOf (lv) -> mkAddrOf (self#update_parameter_lval lv);
      | _ -> e;
          
  method update_instrs (i: instr) : instr =
    match i with
      | Call (lval_option, e1, e2_list, loc) ->
          self#update_call_itself lval_option e1 e2_list loc i;
      | _ -> i;

  method update_stmts (s: stmt) : unit =
    match s.skind with
      | Instr (il) ->
          let new_list = List.map self#update_instrs il in
          s.skind <- Instr (new_list);
      | _ -> ();

  (* Code needed to modify the name of the function being passed as a parameter
     to the XDR routine.  This is extremely tedious because the word we need
     to change is deeply nested in the CIL data structure.  We need to substitute
     one function name for another.
  *)
  method update_references (glob: global) : unit =
    match glob with
      | GFun (f, _) ->
          List.iter self#update_stmts f.sallstmts;
      | _ ->
          Printf.fprintf stderr "Error in update_references\n";
          ();

  (* Given a global, returns the associated varinfo object *)
  method get_varinfo (glob: global) : varinfo =
    match glob with
      | GFun (f, _) ->
          f.svar;
      | _ ->
          Printf.fprintf stderr "Error in get_varinfo\n";
          makeVarinfo false "blah" (TVoid ([]));

  (* Visits every block.  Need to compute CFG information
     so we can get a convenient list of stmts that are present
     in the function.  This saves us from traversing every block
     later on.
  *)
  method vfunc (f: fundec) : fundec visitAction =
    begin
      (Cil.prepareCFG f);
      (Cil.computeCFGInfo f false); (* false = per-function stmt numbering,
                                       true = global stmt numbering *)
      DoChildren;
    end

  (* Called by do_extract_hdr *)
  method rewrite_xdr
    (f: file)
    (orig_h_filename: string)
    (orig_c_filename: string) : unit =
    begin
      (* Executes vfunc for all functions *)
      visitCilFileSameGlobals (self :> cilVisitor) f;

      (* TODO pass in a parameter so we can re-add the header file *)
      let header_file = GText ("#include \"" ^ orig_h_filename ^ "\"") in
      let header_file2 = GText ("#include \"xdr_structs.h\"") in
      let orig_c_file = GText ("#include \"" ^ orig_c_filename ^ "\"") in
      let initial_globs = List.filter self#initial_filter f.globals in
      let new_globs = List.map self#initial_map initial_globs in
      all_new_varinfos <- List.map self#get_varinfo new_globs;
      List.iter self#update_references initial_globs;
      (*let final_globals = [header_file; header_file2] @ initial_globs @ new_globs in*)
      let final_globals = [header_file; header_file2; orig_c_file] @ new_globs in
      f.globals <- final_globals;
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
let do_extract_xdr
    (f: file)
    (xdr_dowhat: string)
    (xdr_orig_h_filename : string)
    (xdr_orig_c_filename : string) : unit =
  begin
    if xdr_dowhat = "java" or xdr_dowhat = "c" then
      let obj : extract_xdr = new extract_xdr in
      obj#generate_xdr f xdr_dowhat
    else if xdr_dowhat = "rewrite-xdr" then
      let obj : rewrite_xdr = new rewrite_xdr in
      obj#rewrite_xdr f xdr_orig_h_filename xdr_orig_c_filename;
  end
