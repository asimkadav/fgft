  (** stubgen_funs: Stub generation *)
  method stubgen_funs (f: fundec) : stmt list =
    begin
      (* 0. See if this function can be handled *)
      let (ret_var_type,_,isvararg,_) = (splitFunctionType f.svar.vtype) in
      if (isvararg = true)
      then (fatal [f.svar.vname; "VARARGS not supported in stubgen_funs for fn:" ^ f.svar.vname]);

      (* 1. Generate the marshaling code at the call site *)
      let marshaling_code = self#call_m_dm_in_stub f [] true in

      (* 2. Create a new variable of the appropriate type and add it to f's slocals *)
      (* Also create the "extra" varinfo list for demarshaling *)
      let demarshlist = ref [] in
      
      let retvar_init_stmts = ref [] in
      let ret_var =
        (match ret_var_type with
           | TVoid(_) -> None;
           | _ ->
               let ret_var_varinfo = (makeTempVar f ~name:("retval") ret_var_type) in
               (* Initialize any new pointer variables to NULL *)
               (* Since this is stub generator, also set ints to 0, so 
                  the subsequent disp_kern_allowed code works *)
               if (isPointerType ret_var_varinfo.vtype) ||
                 (isIntegralType ret_var_varinfo.vtype) then
                   begin
                     let retvar_init = Set((lvalify_varinfo ret_var_varinfo), zero64Uexp, locUnknown) in
                     let stmt_retvar_init = (mkStmt (Instr [retvar_init])) in
                     retvar_init_stmts := [stmt_retvar_init];
                   end;
               demarshlist := (List.append !demarshlist [ret_var_varinfo]);
               Some(Var(ret_var_varinfo),NoOffset)
        )
      in
      (* 3. Create the return statement *)
      let ret_stub =
        (match ret_var with
           | Some(ret_lval) -> Return(Some(expify_lval ret_lval), locUnknown);
           | None -> Return(None, locUnknown);
        ) in
      let stmt_ret = (mkStmt ret_stub) in
      let disp_kern_allowed = 
        if gen_kern = false then
          []
            (*
          gen_disp_kern_allowed ret_var_type ret_var stmt_ret f
            *)
        else
          []
      in

      (* Make sure all formal parameters have names.  Sometimes,
         the function prototype is provided without variable names--
         in these cases, we make some up. *)
      ensure_formals_have_names f;

      (* <The following code uses unlock_user_thread or disp_kern to
         call a function in the user and is the function that must be
         used with two address spaces> *)
      let switch_func = (if gen_kern then (get_unblock_user_thread_fundec()) else (get_disp_kern_fundec ())) in
      let switch_func_expr = (expify_fundec switch_func) in
      (* Create a new local variable rqarg of type req_args *)
      let rqarg_typ = struct_reqargs_typ in
      let rqarg_varinfo = (get_local_variable f rqarg_typ "rqarg") in
      (* Get the ID of the function to be called, and assign it to rqarg *)
      let tgt_funcid = get_function_id (strip_stub_prefix f.svar.vname) in
      let tgt_funcid_exp = (integer tgt_funcid) in
      let stmt_setrqargfuncid = (mkStmtOneInstr
                                   (Set ((Var(rqarg_varinfo), Field(reqargs_funcid, NoOffset)),
                                         tgt_funcid_exp, locUnknown))) in
      (* Get the buffer that stores the data *)
      let curr_marshbuf_varinfo = ref dummyFunDec.svar in
      (match curr_marshbuf with
         | Some(marshbuf) -> curr_marshbuf_varinfo := marshbuf;
         | None -> (fatal ["no marshbuf found"]);
      );
      let curr_marshbuf_lval = (lvalify_varinfo !curr_marshbuf_varinfo) in
      let curr_marshbuf_exp = (expify_lval curr_marshbuf_lval) in
      let stmt_setrqargdata = (mkStmtOneInstr
                                 (Set ((Var(rqarg_varinfo), Field(reqargs_data, NoOffset)),
                                       curr_marshbuf_exp, locUnknown))) in
      (* Get the length of the buffer *)
      let curr_marshoff_varinfo = ref dummyFunDec.svar in
      (match curr_marshoff with
         | Some(marshoff) -> curr_marshoff_varinfo := marshoff;
         | None -> (fatal ["no marshoff found"]);
      );
      let curr_marshoff_lval = (lvalify_varinfo !curr_marshoff_varinfo) in
      let curr_marshoff_exp = (expify_lval curr_marshoff_lval) in
      let stmt_setrqarglength = (mkStmtOneInstr
                                   (Set ((Var(rqarg_varinfo), Field(reqargs_length, NoOffset)),
                                         curr_marshoff_exp, locUnknown))) in

      (* Create a call to unblock_user_thread/disp_kern *)
      let rqarg_lval = (lvalify_varinfo rqarg_varinfo) in
      let rqarg_exp = (mkAddrOf rqarg_lval) in
      let argslist = [rqarg_exp] in
      let call_switch_func = Call(None, switch_func_expr, argslist, locUnknown) in
      let stmt_call_switch_func = (mkStmt (Instr [call_switch_func])) in
      (* Fetch the value of rqarg.data into the marshaling buffer *)
      let rqargdata_lval = (Var(rqarg_varinfo), Field(reqargs_data, NoOffset)) in
      let stmt_getrqargdata = (mkStmtOneInstr
                                 (Set (curr_marshbuf_lval, (expify_lval rqargdata_lval), locUnknown))) in
      let stmts_switch =
        [stmt_setrqargfuncid; stmt_setrqargdata;
         stmt_setrqarglength; stmt_call_switch_func; stmt_getrqargdata] in

      (* 4. Insert the demarshaling code after the return *)
      let demarshaling_code = (self#call_m_dm_in_stub f !demarshlist false) in
      (* 5. Create the code to free the marshaling buffer *)
      let curr_marshbuf_varinfo = ref dummyFunDec.svar in
      (match curr_marshbuf with
         | Some(demarshbuf) -> curr_marshbuf_varinfo := demarshbuf;
         | None -> (fatal ["no marshbuf found"]);
      );
      (* let free_after_marsh = (gen_marshbuf_free "MARSHBUF_FREE" !curr_marshbuf_varinfo) in *)
      let free_after_demarsh = (gen_marshbuf_free "DEMARSHBUF_FREE" !curr_marshbuf_varinfo) in
      (* 6. Create the list of statements to return *)
      let ret_stmts = ref [] in
      if gen_kern then begin
        let down_rec_lock_stmts = generate_sync_call "ACQUIRE_REC_LOCK" in
        ret_stmts := List.append !ret_stmts down_rec_lock_stmts;
      end;
      ret_stmts := (List.append !ret_stmts !retvar_init_stmts);
      ret_stmts := (List.append !ret_stmts disp_kern_allowed);
      ret_stmts := (List.append !ret_stmts marshaling_code);
      ret_stmts := (List.append !ret_stmts stmts_switch);
      (* XXX   ret_stmts := (List.append !ret_stmts [free_after_marsh]); *)
      ret_stmts := (List.append !ret_stmts demarshaling_code);
      ret_stmts := (List.append !ret_stmts [free_after_demarsh]);
      if gen_kern then begin
        let up_marshsem_stmts = generate_sync_call "UP_MARSH_SEM" in
        ret_stmts := List.append !ret_stmts up_marshsem_stmts;
        let up_rec_lock_stmts = generate_sync_call "RELEASE_REC_LOCK" in
        ret_stmts := List.append !ret_stmts up_rec_lock_stmts;
      end;
      ret_stmts := (List.append !ret_stmts [stmt_ret]);
      !ret_stmts;
    end
