(******************************************************************************)
(* History *********************************************************************

- September 25 2013 - revision 1.0 : first revision


******************************************************************** /History *)
(******************************************************************************)

(** High-precision pass. This file contains the building processus of
    high-precision operators. *)

open Libcommon
open Passcommon
open Astcommon
open Highop

module ARG = Commandline
module AST = Irtree
module TYP = Types

(******************************************************************************)



(*!beg!*)

(*** (key, (scope * statement object)) *)
let tmp_tbl:((string, (string * AST.irstatement)) Hashtbl.t) = Hashtbl.create 0
(*** (scope, variable name) *)
let assigned_tbl:((string, string list) Hashtbl.t) = Hashtbl.create 0


let min_bound = ref 0  (* créer une liste d'intervalles à la place pour ne pas transformer le code dupliqué lors de la copie de la boucle pour les mesures de perfs *)
let max_bound = ref 0
let loop_nop_id = ref ""

let set_inter inter =
  match inter with
  | (min, max) ->
    min_bound := min;
    max_bound := max 

let set_loop_pos pos = loop_nop_id := pos

let in_inter exp =
  let id = int_of_string exp in
  (id >= !min_bound) && (id <= !max_bound)

(*!end!*)



(** Deterine if an expression is assigned by reading in the assigned table.
    @param exp expression identifier 
    @param scope function scope of the expression *)
let rec is_assigned exp scope =
  let expr = (AST.find_tbl AST.expression_tbl exp) in
  match expr#get_type with
  | TYP.Constant _  -> false
  | TYP.Variable v  -> List.mem v (try Hashtbl.find assigned_tbl scope with | Not_found -> failwith ("is_assigned : not found : " ^ scope))
  | TYP.Index (t,_) -> is_assigned t scope
  | _               -> failwith "highprec.ml -> is_assigned fail" 


let apply_operator exp op e1 se1 se2 scope loc stm_hd_id seq_id =
  begin match op with
  | "+" -> Hashtbl.add tmp_tbl stm_hd_id (scope, new AST.irstatement ( TYP.Stat
    begin match ((is_assigned se1 scope), (is_assigned se2 scope)) with
    | (true, true)  -> (Highop.sum_hw_hw exp e1 se1 se2 scope loc)
    | (false,false) -> (Highop.sum_w_w   exp e1 se1 se2 scope loc)
    | (false,true)  -> (Highop.sum_hw_w  exp e1 se2 se1 scope loc)
    | (true,false)  -> (Highop.sum_hw_w  exp e1 se1 se2 scope loc)     
    end, loc))

    | "-" -> 
    let neg_id = new_id idx in 
    AST.add_expression neg_id (new AST.irexpression (TYP.Unary (("-",false), se2), loc));
    Hashtbl.add tmp_tbl stm_hd_id (scope, new AST.irstatement ( TYP.Stat
    begin match ((is_assigned se1 scope), (is_assigned se2 scope)) with
    | (true, true)  -> (Highop.sum_hw_hw exp e1 se1 neg_id scope loc)
    | (false,false) -> (Highop.sum_w_w   exp e1 se1 neg_id scope loc)
    | (false,true)  -> (Highop.sum_hw_w  exp e1 neg_id se1 scope loc)
    | (true,false)  -> (Highop.sum_hw_w  exp e1 se1 neg_id scope loc)     
    end, loc))

    | "*" -> 
    Hashtbl.add tmp_tbl stm_hd_id (scope, new AST.irstatement ( TYP.Stat
    begin match ((is_assigned se1 scope), (is_assigned se2 scope)) with
    | (true, true)  -> (Highop.product_hw_hw exp e1 se1 se2 scope loc)
    | (false,false) -> (Highop.product_w_w   exp e1 se1 se2 scope loc)
    | (false,true)  -> (Highop.product_hw_w  exp e1 se2 se1 scope loc)
    | (true,false)  -> (Highop.product_hw_w  exp e1 se1 se2 scope loc)     
     end, loc))

    | _ -> Hashtbl.add tmp_tbl stm_hd_id (scope, (new AST.irstatement(TYP.Nop, loc)))
    end;
    Hashtbl.add tmp_tbl seq_id (scope, (new AST.irstatement(TYP.Stat (stm_hd_id), loc)))


let apply_operator_nop exp op e1 se1 se2 scope loc stm_hd_id seq_id =
  begin match op with 
  | "+" -> Hashtbl.add tmp_tbl stm_hd_id (scope, new AST.irstatement ( TYP.Stat
    begin match ((is_assigned se1 scope), (is_assigned se2 scope)) with
    | (true, true)  -> (Highop.sum_hw_hw_nop exp e1 se1 se2 scope loc)
    | (false,false) -> (Highop.sum_w_w_nop   exp e1 se1 se2 scope loc)
    | (false,true)  -> (Highop.sum_hw_w_nop  exp e1 se2 se1 scope loc)
    | (true,false)  -> (Highop.sum_hw_w_nop  exp e1 se1 se2 scope loc)     
    end, loc))

    | "-" -> 
    let neg_id = new_id idx in 
    AST.add_expression neg_id (new AST.irexpression (TYP.Unary (("-",false), se2), loc));
    Hashtbl.add tmp_tbl stm_hd_id (scope, new AST.irstatement ( TYP.Stat
    begin match ((is_assigned se1 scope), (is_assigned se2 scope)) with
    | (true, true)  -> (Highop.sum_hw_hw_nop exp e1 se1 neg_id scope loc)
    | (false,false) -> (Highop.sum_w_w_nop   exp e1 se1 neg_id scope loc)
    | (false,true)  -> (Highop.sum_hw_w_nop  exp e1 neg_id se1 scope loc)
    | (true,false)  -> (Highop.sum_hw_w_nop  exp e1 se1 neg_id scope loc)     
    end, loc))

    | "*" -> 
    Hashtbl.add tmp_tbl stm_hd_id (scope, new AST.irstatement ( TYP.Stat
    begin match ((is_assigned se1 scope), (is_assigned se2 scope)) with
    | (true, true)  -> (Highop.product_hw_hw_nop exp e1 se1 se2 scope loc)
    | (false,false) -> (Highop.product_w_w_nop   exp e1 se1 se2 scope loc)
    | (false,true)  -> (Highop.product_hw_w_nop  exp e1 se2 se1 scope loc)
    | (true,false)  -> (Highop.product_hw_w_nop  exp e1 se1 se2 scope loc)     
     end, loc))

    | _ -> Hashtbl.add tmp_tbl stm_hd_id (scope, (new AST.irstatement(TYP.Nop, loc)))
    end;
    Hashtbl.add tmp_tbl seq_id (scope, (new AST.irstatement(TYP.Stat (stm_hd_id), loc)))



(** Build a new statement with high-precision operators.
    @param stm id of the statement to transform
    @param scope function scope of the statement *)
let build_hp stm scope  =
  let loc = TYP.Location (scope, -1) in      (* location of the new statement *)
          (* note : location is in (file, line), but here in (scope, line)... *)

  let rec low_exp exp =                           (* return a _low expression *) 
    let exp_id = new_id idx in
    let expr = (AST.find_tbl AST.expression_tbl exp) in
    begin match expr#get_type with
    | TYP.Variable v  -> AST.add_expression exp_id 
                          (new AST.irexpression (TYP.Variable (v^"_low"), loc));
                         add_variable (v^"_low") exp scope true
    | TYP.Index (t,i) -> AST.add_expression exp_id 
                         (new AST.irexpression (TYP.Index ((low_exp t),i), loc))
    | _               -> failwith "Pattern matching missing here 3 !" 
    end;
    exp_id
  in

  let exp_to_stm_hd exp =
    if in_inter exp then Highop.set_nop ();
    let stm_hd_id = new_id idx in
    let expression = (AST.find_tbl AST.expression_tbl exp) in
    let seq_id = new_id idx in
    let stm_exp_id = new_id idx in
    Hashtbl.add tmp_tbl stm_exp_id 
                      (scope, (new AST.irstatement(TYP.Computation (exp), loc)));

    begin match expression#get_type with
    | TYP.Binary (op, e1, e2) ->                 (* exp is a binary expression *)
      let e1_name = (AST.find_tbl AST.expression_tbl e1)#to_string () in      
      let expression2 = (AST.find_tbl AST.expression_tbl e2) in
 
      begin match expression2#get_type with
      | TYP.Constant _ ->                                            (* x op C *)
        let exp_id = new_expr (new AST.irexpression 
            (TYP.Binary (op, (low_exp e1), (float_cnst_expr "0.0" loc)), loc)) in
        Hashtbl.add tmp_tbl stm_hd_id 
                    (scope, (new AST.irstatement(TYP.Computation (exp_id), loc)));
        Hashtbl.add tmp_tbl seq_id 
         (scope, (new AST.irstatement(TYP.Sequence (stm_exp_id, stm_hd_id), loc)))

      | TYP.Variable _ | TYP.Index _ ->                           (* x op x<[]> *)
        let exp_id = new_expr (new AST.irexpression (TYP.Binary (op, (low_exp e1), 
                                           if is_assigned e2 scope then low_exp e2 
                                       else (float_cnst_expr "0.0" loc)), loc)) in
        Hashtbl.add tmp_tbl stm_hd_id 
                    (scope, (new AST.irstatement(TYP.Computation (exp_id), loc)));
        Hashtbl.add tmp_tbl seq_id 
         (scope, (new AST.irstatement(TYP.Sequence (stm_exp_id, stm_hd_id), loc)))

      | TYP.Binary ((op,b), se1, se2) ->                       (* x op (y op z) *)

        if !ARG.to_prec then
        begin
          if List.mem e1_name !ARG.names then
          begin
            apply_operator exp op e1 se1 se2 scope loc stm_hd_id seq_id
          end
          else
          begin
            (* Hashtbl.add tmp_tbl stm_hd_id  *)
            (*                         (scope, (new AST.irstatement(TYP.Nop, loc))); *)
            (* Hashtbl.add tmp_tbl seq_id (scope, ( *)
            (*      new AST.irstatement(TYP.Sequence (stm_exp_id, stm_hd_id), loc))) *)
            apply_operator_nop exp op e1 se1 se2 scope loc stm_hd_id seq_id
          end
        end
        else
            apply_operator exp op e1 se1 se2 scope loc stm_hd_id seq_id

      | _ -> Hashtbl.add tmp_tbl stm_hd_id 
                                    (scope, (new AST.irstatement(TYP.Nop, loc)));
             Hashtbl.add tmp_tbl seq_id (scope, 
                (new AST.irstatement(TYP.Sequence (stm_exp_id, stm_hd_id), loc)))
      end
      

    | TYP.Call (fn, args) ->
      let new_args = ref [] in
      List.iter (
        fun i ->
          if is_assigned i scope then 
          begin
            let arg_id = new_id idx in 
            AST.add_expression arg_id (new AST.irexpression (TYP.Binary (("+",true), i, (low_exp i)), loc));
            new_args := !new_args @ [(arg_id)]
	  end 
	  else
          begin
            new_args := !new_args @ [(i)] 
	  end
      ) args;
      let exp_id = new_id idx in AST.add_expression exp_id (new AST.irexpression (TYP.Call (fn, !new_args), loc));
      Hashtbl.add tmp_tbl seq_id (scope, (new AST.irstatement(TYP.Computation (exp_id), loc)));

    | _ -> Hashtbl.add tmp_tbl seq_id (scope, (new AST.irstatement(TYP.Nop, loc)))
    end;
    Highop.clear_nop ();
    seq_id
  in
  match stm#get_stmt_type with
  | TYP.Computation e -> 

    let default exp = new AST.irstatement(TYP.Stat (exp_to_stm_hd exp), loc) in
    if in_inter e && not !ARG.to_first && !ARG.to_block then
      stm
    else 
      begin
        if not (in_inter e) then begin
        let exp = AST.find_tbl AST.expression_tbl e in 
       
        match exp#get_type with 
        | TYP.Binary (op1, dst, srcs) -> 
        begin
          match (AST.find_tbl AST.expression_tbl srcs)#get_type with 
          | TYP.Binary (op2, src1, src2) ->
            let dn_name = get_var_name dst in
	    let s1_name = get_var_name src1 in
	    let s2_name = get_var_name src2 in
	    let var_tmp = Libcommon.cohd_tmp ^ "_tmp" in
            let tmp_exp1 = new_expr (new AST.irexpression (TYP.Variable (var_tmp^"1"), loc)) in
	    let tmp_exp2 = new_expr (new AST.irexpression (TYP.Variable (var_tmp^"2"), loc)) in
	    let exp_l = ref [] in
	    let tsrc1 = ref false in
	    let tsrc2 = ref false in
            if dn_name = s1_name then 
	      begin 
                add_variable (var_tmp^"1") e scope false;
            
		Hashtbl.replace assigned_tbl scope ((Hashtbl.find assigned_tbl scope) @ [(var_tmp^"1")]);
		(*Hashtbl.replace assigned_tbl scope ([(var_tmp^"1")]);MOD FEV *)

                (* if (not !CMD.double_double) then Hashtbl.replace assigned_tbl scope ([(dn_name)]); *)
                let add_exp = new_expr (new AST.irexpression (TYP.Binary (binary_assign, tmp_exp1, src1), loc)) in
                let var_low = Passcommon.low_exp tmp_exp1 "_low" scope false in
                let src_low = Passcommon.low_exp src1 "_low" scope true in (*MOD FEV*)
                let add_exp_low = new_expr (new AST.irexpression (TYP.Binary (binary_assign, var_low, src_low), loc)) in 
                exp_l := !exp_l @ [add_exp; add_exp_low];
                tsrc1 := true; 
	      end;
            if dn_name = s2_name then 
	      begin 
                add_variable (var_tmp^"2") e scope false;
                let add_exp = new_expr (new AST.irexpression (TYP.Binary (binary_assign, tmp_exp2, src2), loc)) in
                let var_low = low_exp add_exp in
                let add_exp_low = new_expr (new AST.irexpression (TYP.Binary (binary_assign, var_low, src1), loc)) in
                exp_l := !exp_l @ [add_exp; add_exp_low];
                tsrc2 := true;
	      end;
	    begin 
              match !tsrc1, !tsrc2 with
	      | false, false -> default e
	      | true, false -> 
                let stmt_comp1 = new_stmt (new AST.irstatement(TYP.Computation (List.nth !exp_l 0), loc)) in
                let stmt_comp2 = new_stmt (new AST.irstatement(TYP.Computation (List.nth !exp_l 1), loc)) in
		let expr1 = new_expr (new AST.irexpression (TYP.Binary (op2, tmp_exp1, src2), loc)) in 

		Astcommon.replace_expr_by_expr e (new AST.irexpression (TYP.Binary (op1, dst, expr1), loc));

                let stmt_seq1 = new_stmt (new AST.irstatement(TYP.Sequence (stmt_comp2, exp_to_stm_hd e), loc)) in 
                new AST.irstatement(TYP.Sequence (stmt_comp1, stmt_seq1), loc)
	      | false, true -> failwith "a compléter"
	      | true, true -> failwith "a compléter"
	    end
	  | _ -> default e
	end 
        | _ -> default e
        end else default e
      end
  | TYP.Return      e -> 
    let ret_id = new_id idx in
    begin match (AST.find_tbl AST.expression_tbl e)#get_type with
    | TYP.Variable v ->  
      AST.add_expression ret_id (new AST.irexpression (TYP.Binary (("+",true), e, (low_exp e)), loc));
    | _ -> failwith "Pattern matching missing here 5 !"
    end;
    new AST.irstatement(TYP.Return (ret_id), loc)

  | _                 -> failwith "Pattern matching missing here 4 !"






(** Build the assigned variable hash table 
    @param tbl Statement hash table *)
let buildAssignedVariableHashTable tbl =
  let add_in_tbl var exp scope =
    if Hashtbl.mem assigned_tbl scope then    
      begin
	let var_list =
	  try
	    Hashtbl.find assigned_tbl scope
	  with 
	    | Not_found -> failwith "err98\n"
	in
      if List.mem var var_list then () else (
      (* add_variable (var^"_low") exp scope; *)
      Hashtbl.replace assigned_tbl scope (var_list @ [(var)]))
    end 
    else 
      begin
      (* add_variable (var^"_low") exp scope; *)
      Hashtbl.add assigned_tbl scope [(var)]
    end
  in
  let rec add_var exp scope =
    match (AST.find_tbl AST.expression_tbl exp)#get_type with
    | TYP.Variable v  -> add_in_tbl v exp scope
    | TYP.Index (t,_) -> add_var t scope
    | _ ->  failwith ("not supposed to be here 1 : "^(AST.find_tbl AST.expression_tbl exp)#to_string ())
  in
  Hashtbl.iter (
    fun key (scope, stm) ->
      match stm#get_stmt_type with
      | TYP.Computation exp -> 
      begin
        match (AST.find_tbl AST.expression_tbl exp)#get_type with 
	| TYP.Binary (_, e, _) -> add_var e scope
	| _ -> ()
      end
      | TYP.Return exp -> add_var exp scope
      | _ -> failwith "case not pris en compte"
  ) tbl

(** Launch the pass 
    @param file AST file representation object *)
let launch_pass file = 
  if !ARG.verbose then print_string ":: High Precision Pass\n";

  Passcommon.clear_tbl ();

                       (* build a table with variables and their living scope *)
  Passcommon.buildVariableHashTable file#get_file_dcl "global";

       (* build a table with floating-point statements and their living scope *)
                                        (* build a table with function bodies *)
  Passcommon.buildFPStatementHashTable file#get_file_dcl;

                                        (* build a table of assigned variable *)
  buildAssignedVariableHashTable statement_tbl;

  Highop.loop_pos := !loop_nop_id;              (* give nop loop id to highop *)


             (* transform each selected statement in high-precision statement *)
  Hashtbl.iter (
    fun key (scope, stm) -> 
      let new_stm = build_hp stm scope in          (* build the new statement *)
      Hashtbl.add tmp_tbl key (scope, new_stm) (* save it in a tmp hash table *)
  ) Passcommon.statement_tbl;


              (* update the AST statement table of irtree with new statements *)
  Hashtbl.iter (
    fun key (_, stm) ->
      Hashtbl.replace AST.statement_tbl key stm
  ) tmp_tbl;
