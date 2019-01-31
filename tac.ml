open Libcommon
open Astcommon

module PSC = Passcommon
module ARG = Commandline
module TYP = Types
module AST = Irtree

let is_done = ref false

(*** (key, (scope * statement object)) *)
let tmp_tbl:((string, (string * AST.irstatement)) Hashtbl.t) = Hashtbl.create 0
let id_name = ref 0

let build_tac stm scope  =
  let loc = TYP.Location (scope, -1) in
  let new_id_name id = 
    id := !id + 1;
    string_of_int (!id-1)
  in
  let get_tmp_name exp =
    let expression = (AST.find_tbl AST.expression_tbl exp) in
    match expression#get_type with
    | TYP.Constant _ | TYP.Variable _ | TYP.Index _ -> 
      exp
    | _ -> 
      let id = new_id idx in
      let tmp_name = Libcommon.cohd_tmp ^ (new_id_name id_name) in
      PSC.add_variable tmp_name exp scope false;
      AST.add_expression id (new AST.irexpression (TYP.Variable (tmp_name), loc));
      id
  in
  let rec exp_to_tac exp tmp_name =
    let seq_id = new_id idx in 
    let expression = (AST.find_tbl AST.expression_tbl exp) in

    begin match expression#get_type with
    | TYP.Exp e ->  
      Hashtbl.add tmp_tbl seq_id (scope, (new AST.irstatement(TYP.Stat (exp_to_tac e tmp_name), loc)))


    | TYP.Binary (("=",b), exp1, exp2) ->
(*      print_endline ("bin(=):" ^ expression#to_string ()); *)
    begin
      match (AST.find_tbl AST.expression_tbl exp2)#get_type with 
      | TYP.Variable _ 
      | TYP.Constant _ 
      | TYP.Index _ ->

        let stm = new_id idx in
        Hashtbl.add tmp_tbl stm (scope, (new AST.irstatement(TYP.Computation (exp), loc)));
        Hashtbl.add tmp_tbl seq_id (scope, (new AST.irstatement(TYP.Stat (stm), loc)))

      | _ ->  
        Hashtbl.add tmp_tbl seq_id (scope, (new AST.irstatement(TYP.Stat (exp_to_tac exp2 exp1), loc)))


    end
    | TYP.Binary ((op,b), exp1, exp2) ->

(* print_endline ("bin(_):" ^ expression#to_string ()); *)
(*print_endline ("tmp_name : " ^ tmp_name);*)

      let alpha = get_tmp_name exp1 in
      let beta  = get_tmp_name exp2 in
      let tmp_name_cp = (AST.find_tbl AST.expression_tbl tmp_name)#get_copy () in 

      let exp_id = new_id idx in
      let exp2_id = new_id idx in
      AST.add_expression exp2_id (new AST.irexpression (TYP.Binary ((op,b), alpha, beta), loc));
      AST.add_expression exp_id (new AST.irexpression (TYP.Binary (("=",false), tmp_name_cp, exp2_id), loc));


(*print_endline ((AST.find_tbl AST.expression_tbl exp2_id)#to_string ());*)

      let stm4 = new_id idx in Hashtbl.add tmp_tbl stm4 (scope, (new AST.irstatement(TYP.Computation (exp_id), loc)));
      let stm3 = exp_to_tac exp2 beta in

      let stm2 = new_id idx in  
      begin
      let (_, stm_check) = (Hashtbl.find tmp_tbl stm3) in
      match stm_check#get_stmt_type with
	| TYP.Nop ->  Hashtbl.add tmp_tbl stm2 (scope, (new AST.irstatement(TYP.Stat (stm4), loc)))
	| _ -> Hashtbl.add tmp_tbl stm2 (scope, (new AST.irstatement(TYP.Sequence (stm3, stm4), loc)))
      end;


      let stm1 = exp_to_tac exp1 alpha in
      begin
      let (_, stm_check) = (Hashtbl.find tmp_tbl stm1) in
      match stm_check#get_stmt_type with
	| TYP.Nop ->  Hashtbl.add tmp_tbl seq_id (scope, (new AST.irstatement(TYP.Stat (stm2), loc)))
	| _ -> Hashtbl.add tmp_tbl seq_id (scope, (new AST.irstatement(TYP.Sequence (stm1, stm2), loc)))
      end

    | TYP.Call (fn, args) ->

(*      print_endline ("cll:" ^ expression#to_string ());*)

      let tmp_name_cp = ref "" in
      if (tmp_name <> "") then
	begin 
(* print_endline (tmp_name); *)
        tmp_name_cp := (AST.find_tbl AST.expression_tbl tmp_name)#get_copy ()
      end;

      let name_list = ref [] in
      let stm_list = ref [] in
      List.iter (fun i ->
        (* print_endline (" fn_arg:" ^ (AST.find_tbl AST.expression_tbl i)#to_string ()); *)
 
        name_list := !name_list @ [(get_tmp_name i)];
      ) args;

      List.iter2 (fun i j -> 
        stm_list := !stm_list @ [(exp_to_tac i j)]
      ) args !name_list;


      let exp_id = new_id idx in 
        AST.add_expression exp_id (new AST.irexpression (TYP.Call (fn, !name_list), loc));
      let stm_comp = new_id idx in

      if(!tmp_name_cp <> "") then 
	begin
          let expb_id = new_id idx in 
          AST.add_expression expb_id (new AST.irexpression (TYP.Binary (binary_assign, !tmp_name_cp, exp_id), loc));
          Hashtbl.add tmp_tbl stm_comp (scope, (new AST.irstatement(TYP.Computation (expb_id), loc)));
	end
      else
	begin
          Hashtbl.add tmp_tbl stm_comp (scope, (new AST.irstatement(TYP.Computation (exp_id), loc)));
	end;


      stm_list := !stm_list @ [stm_comp];

      let rec build_stmt_seq id stml =
        match stml with
	| [] -> 
          Hashtbl.add tmp_tbl id (scope, (new AST.irstatement(TYP.Nop, loc)))
	| h::t::[] ->
	  begin
            let (_, stm_check) = (Hashtbl.find tmp_tbl h) in
            match stm_check#get_stmt_type with
	    | TYP.Nop ->  Hashtbl.add tmp_tbl id (scope, (new AST.irstatement(TYP.Stat (t), loc)))
	    | _ -> Hashtbl.add tmp_tbl id (scope, (new AST.irstatement(TYP.Sequence (h, t), loc)))
          end
	| h::t -> 
          let idr = new_id idx in
	  build_stmt_seq idr t;
         (* Hashtbl.add tmp_tbl id (scope, (new AST.irstatement(TYP.Sequence (h, idr), loc)))*)

          begin
            let (_, stm_check) = (Hashtbl.find tmp_tbl h) in
            match stm_check#get_stmt_type with
	    | TYP.Nop ->  Hashtbl.add tmp_tbl id (scope, (new AST.irstatement(TYP.Stat (idr), loc)))
 	    | _ -> Hashtbl.add tmp_tbl id (scope, (new AST.irstatement(TYP.Sequence (h, idr), loc)))
          end
        in 
        build_stmt_seq seq_id !stm_list


    | TYP.SizeofType _ 
    | TYP.SizeofExpr _ ->
(* print_endline ("szf:" ^ expression#to_string ()); *)

      let tmp_name_cp = (AST.find_tbl AST.expression_tbl tmp_name)#get_copy () in
      let expb_id = new_id idx in 
      AST.add_expression expb_id (new AST.irexpression (TYP.Binary (binary_assign, tmp_name_cp, exp), loc));
      let stm = new_id idx in
      Hashtbl.add tmp_tbl stm (scope, (new AST.irstatement(TYP.Computation (expb_id), loc)));
      Hashtbl.add tmp_tbl seq_id (scope, (new AST.irstatement(TYP.Stat (stm), loc)))



    | _ ->
(*print_endline ("_:" ^ expression#to_string ()); *)
      Hashtbl.add tmp_tbl seq_id (scope, (new AST.irstatement(TYP.Nop, loc)))

    end;
    seq_id
  in
  match stm#get_stmt_type with
  | TYP.Computation e -> new AST.irstatement(TYP.Stat (exp_to_tac e ""), loc)
  | TYP.Return e      -> 
    let alpha = get_tmp_name e in
    let stm3_id = new_id idx in
      Hashtbl.add tmp_tbl stm3_id (scope, (new AST.irstatement(TYP.Return (alpha), loc)));
    let stm2_id = exp_to_tac e alpha in
    let stm1_id = new_id idx in
      Hashtbl.add tmp_tbl stm1_id (scope, (new AST.irstatement(TYP.Sequence (stm2_id, stm3_id), loc)));
    new AST.irstatement(TYP.Stat (stm1_id), loc)

  | _                 -> failwith "Error: not supposed to be here !\n"






(* PASS MAIN *)
let launch_pass file =
  if not !is_done then begin
    is_done := true;

  if !ARG.verbose then print_string ":: Three Address Code Pass\n";
  PSC.clear_tbl ();

  (* build a table with variables and their living scope *)
  PSC.buildVariableHashTable    file#get_file_dcl "global";
  (* build a table with statements and their living scope 
     build a table with function bodies *)
  PSC.buildFPStatementHashTable file#get_file_dcl;

  (* transform each selected statement in three address code *)
  Hashtbl.iter (
    fun key (scope, stm) -> 
    let new_stm = build_tac stm scope in
    Hashtbl.add tmp_tbl key (scope, new_stm)
  ) PSC.statement_tbl;

  Hashtbl.iter (
    fun key (scope, stm) ->
    Hashtbl.replace PSC.statement_tbl key (scope, stm)
  ) tmp_tbl;

  (* update the statement table of irtree with new statements *)
  Hashtbl.iter (
    fun key (_, stm) ->
    Hashtbl.replace AST.statement_tbl key stm
  ) PSC.statement_tbl;

  end









(**** DEBUG ONLY *)

(* Hashtbl.iter ( *)
(*   fun key (scope, var) -> *)
(*   print_string (key ^ " - " ^ scope ^ " - " ^ var#to_string () ^ "\n"); *)


(* ) variable_tbl; *)


(* Hashtbl.iter ( *)
(*   fun key (scope, stm) -> *)
(*   match stm#get_stmt_type with *)
(*   | TYP.Computation _ -> print_string (key ^ " - " ^ scope ^ " - " ^ stm#to_string () ^ "\n"); *)
(*   | _ -> () *)

(* ) statement_tbl *)
