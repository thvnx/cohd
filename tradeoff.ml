open Libcommon
open Astcommon

module ARG = Commandline
module PSC = Passcommon
module AST = Irtree
module TYP = Types
module PP = Preprocess



(** list of pragma in C source file (line number * pragma declaration) **)
let (pragma_list:(int * string) list ref) = ref []

(** Hashtbl of for loops statements **)
(*** (key, (scope * statement object)) *)
let for_tbl:((string, (string * AST.irstatement)) Hashtbl.t) = Hashtbl.create 0

let loop_nop_id = ref ""


(** Build the for statement hash table
 statement must be a For statement 
 @param dcl an irdefinition object list *)
let buildForStatementHashTable (dcl:AST.irdefinition list) =
  let rec add_stmt stm scope =
  let stmt = (AST.find_tbl AST.statement_tbl stm) in
  match stmt#get_stmt_type with
  | TYP.Sequence (s1, s2) | TYP.If (_, s1, s2) -> add_stmt s1 scope; add_stmt s2 scope
  | TYP.While (_, s) | TYP.DoWhile (_, s) | TYP.Switch (_, s)
    | TYP.Case (_, s) | TYP.Default s | TYP.Label (_, s) | TYP.Stat s -> add_stmt s scope
  | TYP.Block b -> 
    let body = AST.find_tbl AST.body_tbl b in
    add_stmt body#get_body_stm scope
  | TYP.For (_,_,_,s) ->
    Hashtbl.add for_tbl stm (scope, stmt);
    add_stmt s scope
  | _ -> ()
  in
  List.iter (fun i -> 
  match i#get_definition with 
  | TYP.Function    id -> 
    let func = AST.find_tbl AST.function_tbl id in
    let body = AST.find_tbl AST.body_tbl (func#get_body) in
    add_stmt body#get_body_stm func#get_name
  | _ -> ()
  ) dcl


let add_variables vars scope loc =
    let typ_id = new_id idx in
      AST.add_type typ_id (new AST.irtype (TYP.Int ("",""), loc));
    let vars_id = ref [] in
    List.iter (fun i ->
      let var = new_id idx in
        AST.add_variable var (new AST.irvariable(i, typ_id, nop_expr));
      vars_id := !vars_id @ [var]
    ) vars;
    let nmg_id = new_id idx in  
      AST.add_namegroup nmg_id (new AST.irnamegroup(typ_id, "", !vars_id));
    let dec = new AST.irdefinition(TYP.Declaration (nmg_id)) in
    Hashtbl.iter (fun x y -> 
      if(y#get_name == scope) then 
        (AST.find_tbl AST.body_tbl y#get_body)#add_body_dcl (dec)
    ) AST.function_tbl



let rec right_part_of_expression expression =
  match (AST.find_tbl AST.expression_tbl expression)#get_type with
  | TYP.Exp exp -> right_part_of_expression exp
  | TYP.Binary (_, _, exp) -> exp
  | _ -> failwith "No right part"

let rec operator_of_expression expression =
  match (AST.find_tbl AST.expression_tbl expression)#get_type with
  | TYP.Exp exp -> operator_of_expression exp
  | TYP.Binary (op, _, _) -> op
  | _ -> failwith "No operator"


let rec new_right_part expression right_part location =
  match (AST.find_tbl AST.expression_tbl expression)#get_type with
  | TYP.Exp exp -> new_right_part exp right_part location
  | TYP.Binary (op, left, right) -> 
    let exp_id = new_id idx in
    AST.add_expression exp_id (new AST.irexpression (TYP.Binary (op, left, right_part), location));
    exp_id
  | _ -> failwith "No right part"
 





let stm_min = ref 0
let stm_max = ref 0
let get_inter_stm _ =
  (!stm_min, !stm_max)


let new_stm statement =
  stm_min := !idx;
  let stm = (AST.find_tbl AST.statement_tbl statement)#get_copy () in
  stm_max := !idx;
  stm
  








let trade_off_0 stm_id direction scope =
  let loc = TYP.Location (scope,-1) in
  (**)
  let get_new_for_conditions start stop =
    let cast_type = new_id idx in
      AST.add_type cast_type (new AST.irtype (TYP.Int ("",""), loc));

    let exp_block_size = float_cnst_expr (string_of_float (float_of_int !ARG.to_num /. float_of_int !ARG.to_den)) loc in
 
    let exp_dir = 
    begin match direction with
    | "increase" -> stop
    | "decrease" -> start
    | _ -> failwith "Impossible fail 1"
    end in

    let exp_new_block =  
    begin match (!ARG.to_first, direction) with
    | (true, "increase") -> exp_block_size
    | (false,"increase") -> float_cnst_expr (string_of_float (1. -. (float_of_int !ARG.to_num /. float_of_int !ARG.to_den))) loc
    | (true, "decrease") -> float_cnst_expr (string_of_float (1. -. (float_of_int !ARG.to_num /. float_of_int !ARG.to_den))) loc
    | (false,"decrease") -> exp_block_size
    | _ -> failwith "Impossible fail 2"
    end in

    let exp_stop2_tmp1 = new_id idx in
      AST.add_expression exp_stop2_tmp1 (new AST.irexpression (TYP.Binary(("*",true), exp_dir, exp_new_block), loc));
    (* let exp_stop2_tmp2 = new_id idx in *)
    (*   AST.add_expression exp_stop2_tmp2 (new AST.irexpression (TYP.Binary(("-",true), exp_dir, exp_stop2_tmp1), loc)); *)
    let exp_stop2 = new_id idx in
      AST.add_expression exp_stop2 (new AST.irexpression (TYP.Cast (cast_type, exp_stop2_tmp1), loc));

    (* let op =  *)
    (* begin match direction with *)
    (* | "increase" -> "+" *)
    (* | "decrease" -> "-" *)
    (* | _ -> failwith "Impossible fail 2" *)
    (* end in *)

    (* let exp_start2_tmp = new_id idx in *)
    (*   AST.add_expression exp_start2_tmp (new AST.irexpression(TYP.Binary((op,true), exp_stop2_tmp2, exp_one), loc)); *)
    (* let exp_start2 = new_id idx in *)
    (*   AST.add_expression exp_start2 (new AST.irexpression (TYP.Cast (cast_type, exp_start2_tmp), loc)); *)

    (* (exp_stop2, exp_start2) *)
    exp_stop2

  in
  (**)
  let get_new_for_statements exp_start exp_stop exp_iter stm_body =

    let endloop = new_expr (new AST.irexpression (TYP.Variable ("cohd_end"), loc)) in
    let exp_one = int_cnst_expr "1" loc in
    add_variables ["cohd_end"] scope loc;

    let start = right_part_of_expression exp_start in
    let stop = right_part_of_expression exp_stop in

    (* let (r_stop2, r_start2) = get_new_for_conditions start stop in *)
    let r_stop2_tmp = get_new_for_conditions start stop in
    let r_stop2_exp = new_expr (new AST.irexpression (TYP.Binary(binary_assign, endloop, r_stop2_tmp), loc)) in
    let r_stop2 = new_stmt (new AST.irstatement (TYP.Computation (r_stop2_exp), loc)) in

    let op =
    begin match direction with
    | "increase" -> "+"
    | "decrease" -> "-"
    | _ -> failwith "Impossible fail 2"
    end in

    let cond = operator_of_expression exp_stop in

    let r_start2 = 
    begin
      match cond with
      | (">=", _) | ("<=", _) ->
        new_expr (new AST.irexpression(TYP.Binary((op,true), endloop, exp_one), loc))
      | (">", _) | ("<", _) -> (*si pas égal dans la condition, ne pas faire le +/- 1*)
        endloop
      | _ -> failwith "condition pas supportée (tradeoff1)"
    end
    in

    let exp_stop2 = new_right_part exp_stop endloop loc in
    let exp_start2 = new_right_part exp_start r_start2 loc in

    let (stm_body1, stm_body2) =  
    begin match !ARG.to_first with
    | true  -> (stm_body, new_stm stm_body)
    | false -> (new_stm stm_body, stm_body)
    end in

    let stm_for1 = new_id idx in
      AST.add_statement stm_for1 (new AST.irstatement (TYP.For(exp_start, exp_stop2, exp_iter, stm_body1), loc));
    let stm_for2 = new_id idx in
      AST.add_statement stm_for2 (new AST.irstatement (TYP.For(exp_start2, exp_stop, exp_iter, stm_body2), loc));

    loop_nop_id :=
    begin match !ARG.to_first with
    | true  -> stm_for2
    | false -> stm_for1
    end;


    (r_stop2, stm_for1, stm_for2)
  in
  (**)
  let stm = AST.find_tbl AST.statement_tbl stm_id in
  match stm#get_stmt_type with
  | TYP.For (e1, e2, e3, s) -> 
    begin 
      let (endloop, for1, for2) = get_new_for_statements e1 e2 e3 s in
      Hashtbl.replace AST.statement_tbl stm_id 
        (new AST.irstatement (TYP.Sequence(endloop,
          new_stmt (new AST.irstatement (TYP.Sequence (for1, for2), loc))), loc))

    end
  | _ -> failwith "Pattern matching suposed to be For _ !"







(******************************************************************************)
(** Type 1 -- Trade Off *)
let trade_off_1 stm_id direction scope =
  let loc = TYP.Location (scope, -1) in (** Location of loops transformations *)
  let cohdi = new_expr (new AST.irexpression (TYP.Variable ("cohd_i"), loc)) in
  let endloop2 = new_expr (new AST.irexpression (TYP.Variable ("cohd_end2"), loc)) in
  let endloop3 = new_expr (new AST.irexpression (TYP.Variable ("cohd_end3"), loc)) in
  (**)
  add_variables ["cohd_i";"cohd_end2";"cohd_end3"] scope loc;
  (** Create the new main For loop conditions *)
  let new_for1_conditions for_begin for_end for_it =
    let begin_right = right_part_of_expression for_begin in
    let begin_op    = operator_of_expression   for_begin in
    let end_right   = right_part_of_expression for_end in    
    let end_op      = operator_of_expression   for_end in

    let start = new_expr 
      (new AST.irexpression (TYP.Binary (begin_op, cohdi, begin_right), loc)) in
    let stop  = new_expr
      (new AST.irexpression (TYP.Binary (end_op,   cohdi, end_right), loc)) in

    let pas_exp = int_cnst_expr (string_of_int !ARG.to_den) loc in
    let it_op   = ((if direction <> "increase" then "-" else "+"), true) in
    let it_exp  = new_expr
      (new AST.irexpression (TYP.Binary (it_op, cohdi, pas_exp), loc)) in

    let iterator = new_expr
      (new AST.irexpression (TYP.Binary (("=", false), cohdi, it_exp), loc)) in

    (start, stop, iterator)
  in
  (**)
  let end_of_loop2 for_end for_it =
    let block_exp   = 
      begin match !ARG.to_first with
      | true  -> int_cnst_expr (string_of_int !ARG.to_num) loc 
      | false -> int_cnst_expr (string_of_int (!ARG.to_den - !ARG.to_num)) loc
      end
    in
    let end_right   = right_part_of_expression for_end in    
    let end_op      = operator_of_expression   for_end in

    let op  = ((if direction <> "increase" then "-" else "+"), true) in


    let cond = operator_of_expression for_end in
    let exp = 
    begin
      match cond with
      | (">=", _) | ("<=", _) ->
        new_expr (new AST.irexpression (TYP.Binary (op, cohdi, 
          new_expr (new AST.irexpression (TYP.Binary (op, block_exp, (int_cnst_expr "1" loc)), loc))
        ), loc))
      | (">", _) | ("<", _) -> (*si pas égal dans la condition, ne pas faire le +/- 1*)
        new_expr (new AST.irexpression (TYP.Binary (op, cohdi, block_exp), loc))
      | _ -> failwith "condition pas supportée (tradeoff2-3)"
    end
    in

    (* let exp = new_expr (new AST.irexpression (TYP.Binary (op, cohdi,  *)
    (*   new_expr (new AST.irexpression (TYP.Binary (op, block_exp, (int_cnst_expr "1" loc)), loc)) *)
    (* ), loc)) in *)



    let ask = new_expr (new AST.irexpression (TYP.Binary (end_op, exp, end_right), loc)) in

    new_stmt (new AST.irstatement (TYP.Computation (
      new_expr (new AST.irexpression (TYP.Binary   (binary_assign, endloop2, 
        new_expr (new AST.irexpression (TYP.Question (ask, exp, end_right), loc))
      ), loc))
    ), loc))
  in
  (**)
  let end_of_loop3 for_end for_it =
    let block_exp   = int_cnst_expr (string_of_int !ARG.to_den) loc in
    let end_right   = right_part_of_expression for_end in    
    let end_op      = operator_of_expression   for_end in

    let op  = ((if direction <> "increase" then "-" else "+"), true) in

    let cond = operator_of_expression for_end in
    let exp = 
    begin
      match cond with
      | (">=", _) | ("<=", _) ->
        new_expr (new AST.irexpression (TYP.Binary (op, cohdi,
          new_expr (new AST.irexpression (TYP.Binary (op, block_exp, (int_cnst_expr "1" loc)), loc))
        ), loc))
      | (">", _) | ("<", _) -> (*si pas égal dans la condition, ne pas faire le +/- 1*)
        new_expr (new AST.irexpression (TYP.Binary (op, cohdi, block_exp), loc))
      | _ -> failwith "condition pas supportée (tradeoff2-2)"
    end
    in


    (* let exp = new_expr (new AST.irexpression (TYP.Binary (op, cohdi,  *)
    (*   new_expr (new AST.irexpression (TYP.Binary (op, block_exp, (int_cnst_expr "1" loc)), loc)) *)
    (* ), loc)) in *)


    let ask = new_expr (new AST.irexpression (TYP.Binary (end_op, exp, end_right), loc)) in

    new_stmt (new AST.irstatement (TYP.Computation (
      new_expr (new AST.irexpression (TYP.Binary   (binary_assign, endloop3, 
        new_expr (new AST.irexpression (TYP.Question (ask, exp, end_right), loc))
      ), loc))
    ), loc))
  in
  (**)
  let new_body for_begin for_end for_it body =

    let (stm_body1, stm_body2) =  
    begin match !ARG.to_first with
    | true  -> (body, new_stm body)
    | false -> (new_stm body, body)
    end in

    let op = ((if direction <> "increase" then "-" else "+"), true) in

    let cond = operator_of_expression for_end in

    let endloop = 
    begin
      match cond with
      | (">=", _) | ("<=", _) ->
        new_expr (new AST.irexpression (TYP.Binary (op, endloop2, (int_cnst_expr "1" loc)), loc))
      | (">", _) | ("<", _) -> (*si pas égal dans la condition, ne pas faire le +/- 1*)
        endloop2
      | _ -> failwith "condition pas supportée (tradeoff2)"
    end
    in

    (* let endloop = new_expr (new AST.irexpression (TYP.Binary (op, endloop2, (int_cnst_expr "1" loc)), loc)) in *)


    let loop3 = new_stmt (new AST.irstatement (TYP.For ((new_right_part for_begin endloop loc), 
                                                        (new_right_part for_end endloop3 loc), 
                                                        for_it, stm_body2), loc)) in

    let loop2 = new_stmt (new AST.irstatement (TYP.For ((new_right_part for_begin cohdi loc), 
                                                        (new_right_part for_end endloop2 loc), 
                                                        for_it, stm_body1), loc)) in

    loop_nop_id :=
    begin match !ARG.to_first with
    | true  -> loop3
    | false -> loop2
    end;

    let stmt_seq3 = new_stmt (new AST.irstatement (TYP.Sequence (loop2, loop3), loc)) in
    let stmt_seq2 = new_stmt (new AST.irstatement (TYP.Sequence ((end_of_loop3 for_end for_it), stmt_seq3), loc)) in
    let stmt_seq1 = new_stmt (new AST.irstatement (TYP.Sequence ((end_of_loop2 for_end for_it), stmt_seq2), loc)) in


    let blk = new_id idx in
      AST.add_body blk (new AST.irbody ([], stmt_seq1));
    new_stmt (new AST.irstatement (TYP.Block (blk), loc))
  in
  (**)
  let get_new_for_statement for_begin for_end for_it body =

    let body1 = new_body for_begin for_end for_it body in

    let (start1, stop1, it1) = new_for1_conditions for_begin for_end for_it in
    new AST.irstatement (TYP.For (start1, stop1, it1, body1), loc)
  in 
  (**)
  let stm = AST.find_tbl AST.statement_tbl stm_id in
  match stm#get_stmt_type with
  | TYP.For (e1, e2, e3, s) -> 
    begin 
      let for_stmt = get_new_for_statement e1 e2 e3 s in
      Hashtbl.replace AST.statement_tbl stm_id for_stmt

    end
  | _ -> failwith "Pattern matching suposed to be For _ !"






let process_trade_off stm_id direction scope =
  if !ARG.to_block then (* if trade off is of type 0 *)
    begin
      trade_off_0 stm_id direction scope 
    end
  else (* trade off is of type 1 *)
    begin
    if !ARG.to_alter then
      begin
        trade_off_1 stm_id direction scope 
      end
    end


(*** main function *)
let launch_pass file = 
  PSC.clear_tbl ();
  if !ARG.verbose then print_string ":: Trade Off Pass\n";

  (* build for statement hash table *)
  buildForStatementHashTable file#get_file_dcl;
  (* build pragma list *)
  (* build_pragma_list file#get_name; *)
  pragma_list := PP.for_pragma_list (file#get_name);

  (* apply transformations on for loops tagged with a pragma *)
  if (List.length !pragma_list) > 0 
  then
    begin (* for all pragma *)
    List.iter (
      fun (line, pragma) ->
	let pragma_words = Str.split (Str.regexp "[ \t]+") pragma in
            Hashtbl.iter (
              fun id (scope, stm) ->
                if(stm#get_line_number = line) then 
		  begin (* if statement For as same line number of pragma then process the tradeoff *)
		    let direction = (if List.mem "decrease" pragma_words then "decrease" else "increase") in
                    process_trade_off id direction scope
		  end
            ) for_tbl
    ) !pragma_list
    end
  else
    prerr_string "No pragma(s) : exit pass\n";

