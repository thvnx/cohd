open Libcommon
open Astcommon

module ARG = Commandline
module AST = Irtree
module TYP = Types
module PSC = Passcommon
module CON = Config

(******************************************************************************)
(******************************************************************************)
(* Pragmas manipulation                                                       *)
(******************************************************************************)
(******************************************************************************)

(** list of pragma in C source file (line number * pragma declaration) **)
let (pragma_list:(int * string) list ref) = ref []

let build_pragma_list filename =
  let detect_pragma line number =
    try (
      if Str.string_match (Str.regexp "#pragma") line (String.index line '#') 
      then pragma_list := (number, line) :: !pragma_list
    ) with Not_found -> ()
  in
  let rec read_file channel line_number =
    try (
      let line = input_line channel in
      detect_pragma line (line_number + 1);
      read_file channel (line_number + 1)
    )
    with End_of_file -> ()
  in
  let chan = open_in filename in
  read_file chan 1;
  close_in chan


let get_pragma_list prag =
  let (pragmal:(int * string) list ref) = ref [] in
  List.iter (
    fun (line, pragma) ->
      let pragma_words = Str.split (Str.regexp "[ \t]+") pragma in
      if List.mem prag pragma_words then
      begin 
        pragmal := (line, pragma) :: !pragmal
      end
    ) !pragma_list;
  !pragmal
 

let for_pragma_list filename = 
  if (List.length !pragma_list) < 1 then build_pragma_list filename;
  get_pragma_list "for"
let time_pragma_list _     = get_pragma_list "time"
let accuracy_pragma_list _ = get_pragma_list "accuracy"

(******************************************************************************)
(******************************************************************************)
(* C PAPI calls                                                               *)
(******************************************************************************)
(******************************************************************************)

let papi_start = "PAPI_start_counters"
let papi_stop = "PAPI_stop_counters"
let papi_read = "PAPI_read_counters"
let papi_array_events = "__p_events"
let papi_array_results = "__p_results"
let papi_array_results_accum = "__p_accum"
let papi_array_size = "3"
let papi_tot_ins = "PAPI_TOT_INS"
let papi_tot_cyc = "PAPI_TOT_CYC"
let papi_fp_ins = "PAPI_FP_INS"
let papi_iterator = "__p_i"
let papi_max_it = "__p_n"
let papi_trash = "__p_trsh"

let papi_dcl _ = 
  (*array of events*)
  let typ1_id = new_id idx in
    AST.add_type typ1_id (new AST.irtype (TYP.Int ("", ""), TYP.NoLocation));
  let array_exp = int_cnst_expr papi_array_size TYP.NoLocation in
  let array_id = new_id idx in
    AST.add_array array_id (new AST.irarray (array_id, array_exp));
  let typ2_id = new_id idx in
    AST.add_type typ2_id (new AST.irtype (TYP.Array (array_id, typ1_id), TYP.NoLocation));
  let exp1 = new_id idx in
  let exp2 = new_id idx in
  let exp3 = new_id idx in
    AST.add_expression exp1 (new AST.irexpression (TYP.Variable (papi_tot_cyc), TYP.NoLocation));
    AST.add_expression exp2 (new AST.irexpression (TYP.Variable (papi_tot_ins), TYP.NoLocation));
    AST.add_expression exp3 (new AST.irexpression (TYP.Variable (papi_fp_ins), TYP.NoLocation));
  let cst_id = new_id idx in
    AST.add_constant cst_id (new AST.irconstant (TYP.CExpList ([exp1;exp2;exp3])));
  let exp_id = new_id idx in
    AST.add_expression exp_id (new AST.irexpression (TYP.Constant (cst_id), TYP.NoLocation));
  let var_id = new_id idx in
    AST.add_variable var_id (new AST.irvariable (papi_array_events, typ2_id, exp_id));
  let nmg_id = new_id idx in
    AST.add_namegroup nmg_id (new AST.irnamegroup (typ2_id, "", [var_id]));
  (*array of results*)
  let typ3_id = new_id idx in
    AST.add_type typ3_id (new AST.irtype (TYP.Int ("long long ", ""), TYP.NoLocation));
  let array2_exp = int_cnst_expr papi_array_size TYP.NoLocation in
  let array2_id = new_id idx in
    AST.add_array array2_id (new AST.irarray (array2_id, array2_exp));
  let typ4_id = new_id idx in
    AST.add_type typ4_id (new AST.irtype (TYP.Array (array2_id, typ3_id), TYP.NoLocation));
  let exp4 = int_cnst_expr "0" TYP.NoLocation in
  let cst_id = new_id idx in
    AST.add_constant cst_id (new AST.irconstant (TYP.CExpList ([exp4])));
  let exp2_id = new_id idx in
    AST.add_expression exp2_id (new AST.irexpression (TYP.Constant (cst_id), TYP.NoLocation));
  let var2_id = new_id idx in
    AST.add_variable var2_id (new AST.irvariable (papi_array_results, typ4_id, exp2_id));
  let nmg2_id = new_id idx in
    AST.add_namegroup nmg2_id (new AST.irnamegroup (typ4_id, "", [var2_id]));
  (*array of results accumulation *)
  let var3_id = new_id idx in
    AST.add_variable var3_id (new AST.irvariable (papi_array_results_accum, typ4_id, exp2_id));
  let nmg3_id = new_id idx in
    AST.add_namegroup nmg3_id (new AST.irnamegroup (typ4_id, "", [var3_id]));
  (*variables*)
  let var_id_i = new_id idx in
    AST.add_variable var_id_i (new AST.irvariable (papi_iterator, typ1_id, int_cnst_expr "0" TYP.NoLocation));
  let var_id_n = new_id idx in
    AST.add_variable var_id_n (new AST.irvariable (papi_max_it, typ1_id, int_cnst_expr (string_of_int !ARG.p_iterator) TYP.NoLocation));
  let var_id_t = new_id idx in
    AST.add_variable var_id_t (new AST.irvariable (papi_trash, typ1_id, int_cnst_expr (string_of_int CON.papi_trash) TYP.NoLocation));
  let nmg4_id = new_id idx in
    AST.add_namegroup nmg4_id (new AST.irnamegroup (typ1_id, "", [var_id_i; var_id_n; var_id_t]));

  [new AST.irdefinition (TYP.Declaration nmg_id); new AST.irdefinition (TYP.Declaration nmg2_id); 
      new AST.irdefinition (TYP.Declaration nmg3_id); new AST.irdefinition (TYP.Declaration nmg4_id)]



let stmt_papi_func fn_name papi_array =
  let func_id = new_id idx in
    AST.add_expression func_id (new AST.irexpression (TYP.Variable (fn_name), TYP.NoLocation));
  let param1 = new_id idx in
  AST.add_expression param1 (new AST.irexpression (TYP.Variable (papi_array), TYP.NoLocation));
  let param2 = int_cnst_expr papi_array_size TYP.NoLocation in
  let call_id = new_id idx in
    AST.add_expression call_id (new AST.irexpression (TYP.Call (func_id, [param1;param2]), TYP.NoLocation));
  let stmt_id = new_id idx in 
    AST.add_statement stmt_id (new AST.irstatement (TYP.Computation (call_id), TYP.NoLocation));
  stmt_id

let stmt_papi_print _ =
  let func_id = new_id idx in
    AST.add_expression func_id (new AST.irexpression (TYP.Variable ("printf"), TYP.NoLocation));
  let param1 = new_id idx in
  let cst1 = new_id idx in
    AST.add_constant cst1 (new AST.irconstant (TYP.CString ("@TC %lld\n@TI %lld\n@FP %lld\n")));
    AST.add_expression param1 (new AST.irexpression (TYP.Constant (cst1), TYP.NoLocation));
  let cst2 = int_cnst_expr "0" TYP.NoLocation in
  let cst3 = int_cnst_expr "1" TYP.NoLocation in
  let cst4 = int_cnst_expr "2" TYP.NoLocation in
  let table = new_id idx in
    AST.add_expression table (new AST.irexpression (TYP.Variable (papi_array_results), TYP.NoLocation));
  let param2 = new_id idx in
    AST.add_expression param2 (new AST.irexpression (TYP.Index (table, cst2), TYP.NoLocation));
  let param3 = new_id idx in
    AST.add_expression param3 (new AST.irexpression (TYP.Index (table, cst3), TYP.NoLocation));
  let param4 = new_id idx in
    AST.add_expression param4 (new AST.irexpression (TYP.Index (table, cst4), TYP.NoLocation));
  let call_id = new_id idx in
    AST.add_expression call_id (new AST.irexpression (TYP.Call (func_id, [param1;param2;param3;param4]), TYP.NoLocation));
  let stmt_id = new_id idx in 
    AST.add_statement stmt_id (new AST.irstatement (TYP.Computation (call_id), TYP.NoLocation));
  stmt_id


let stmt_papi_print_accum _ =
  let loc = TYP.NoLocation in
  let papi_n = new_expr (new AST.irexpression (TYP.Variable (papi_max_it), loc)) in

  let func_id = new_id idx in
    AST.add_expression func_id (new AST.irexpression (TYP.Variable ("printf"), TYP.NoLocation));
  let param1 = new_id idx in
  let cst1 = new_id idx in
    AST.add_constant cst1 (new AST.irconstant (TYP.CString ("@TC %lld\n@TI %lld\n@FP %lld\n")));
    AST.add_expression param1 (new AST.irexpression (TYP.Constant (cst1), TYP.NoLocation));
  let cst2 = int_cnst_expr "0" TYP.NoLocation in
  let cst3 = int_cnst_expr "1" TYP.NoLocation in
  let cst4 = int_cnst_expr "2" TYP.NoLocation in
  let table = new_id idx in
    AST.add_expression table (new AST.irexpression (TYP.Variable (papi_array_results_accum), TYP.NoLocation));
  let param2 = new_id idx in
    AST.add_expression param2 (new AST.irexpression (TYP.Index (table, cst2), TYP.NoLocation));
  let param22 = new_expr (new AST.irexpression (TYP.Binary ((binary_div), param2, papi_n), loc)) in
  let param3 = new_id idx in
    AST.add_expression param3 (new AST.irexpression (TYP.Index (table, cst3), TYP.NoLocation));
  let param33 = new_expr (new AST.irexpression (TYP.Binary ((binary_div), param3, papi_n), loc)) in
  let param4 = new_id idx in
    AST.add_expression param4 (new AST.irexpression (TYP.Index (table, cst4), TYP.NoLocation));
  let param44 = new_expr (new AST.irexpression (TYP.Binary ((binary_div), param4, papi_n), loc)) in
  let call_id = new_id idx in
    AST.add_expression call_id (new AST.irexpression (TYP.Call (func_id, [param1;param22;param33;param44]), TYP.NoLocation));
  let stmt_id = new_id idx in 
    AST.add_statement stmt_id (new AST.irstatement (TYP.Computation (call_id), TYP.NoLocation));
  stmt_id


let stmt_papi_print_result _ =
  let loc = TYP.NoLocation in
  let papi_n = new_expr (new AST.irexpression (TYP.Variable (papi_max_it), loc)) in

  let func_id = new_id idx in
    AST.add_expression func_id (new AST.irexpression (TYP.Variable ("printf"), TYP.NoLocation));
  let param1 = new_id idx in
  let cst1 = new_id idx in
    AST.add_constant cst1 (new AST.irconstant (TYP.CString ("@TC %lld\n@TI %lld\n@FP %lld\n")));
    AST.add_expression param1 (new AST.irexpression (TYP.Constant (cst1), TYP.NoLocation));
  let cst2 = int_cnst_expr "0" TYP.NoLocation in
  let cst3 = int_cnst_expr "1" TYP.NoLocation in
  let cst4 = int_cnst_expr "2" TYP.NoLocation in
  let table = new_id idx in
    AST.add_expression table (new AST.irexpression (TYP.Variable (papi_array_results), TYP.NoLocation));
  let param2 = new_id idx in
    AST.add_expression param2 (new AST.irexpression (TYP.Index (table, cst2), TYP.NoLocation));
  let param22 = new_expr (new AST.irexpression (TYP.Binary ((binary_div), param2, papi_n), loc)) in
  let param3 = new_id idx in
    AST.add_expression param3 (new AST.irexpression (TYP.Index (table, cst3), TYP.NoLocation));
  let param33 = new_expr (new AST.irexpression (TYP.Binary ((binary_div), param3, papi_n), loc)) in
  let param4 = new_id idx in
    AST.add_expression param4 (new AST.irexpression (TYP.Index (table, cst4), TYP.NoLocation));
  let param44 = new_expr (new AST.irexpression (TYP.Binary ((binary_div), param4, papi_n), loc)) in
  let call_id = new_id idx in
    AST.add_expression call_id (new AST.irexpression (TYP.Call (func_id, [param1;param22;param33;param44]), TYP.NoLocation));
  let stmt_id = new_id idx in 
    AST.add_statement stmt_id (new AST.irstatement (TYP.Computation (call_id), TYP.NoLocation));
  stmt_id


let stmt_papi_accum _ =
  let loc = TYP.NoLocation in
  let papi_it = new_expr (new AST.irexpression (TYP.Variable (papi_iterator), loc)) in
  let papi_t = new_expr (new AST.irexpression (TYP.Variable (papi_trash), loc)) in

  let exp_cond = new_expr (new AST.irexpression (TYP.Binary ((">=", false), papi_it, papi_t), loc)) in

  let else_stmt = new_stmt (new AST.irstatement (TYP.Nop, loc)) in


  let array_res = new_expr (new AST.irexpression (TYP.Variable papi_array_results, loc)) in 
  let array_acc = new_expr (new AST.irexpression (TYP.Variable papi_array_results_accum, loc)) in 
  let array_res0 = new_expr (new AST.irexpression (TYP.Index (array_res, int_cnst_expr "0" loc), loc)) in 
  let array_acc0 = new_expr (new AST.irexpression (TYP.Index (array_acc, int_cnst_expr "0" loc), loc)) in 
  let array_res1 = new_expr (new AST.irexpression (TYP.Index (array_res, int_cnst_expr "1" loc), loc)) in 
  let array_acc1 = new_expr (new AST.irexpression (TYP.Index (array_acc, int_cnst_expr "1" loc), loc)) in 
  let array_res2 = new_expr (new AST.irexpression (TYP.Index (array_res, int_cnst_expr "2" loc), loc)) in 
  let array_acc2 = new_expr (new AST.irexpression (TYP.Index (array_acc, int_cnst_expr "2" loc), loc)) in 


  let exp1 = new_expr (new AST.irexpression (TYP.Binary (("+=", false), array_acc0, array_res0), loc)) in
  let exp2 = new_expr (new AST.irexpression (TYP.Binary (("+=", false), array_acc1, array_res1), loc)) in
  let exp3 = new_expr (new AST.irexpression (TYP.Binary (("+=", false), array_acc2, array_res2), loc)) in

  let stm5 = new_stmt (new AST.irstatement (TYP.Computation (exp3), loc)) in
  let stm4 = new_stmt (new AST.irstatement (TYP.Computation (exp2), loc)) in
  let stm3 = new_stmt (new AST.irstatement (TYP.Computation (exp1), loc)) in
  let stm2 = new_stmt (new AST.irstatement (TYP.Sequence (stm3, stm4), loc)) in
  let stm1 = new_stmt (new AST.irstatement (TYP.Sequence (stm2, stm5), loc)) in

  let then_body = new_id idx in
    AST.add_body then_body (new AST.irbody ([], stm1));
  let then_stmt = new_stmt (new AST.irstatement (TYP.Block then_body, loc)) in

  let if_stmt = new_stmt (new AST.irstatement (TYP.If (exp_cond, then_stmt, else_stmt), loc)) in

  let seq = new_stmt (new AST.irstatement (TYP.Sequence (
    stmt_papi_func papi_read papi_array_results,
    if_stmt
  ), loc)) in
  seq



let papi_loop s_blk =
  let loc = TYP.NoLocation in

  let build_for_loop_papi block_stm =
    let papi_it = new_expr (new AST.irexpression (TYP.Variable (papi_iterator), loc)) in
    let papi_n = new_expr (new AST.irexpression (TYP.Variable (papi_max_it), loc)) in
    let papi_t = new_expr (new AST.irexpression (TYP.Variable (papi_trash), loc)) in
    let exp_start = new_expr 
      (new AST.irexpression (TYP.Binary(binary_assign, papi_it, (int_cnst_expr "0" loc)), loc)) in
    (* let exp_stop2 = new_expr *)
    (*   (new AST.irexpression (TYP.Binary(binary_add, papi_n, papi_t), loc)) in *)
    let exp_stop = new_expr 
      (new AST.irexpression (TYP.Binary(("<", false), papi_it, papi_n), loc)) in
    let exp_it = new_expr 
      (new AST.irexpression (TYP.Unary(("++", true), papi_it), loc)) in

    (*  let read_stmt = (stmt_papi_func papi_read papi_array_results) in *)

    (* let seq1 = new_stmt (new AST.irstatement (TYP.Sequence (read_stmt, block_stm), loc)) in *)
    (* let seq2 = new_stmt (new AST.irstatement (TYP.Sequence (seq1, stmt_papi_accum ()), loc)) in *)
    
    (* let bblk = new_id idx in  *)
    (*   AST.add_body bblk (new AST.irbody ([], seq2)); *)

    let bblk = new_id idx in
      AST.add_body bblk (new AST.irbody ([], block_stm));

    let block = new_stmt (new AST.irstatement (TYP.Block (bblk), loc)) in


    let stm_id = new_stmt (new AST.irstatement (TYP.For (exp_start, exp_stop, exp_it, block), loc)) in
    stm_id
  in
  let build_for_loop_tmp block_stm =
    let papi_it = new_expr (new AST.irexpression (TYP.Variable (papi_iterator), loc)) in
    let papi_n = new_expr (new AST.irexpression (TYP.Variable (papi_max_it), loc)) in
    let papi_t = new_expr (new AST.irexpression (TYP.Variable (papi_trash), loc)) in
    let exp_start = new_expr 
      (new AST.irexpression (TYP.Binary(binary_assign, papi_it, (int_cnst_expr "0" loc)), loc)) in
    let exp_stop = new_expr 
      (new AST.irexpression (TYP.Binary(("<", false), papi_it, papi_t), loc)) in
    let exp_it = new_expr 
      (new AST.irexpression (TYP.Unary(("++", true), papi_it), loc)) in

    (*  let read_stmt = (stmt_papi_func papi_read papi_array_results) in *)

    (* let seq1 = new_stmt (new AST.irstatement (TYP.Sequence (read_stmt, block_stm), loc)) in *)
    (* let seq2 = new_stmt (new AST.irstatement (TYP.Sequence (seq1, stmt_papi_accum ()), loc)) in *)
    
    (* let bblk = new_id idx in  *)
    (*   AST.add_body bblk (new AST.irbody ([], seq2)); *)

    let b_copy = (AST.find_tbl AST.statement_tbl block_stm)#get_copy () in

    let bblk = new_id idx in
      AST.add_body bblk (new AST.irbody ([], b_copy));

    let block = new_stmt (new AST.irstatement (TYP.Block (bblk), loc)) in


    let stm_id = new_stmt (new AST.irstatement (TYP.For (exp_start, exp_stop, exp_it, block), loc)) in
    stm_id


  in
    let s_blk_id = new_id idx in AST.add_statement s_blk_id s_blk; 
    let stm_loop_papi = build_for_loop_papi s_blk_id in
    let stm_loop_tmp = build_for_loop_tmp s_blk_id in



    let seq0 = new_stmt 
      (new AST.irstatement (TYP.Sequence (stm_loop_tmp, (stmt_papi_func papi_start papi_array_events)), loc)) in
    let seq1 = new_stmt 
      (new AST.irstatement (TYP.Sequence (seq0 ,stm_loop_papi), loc)) in
    let seq2 = new_stmt 
      (new AST.irstatement (TYP.Sequence (
        (stmt_papi_func papi_stop papi_array_results), 
        (* (stmt_papi_print_accum ()) *)
        (stmt_papi_print_result ())
      ), loc)) 
    in
  new AST.irstatement (TYP.Sequence (seq1, seq2), loc)

  


let seq_start stm =(*old*)
  let stm_id = new_id idx in AST.add_statement stm_id stm; 
  new AST.irstatement (TYP.Sequence ((stmt_papi_func papi_start papi_array_events), stm_id), TYP.NoLocation)

let seq_stop stm =(*old*)
  let stm_id = new_id idx in AST.add_statement stm_id stm; 
  let seq_id1 = new_id idx in
    AST.add_statement seq_id1 (new AST.irstatement (TYP.Sequence (
               (stmt_papi_func papi_stop papi_array_results), 
               (stmt_papi_func papi_read papi_array_results)), TYP.NoLocation));
  let seq_id2 = new_id idx in
    AST.add_statement seq_id2 (new AST.irstatement (TYP.Sequence ((seq_id1), (stmt_papi_print ())), TYP.NoLocation));
  new AST.irstatement (TYP.Sequence (seq_id2, stm_id), TYP.NoLocation)




(******************************************************************************)
(******************************************************************************)
(* C accuracy calls                                                           *)
(******************************************************************************)
(******************************************************************************)

let stmt_print_accuracy var_name =
  let func_id = new_id idx in
    AST.add_expression func_id (new AST.irexpression (TYP.Variable ("printf"), TYP.NoLocation));
  let param1 = new_id idx in
  let cst1 = new_id idx in
    AST.add_constant cst1 (new AST.irconstant (TYP.CString ("@AC "^var_name^"=%.16e\n")));
    AST.add_expression param1 (new AST.irexpression (TYP.Constant (cst1), TYP.NoLocation));
  let param2 = new_id idx in
    AST.add_expression param2 (new AST.irexpression (TYP.Variable (var_name), TYP.NoLocation));
   let call_id = new_id idx in
    AST.add_expression call_id (new AST.irexpression (TYP.Call (func_id, [param1;param2]), TYP.NoLocation));
  let stmt_id = new_id idx in 
    AST.add_statement stmt_id (new AST.irstatement (TYP.Computation (call_id), TYP.NoLocation));
  stmt_id

let seq_accuracy stm var_name =
  let stm_id = new_id idx in AST.add_statement stm_id stm; 
  new AST.irstatement (TYP.Sequence (stmt_print_accuracy var_name, stm_id), TYP.NoLocation)

let seq_accuracy2 stm var_name =
  let stm_id = new_id idx in AST.add_statement stm_id stm; 
  new AST.irstatement (TYP.Sequence (stm_id, stmt_print_accuracy var_name), TYP.NoLocation)









(******************************************************************************)
(******************************************************************************)
(* main                                                                       *)
(******************************************************************************)
(******************************************************************************)

let launch_pass file =
  if !ARG.verbose then print_string ":: Preprocess Pass\n";

  (* build pragma list *)
  build_pragma_list file#get_name;


  if !ARG.p_performances then
  begin
  (* papi *)
  List.iter (fun i -> file#add_dcl (i) ) (papi_dcl ());

  List.iter (
    fun (line, pragma) ->
      let pragma_words = Str.split (Str.regexp "[ \t]+") pragma in

      if List.mem "start" pragma_words then
      	begin
      	  let stm_id = PSC.get_statement_id_by_line_number line in
      	  let stm = AST.find_tbl AST.statement_tbl stm_id in
            Hashtbl.replace AST.statement_tbl stm_id (seq_start stm)
      	end;
      if List.mem "stop" pragma_words then
      	begin
      	  let stm_id = PSC.get_statement_id_by_line_number line in
      	  let stm = AST.find_tbl AST.statement_tbl stm_id in
            Hashtbl.replace AST.statement_tbl stm_id (seq_stop stm)
      	end;
      if List.mem "block" pragma_words then
	begin
            let stm_blk = PSC.get_statement_blk_id_by_line_number line in
            let stm = AST.find_tbl AST.statement_tbl stm_blk in
            Hashtbl.replace AST.statement_tbl stm_blk (papi_loop stm)
	end

  ) (time_pragma_list ()) ;  

  end;

  if !ARG.p_accuracy then
  begin
  (* accuracy *)
  List.iter (
    fun (line, pragma) ->
      let pragma_words = Str.split (Str.regexp "[ \t]+") pragma in
        let stm_id = PSC.get_statement_id_by_line_number line in
	let stm = AST.find_tbl AST.statement_tbl stm_id in
          Hashtbl.replace AST.statement_tbl stm_id (seq_accuracy stm (List.nth pragma_words 3))

  ) (accuracy_pragma_list ()) ;  
  end



(******************************************************************************)
(******************************************************************************)
(* main 2 - devel version                                                     *)
(******************************************************************************)
(******************************************************************************)
let print_intermediate_results file =
  if !ARG.verbose then print_string ":: Print intermetiate results Pass\n";

  let stmt_to_replace = Hashtbl.create 0 in

  let rec get_exp_name exp =
    let expr = (AST.find_tbl AST.expression_tbl exp) in
    match expr#get_type with
    | TYP.Variable v -> v
    | TYP.Index (v, i) -> (get_exp_name v)
    | _ -> ""
  in
 
  let print_res stm_id bin_exp =
    let expr = (AST.find_tbl AST.expression_tbl bin_exp) in
    match expr#get_type with
    | TYP.Binary ((op,_), e1, e2) -> 
      if op = "=" && get_exp_name e2 = "" then 
        begin
          (* print_string (expr#to_string() ^ "\n"); *)
          let expr1 = (AST.find_tbl AST.expression_tbl e1) in
          Hashtbl.add stmt_to_replace stm_id (expr1#to_string()) (*(get_exp_name e1)*)
        end
    | _ -> ()
  in

  Hashtbl.iter ( fun x y -> 
    match y#get_stmt_type with
    | TYP.Computation e ->
      begin
      match (AST.find_tbl AST.expression_tbl e)#get_type with
      | TYP.Binary _ -> print_res x e
      | _ -> ()
      end
    | _ -> ()
  ) AST.statement_tbl;

  Hashtbl.iter ( fun x y ->
    let stm = AST.find_tbl AST.statement_tbl x in
    Hashtbl.replace AST.statement_tbl x (seq_accuracy2 stm y)
  ) stmt_to_replace
