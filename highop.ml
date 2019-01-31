(******************************************************************************)
(* History *********************************************************************

- September 25 2013 - revision 1.0 : first revision


******************************************************************** /History *)
(******************************************************************************)

(** High-Operators Library : 
    This file contains all the EFT algorithms (TwoProduct, TwoProductFMA, Split,
    TwoSum and FastTwoSum) needed to high-precision operators :
    - Double-Word operators and
    - Comp-Word operators *)

open Libcommon
open Astcommon

module TYP = Types
module AST = Irtree
module CMD = Commandline
module PSC = Passcommon


(******************************************************************************)

(** Variables and functions needed to transformations for some of the 
    High-precision operators *)

let nop = ref false     (** If is set then default operator is a nop operator *)
let set_nop _   = nop := true
let clear_nop _ = nop := false

let loop_pos = ref ""          (** Position of the loop for closing sequences *)

(** Closing a sequence of floating-point operations in nop operators when do not
    propagate the errors throught the nop operators
    @param exp_id ID of the 3-address form computation to close
    @param res_id ID of the variable to close
    @param scope Scope of the variable to close 
    @param loc Location of transformation *)
let close_sequence exp_id res_id scope loc =
  let op =
  match (AST.find_tbl AST.expression_tbl res_id)#get_type with
  | TYP.Unary (("-",_), e) -> e
  | _ -> res_id
  in

  let var_id = PSC.low_exp op "_low" scope true in

  let (pos, var_array) = (* Check if the variable is an array *)
    let var_string = (AST.find_tbl AST.expression_tbl var_id)#to_string () in
    if String.contains var_string '[' 
      then (exp_id   , true)  (* Close sequence inside of the loop *)
      else (!loop_pos, false) (* Close sequence outsile of the loop *)
  in
  (* close variable : add its lower part and set it to zero *)
  let exp1 = new_expr (new AST.irexpression 
    (TYP.Binary (("+=",false), op, var_id), loc)) in
   let exp2 = new_expr (new AST.irexpression 
    (TYP.Binary (binary_assign, var_id, (float_cnst_expr "0.0" loc)), loc)) in

  let stm0_id = new_stmt (new AST.irstatement (TYP.Computation (exp1), loc)) in
  let stm1_id = new_stmt (new AST.irstatement (TYP.Computation (exp2), loc)) in
  let stm2_id = new_stmt (new AST.irstatement 
                                     (TYP.Sequence (stm0_id, stm1_id), loc)) in

(* print_endline ("closing " ^ (
   AST.find_tbl AST.expression_tbl var_id)#to_string () ^ " at pos: " ^ pos); *)

  replace_stmt_by_sequence pos (if var_array then stm0_id else stm2_id) loc


(******************************************************************************)

(** Error-Free Transformations Library *)

(** Double precision splitter *)
let splitter location = int_cnst_expr "134217729" location 


(** Split Algorithm 
    @param exp_id Variable to split 
    @param scope Scope of the variable
    @param loc Location of transformation *)
let split exp_id scope loc =
  let exp_spl = splitter loc in 

  let var1_id = PSC.low_exp exp_id "_spl_1" scope false in
  let var2_id = PSC.low_exp exp_id "_spl_2" scope false in
  let splh_id = PSC.low_exp exp_id "_spl_h" scope false in
  let spll_id = PSC.low_exp exp_id "_spl_l" scope false in

  let exp1_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_mul   , exp_spl, exp_id) , loc)) in
  let exp2_id = new_expr (new AST.irexpression
                         (TYP.Binary (binary_assign, var1_id, exp1_id), loc)) in
  let exp3_id = new_expr (new AST.irexpression
                         (TYP.Binary (binary_sub   , exp_id , var1_id), loc)) in
  let exp4_id = new_expr (new AST.irexpression
                         (TYP.Binary (binary_assign, var2_id, exp3_id), loc)) in
  let exp5_id = new_expr (new AST.irexpression
                         (TYP.Binary (binary_add   , var1_id, var2_id), loc)) in
  let exp6_id = new_expr (new AST.irexpression
                         (TYP.Binary (binary_assign, splh_id, exp5_id), loc)) in
  let exp7_id = new_expr (new AST.irexpression
                         (TYP.Binary (binary_sub   , exp_id , splh_id), loc)) in
  let exp8_id = new_expr (new AST.irexpression
                         (TYP.Binary (binary_assign, spll_id, exp7_id), loc)) in
 
  let stm1_id = new_stmt (new AST.irstatement (TYP.Computation exp8_id, loc)) in
  let stm2_id = new_stmt (new AST.irstatement (TYP.Computation exp6_id, loc)) in
  let stm3_id = new_stmt (new AST.irstatement (TYP.Computation exp4_id, loc)) in
  let stm4_id = new_stmt (new AST.irstatement (TYP.Computation exp2_id, loc)) in
  let stm5_id = new_stmt (new AST.irstatement 
                                      (TYP.Sequence (stm2_id, stm1_id), loc)) in
  let stm6_id = new_stmt (new AST.irstatement 
                                      (TYP.Sequence (stm3_id, stm5_id), loc)) in
  let stm7_id = new_stmt (new AST.irstatement 
                                      (TYP.Sequence (stm4_id, stm6_id), loc)) in
  stm7_id 


(** TwoProduct Algorithm 
    @param exp_id Multiplication expression id
    @param res_id Result variable id
    @param low_name Result low part name
    @param bool_name True to keep array index
    @param op1_id First operand id to multiply
    @param op2_id Second operand id to multiply
    @param scope Scope of variables to multiply
    @param loc Location of transformation *)
let twoProduct exp_id res_id low_name bool_name op1_id op2_id scope loc =
  (* splitting operands *)
  let stm_split_op1 = split op1_id scope loc in
  let stm_split_op2 = split op2_id scope loc in

  let twoprod  = PSC.low_exp res_id low_name scope bool_name in
  let op1_h_id = PSC.low_exp op1_id "_spl_h" scope false in
  let op1_l_id = PSC.low_exp op1_id "_spl_l" scope false in
  let op2_h_id = PSC.low_exp op2_id "_spl_h" scope false in
  let op2_l_id = PSC.low_exp op2_id "_spl_l" scope false in

  let exp1_id  = new_expr (new AST.irexpression 
                           (TYP.Unary (unary_neg , res_id)            , loc)) in
  let exp2_id  = new_expr (new AST.irexpression 
                          (TYP.Binary (binary_mul, op1_h_id, op2_h_id), loc)) in
  let exp3_id  = new_expr (new AST.irexpression 
                          (TYP.Binary (binary_add, exp1_id , exp2_id) , loc)) in
  let exp4_id  = new_expr (new AST.irexpression 
                          (TYP.Binary (binary_mul, op1_h_id, op2_l_id), loc)) in
  let exp5_id  = new_expr (new AST.irexpression 
                          (TYP.Binary (binary_add, exp3_id , exp4_id) , loc)) in
  let exp6_id  = new_expr (new AST.irexpression 
                          (TYP.Binary (binary_mul, op1_l_id, op2_h_id), loc)) in
  let exp7_id  = new_expr (new AST.irexpression 
                          (TYP.Binary (binary_add, exp5_id , exp6_id) , loc)) in
  let exp8_id  = new_expr (new AST.irexpression 
                          (TYP.Binary (binary_mul, op1_l_id, op2_l_id), loc)) in
  let exp9_id  = new_expr (new AST.irexpression 
                          (TYP.Binary (binary_add, exp7_id , exp8_id) , loc)) in
  let exp10_id = new_expr (new AST.irexpression 
                       (TYP.Binary (binary_assign, twoprod , exp9_id) , loc)) in

  let stm0_id = new_stmt (new AST.irstatement 
                                (TYP.Computation exp_id               , loc)) in
  let stm1_id = new_stmt (new AST.irstatement 
                                (TYP.Computation exp10_id             , loc)) in
  let stm2_id = new_stmt (new AST.irstatement
                                (TYP.Sequence (stm_split_op2, stm1_id), loc)) in
  let stm3_id = new_stmt (new AST.irstatement
                                (TYP.Sequence (stm_split_op1, stm2_id), loc)) in
  let stm4_id = new_stmt (new AST.irstatement
                                (TYP.Sequence (stm0_id      , stm3_id), loc)) in
  stm4_id 


(** TwoProductFMA Algorithm 
    @param exp_id Multiplication expression id
    @param res_id Result variable id
    @param low_name Result low part name
    @param bool_name True to keep array index
    @param op1_id First operand id to multiply
    @param op2_id Second operand id to multiply
    @param scope Scope of variables to multiply
    @param loc Location of transformation *)
let twoProductFMA exp_id res_id low_name bool_name op1_id op2_id scope loc =
  let twoprod = PSC.low_exp res_id low_name scope bool_name in
 
  let neg_id   = new_expr (new AST.irexpression 
                                       (TYP.Unary (unary_neg, res_id), loc)) in
  let fn_id    = new_expr (new AST.irexpression (TYP.Variable ("fma"), loc)) in
  let args_ids = [op1_id; op2_id; neg_id] in
  let exp1_id  = new_expr (new AST.irexpression 
                                          (TYP.Call (fn_id, args_ids), loc)) in
  let exp2_id  = new_expr (new AST.irexpression 
                        (TYP.Binary (binary_assign, twoprod, exp1_id), loc)) in
 
  let stm1_id = new_stmt 
                      (new AST.irstatement (TYP.Computation (exp2_id), loc)) in
  let stm2_id = new_stmt 
                       (new AST.irstatement (TYP.Computation (exp_id), loc)) in
  let stm3_id = new_stmt 
                (new AST.irstatement (TYP.Sequence (stm2_id, stm1_id), loc)) in
  stm3_id 


(** FastTwoSum Algorithm 
    @param exp_id Addition expression id
    @param res_id Result variable id
    @param low_name Result low part name
    @param bool_name True to keep array index
    @param op1_id First operand id to add
    @param op2_id Second operand id to add
    @param scope Scope of variables to add
    @param loc Location of transformation *)
let fastTwoSum res_id low_name bool_name op1_id op2_id scope loc =
  let f2s_id  = PSC.low_exp res_id low_name scope bool_name in
  let var1_id = PSC.low_exp res_id "_fts" scope false in

  let exp6_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_assign, res_id , var1_id), loc)) in
  let exp5_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_add   , op1_id , op2_id) , loc)) in
  let exp4_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_assign, var1_id, exp5_id), loc)) in
  let exp3_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_sub   , var1_id, op1_id) , loc)) in
  let exp2_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_sub   , op2_id , exp3_id), loc)) in
  let exp1_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_assign, f2s_id , exp2_id), loc)) in

  let stm1_id = new_stmt (new AST.irstatement 
                                      (TYP.Computation exp4_id        , loc)) in
  let stm2_id = new_stmt (new AST.irstatement 
                                      (TYP.Computation exp1_id        , loc)) in
  let stm3_id = new_stmt (new AST.irstatement 
                                      (TYP.Computation exp6_id        , loc)) in
  let seq2_id = new_stmt (new AST.irstatement 
                                      (TYP.Sequence (stm2_id, stm3_id), loc)) in
  let seq1_id = new_stmt (new AST.irstatement 
                                      (TYP.Sequence (stm1_id, seq2_id), loc)) in
  seq1_id


(** TwoSum Algorithm 
    @param exp_id Addition expression id
    @param res_id Result variable id
    @param low_name Result low part name
    @param bool_name True to keep array index
    @param op1_id First operand id to add
    @param op2_id Second operand id to add
    @param scope Scope of variables to add
    @param loc Location of transformation *)
let twoSum exp_id res_id low_name bool_name op1_id op2_id scope loc =
  let twosum  = PSC.low_exp res_id low_name scope bool_name in
  let var1_id = PSC.low_exp res_id "_ts1" scope false in
  let var2_id = PSC.low_exp res_id "_ts2" scope false in

  let exp1_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_sub   , res_id ,  op2_id), loc)) in
  let exp2_id = new_expr (new AST.irexpression
                         (TYP.Binary (binary_assign, var1_id, exp1_id), loc)) in
  let exp3_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_sub   , res_id , var1_id), loc)) in
  let exp4_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_assign, var2_id, exp3_id), loc)) in
  let exp5_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_sub   , op1_id , var1_id), loc)) in
  let exp6_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_sub   , op2_id , var2_id), loc)) in
  let exp7_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_add   , exp5_id, exp6_id), loc)) in
  let exp8_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_assign, twosum , exp7_id), loc)) in

  let stm0_id = new_stmt (new AST.irstatement
                                      (TYP.Computation exp_id         , loc)) in
  let stm1_id = new_stmt (new AST.irstatement
                                      (TYP.Computation exp8_id        , loc)) in
  let stm2_id = new_stmt (new AST.irstatement
                                      (TYP.Computation exp4_id        , loc)) in
  let stm3_id = new_stmt (new AST.irstatement
                                      (TYP.Computation exp2_id        , loc)) in
  let stm4_id = new_stmt (new AST.irstatement
                                      (TYP.Sequence (stm2_id, stm1_id), loc)) in
  let stm5_id = new_stmt (new AST.irstatement
                                      (TYP.Sequence (stm3_id, stm4_id), loc)) in
  let stm6_id = new_stmt (new AST.irstatement
                                      (TYP.Sequence (stm0_id, stm5_id), loc)) in
  stm6_id 



(******************************************************************************)

(** Double-word operators library *)


(** Sum of two double-word numbers 
    @param exp_id Addition expression id
    @param res_id Result variable id
    @param op1_id First operand variable id
    @param op2_id Second operand variable id
    @param scope Scope of variables
    @param loc Location of transformation *)
let sum_dw_dw exp_id res_id op1_id op2_id scope loc =
  let res_low = PSC.low_exp res_id "_low" scope true in
  let op1_low = PSC.low_exp op1_id "_low" scope true in
  let op2_low = PSC.low_exp op2_id "_low" scope true in

  let rec exp_low exp =
    match (AST.find_tbl AST.expression_tbl exp)#get_type with
    | TYP.Exp e               -> exp_low e
    | TYP.Binary (op, e1, e2) ->
      new_expr (new AST.irexpression 
                                 (TYP.Binary (op, exp_low e1, exp_low e2), loc))
    | _                       -> PSC.low_exp exp "_low" scope true
  in
  let exp_id_low = exp_low exp_id in

  let res_twosum1  = PSC.low_exp res_id  "_twosum"  scope false in
  let res_twosum2  = PSC.low_exp res_low "_twosum"  scope false in
  let res_fastsum1 = PSC.low_exp res_id  "_fastsum" scope false in

  let twosum1  = twoSum exp_id res_id "_twosum" false op1_id op2_id scope loc in
  let twosum2  = 
          twoSum exp_id_low res_low "_twosum" false op1_low op2_low scope loc in
  let add1_id  = PSC.low_exp res_id "_dd1" scope false in
  let fastsum1 = fastTwoSum  res_id "_fastsum" false res_id add1_id scope loc in
  let add2_id  = PSC.low_exp res_id "_dd2" scope false in
  let fastsum2 = fastTwoSum  res_id "_low" true res_id add2_id scope loc in


  let exp3_id = new_expr (new AST.irexpression 
                        (TYP.Binary (binary_add, res_twosum1, res_low), loc)) in
  let exp4_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_assign, add1_id, exp3_id), loc)) in
  let exp7_id = new_expr (new AST.irexpression 
                   (TYP.Binary (binary_add, res_fastsum1, res_twosum2), loc)) in
  let exp8_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_assign, add2_id, exp7_id), loc)) in

  let stm4_id = 
              new_stmt (new AST.irstatement (TYP.Computation (exp8_id), loc)) in
  let stm2_id = 
              new_stmt (new AST.irstatement (TYP.Computation (exp4_id), loc)) in
  let seq1_id = 
      new_stmt (new AST.irstatement (TYP.Sequence (stm4_id , fastsum2), loc)) in
  let seq2_id = 
      new_stmt (new AST.irstatement (TYP.Sequence (fastsum1, seq1_id) , loc)) in
  let seq3_id = 
      new_stmt (new AST.irstatement (TYP.Sequence (stm2_id , seq2_id) , loc)) in
  let seq4_id = 
      new_stmt (new AST.irstatement (TYP.Sequence (twosum2 , seq3_id) , loc)) in
  let seq5_id =
      new_stmt (new AST.irstatement (TYP.Sequence (twosum1 , seq4_id) , loc)) in

  seq5_id


(** Sum of a double-word number and a floating-point number 
    @param exp_id Addition expression id
    @param res_id Result variable id
    @param op1_id First operand variable id
    @param op2_id Second operand variable id
    @param scope Scope of variables
    @param loc Location of transformation *)
let sum_dw_w exp_id res_id op1_id op2_id scope loc = 
  let op1_low    = PSC.low_exp op1_id "_low"    scope true in
  let res_twosum = PSC.low_exp res_id "_twosum" scope true in

  let twosum  = twoSum exp_id res_id "_twosum" true op1_id op2_id scope loc in
  let add1    = PSC.low_exp res_id "_dd1" scope true in
  let fastsum = fastTwoSum res_id "_low" true res_id add1 scope loc in
  
  let exp1_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_add, res_twosum, op1_low), loc)) in
  let exp2_id = new_expr (new AST.irexpression 
                            (TYP.Binary (binary_assign, add1, exp1_id), loc)) in

  let stm1_id = new_stmt 
                       (new AST.irstatement (TYP.Computation (exp2_id), loc)) in
  let seq1_id = new_stmt 
                 (new AST.irstatement (TYP.Sequence (stm1_id, fastsum), loc)) in
  let seq2_id = new_stmt 
                  (new AST.irstatement (TYP.Sequence (twosum, seq1_id), loc)) in

   seq2_id


(** Product of two double-word numbers 
    @param exp_id Addition expression id
    @param res_id Result variable id
    @param op1_id First operand variable id
    @param op2_id Second operand variable id
    @param scope Scope of variables
    @param loc Location of transformation *)
let product_dw_dw exp_id res_id op1_id op2_id scope loc =
  let stm_twoproduct = 
    if !CMD.fma then
      twoProductFMA exp_id res_id "_twoprod" false op1_id op2_id scope loc
    else 
      twoProduct    exp_id res_id "_twoprod" false op1_id op2_id scope loc
  in

  let var1_id = PSC.low_exp res_id "_twoprod" scope false in
  let var2_id = PSC.low_exp res_id "_dd1"     scope false in
  let var3_id = PSC.low_exp op1_id "_low"     scope true in
  let var4_id = PSC.low_exp op2_id "_low"     scope true in

  let exp1_id = new_expr 
       (new AST.irexpression (TYP.Binary (binary_mul, var4_id, op1_id), loc)) in
  let exp2_id = new_expr 
       (new AST.irexpression (TYP.Binary (binary_mul, var3_id, op2_id), loc)) in
  let exp3_id = new_expr 
      (new AST.irexpression (TYP.Binary (binary_add, exp2_id, exp1_id), loc)) in
  let exp4_id = new_expr 
      (new AST.irexpression (TYP.Binary (binary_add, var1_id, exp3_id), loc)) in
  let exp5_id = new_expr 
   (new AST.irexpression (TYP.Binary (binary_assign, var2_id, exp4_id), loc)) in

  let stm_fastsum = fastTwoSum res_id "_low" true res_id var2_id scope loc in

  let stm1_id = new_stmt 
                       (new AST.irstatement (TYP.Computation (exp5_id), loc)) in
  let stm2_id = new_stmt 
             (new AST.irstatement (TYP.Sequence (stm1_id, stm_fastsum), loc)) in
  let stm3_id = new_stmt 
          (new AST.irstatement (TYP.Sequence (stm_twoproduct, stm2_id), loc)) in

  stm3_id 


(** Product of a double-word number and a floating-point number 
    @param exp_id Addition expression id
    @param res_id Result variable id
    @param op1_id First operand variable id
    @param op2_id Second operand variable id
    @param scope Scope of variables
    @param loc Location of transformation *)
let product_dw_w exp_id res_id op1_id op2_id scope loc =
  let stm_twoproduct = 
    if !CMD.fma then
      twoProductFMA exp_id res_id "_twoprod" false op1_id op2_id scope loc
    else 
      twoProduct    exp_id res_id "_twoprod" false op1_id op2_id scope loc
  in

  let var1_id = PSC.low_exp res_id "_twoprod" scope false in
  let var2_id = PSC.low_exp res_id "_dd1"     scope false in
  let var3_id = PSC.low_exp op1_id "_low"     scope true in

  let exp1_id = new_expr 
       (new AST.irexpression (TYP.Binary (binary_mul, var3_id, op2_id), loc)) in
  let exp2_id = new_expr 
      (new AST.irexpression (TYP.Binary (binary_add, var1_id, exp1_id), loc)) in
  let exp3_id = new_expr 
   (new AST.irexpression (TYP.Binary (binary_assign, var2_id, exp2_id), loc)) in

  let stm_fastsum = fastTwoSum res_id "_low" true res_id var2_id scope loc in

  let stm1_id = new_stmt 
                       (new AST.irstatement (TYP.Computation (exp3_id), loc)) in
  let stm2_id = new_stmt 
             (new AST.irstatement (TYP.Sequence (stm1_id, stm_fastsum), loc)) in
  let stm3_id = new_stmt 
          (new AST.irstatement (TYP.Sequence (stm_twoproduct, stm2_id), loc)) in

  stm3_id 





(******************************************************************************)

(** Comp-word operators library *)


(** Sum of two floating-point numbers 
    @param exp_id Addition expression id
    @param res_id Result variable id
    @param op1_id First operand variable id
    @param op2_id Second operand variable id
    @param scope Scope of variables
    @param loc Location of transformation *)
let sum_w_w2 exp_id res_id op1_id op2_id scope loc =
  twoSum exp_id res_id "_low" true op1_id op2_id  scope loc

let sum_w_w_nop exp_id res_id op1_id op2_id scope loc =
  if !CMD.to_prop then begin (* case of error propagation *)
    let var1_id = PSC.low_exp res_id "_low" scope true in
    let var2_id = float_cnst_expr "0.0" loc in

    let exp1_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_assign, var1_id, var2_id), loc)) in

    let stm0_id = new_stmt 
                        (new AST.irstatement (TYP.Computation (exp_id), loc)) in
    let stm1_id = new_stmt 
                       (new AST.irstatement (TYP.Computation (exp1_id), loc)) in
    let stm2_id = new_stmt 
                 (new AST.irstatement (TYP.Sequence (stm0_id, stm1_id), loc)) in
    stm2_id 
  end 
  else
    new_stmt (new AST.irstatement (TYP.Computation (exp_id), loc))

let sum_w_w exp_id res_id op1_id op2_id scope loc =
  begin match (!nop, !CMD.double_double) with
  | (true, false)  -> sum_w_w_nop exp_id res_id op1_id op2_id scope loc
  | (false, false) -> sum_w_w2 exp_id res_id op1_id op2_id scope loc
  | (_, true)      -> sum_w_w2 exp_id res_id op1_id op2_id scope loc
  end


(** Sum of a comp-word number and a floating-point number 
    @param exp_id Addition expression id
    @param res_id Result variable id
    @param op1_id First operand variable id
    @param op2_id Second operand variable id
    @param scope Scope of variables
    @param loc Location of transformation *)
let sum_hw_w2 exp_id res_id op1_id op2_id scope loc =
  let stm_twosum = 
                 twoSum exp_id res_id "_twosum" false op1_id op2_id scope loc in

  let var1_id = PSC.low_exp res_id "_twosum" scope false in
  let var2_id = PSC.low_exp res_id "_low" scope true in
  let var3_id = PSC.low_exp op1_id "_low" scope true in

  let exp1_id = new_expr (new AST.irexpression 
                            (TYP.Binary (binary_add, var1_id, var3_id), loc)) in
  let exp2_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_assign, var2_id, exp1_id), loc)) in

  let stm1_id = new_stmt 
                       (new AST.irstatement (TYP.Computation (exp2_id), loc)) in
  let stm2_id = new_stmt 
              (new AST.irstatement (TYP.Sequence (stm_twosum, stm1_id), loc)) in

  stm2_id 

let sum_hw_w_nop exp_id res_id op1_id op2_id scope loc =
  let var1_id = PSC.low_exp res_id "_low" scope true in
  let var2_id = PSC.low_exp op1_id "_low" scope true in

  if !CMD.to_prop then begin (* case of error propagation *)

    let exp1_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_assign, var1_id, var2_id), loc)) in

    let stm0_id = new_stmt 
                        (new AST.irstatement (TYP.Computation (exp_id), loc)) in
    let stm1_id = new_stmt 
                       (new AST.irstatement (TYP.Computation (exp1_id), loc)) in
    let stm2_id = new_stmt 
                 (new AST.irstatement (TYP.Sequence (stm0_id, stm1_id), loc)) in

    stm2_id 
    end
  else
    begin
    let stm_res = new_stmt 
                        (new AST.irstatement (TYP.Computation (exp_id), loc)) in
    close_sequence stm_res op1_id scope loc;
    stm_res
    end

let sum_hw_w exp_id res_id op1_id op2_id scope loc =
  begin match (!nop, !CMD.double_double) with
  | (true, false)  -> sum_hw_w_nop exp_id res_id op1_id op2_id scope loc
  | (false, false) -> sum_hw_w2 exp_id res_id op1_id op2_id scope loc
  | (_, true)      -> sum_dw_w exp_id res_id op1_id op2_id scope loc
  end


(** Sum of two comp-word numbers 
    @param exp_id Addition expression id
    @param res_id Result variable id
    @param op1_id First operand variable id
    @param op2_id Second operand variable id
    @param scope Scope of variables
    @param loc Location of transformation *)
let sum_hw_hw2 exp_id res_id op1_id op2_id scope loc =
  let stm_twosum = 
                 twoSum exp_id res_id "_twosum" false op1_id op2_id scope loc in

  let var1_id = PSC.low_exp res_id "_twosum" scope false in
  let var2_id = PSC.low_exp res_id "_low" scope true in
  let var3_id = PSC.low_exp op1_id "_low" scope true in
  let var4_id = PSC.low_exp op2_id "_low" scope true in

  let exp3_id = new_expr (new AST.irexpression
                            (TYP.Binary (binary_add, var3_id, var4_id), loc)) in
  let exp2_id = new_expr (new AST.irexpression 
                            (TYP.Binary (binary_add, var1_id, exp3_id), loc)) in
  let exp3_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_assign, var2_id, exp2_id), loc)) in

  let stm1_id = new_stmt 
                       (new AST.irstatement (TYP.Computation (exp3_id), loc)) in
  let stm2_id = new_stmt 
              (new AST.irstatement (TYP.Sequence (stm_twosum, stm1_id), loc)) in

  stm2_id 

let sum_hw_hw_nop exp_id res_id op1_id op2_id scope loc =
  let var1_id = PSC.low_exp res_id "_low" scope true in
  let var2_id = PSC.low_exp op1_id "_low" scope true in
  let var3_id = PSC.low_exp op2_id "_low" scope true in

  if !CMD.to_prop then begin (* case of error propagation *)

    let exp1_id = new_expr (new AST.irexpression 
                            (TYP.Binary (binary_add, var2_id, var3_id), loc)) in
    let exp2_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_assign, var1_id, exp1_id), loc)) in

    let stm0_id = new_stmt 
                        (new AST.irstatement (TYP.Computation (exp_id), loc)) in
    let stm1_id = new_stmt 
                       (new AST.irstatement (TYP.Computation (exp2_id), loc)) in
    let stm2_id = new_stmt 
                 (new AST.irstatement (TYP.Sequence (stm0_id, stm1_id), loc)) in

    stm2_id 
    end
  else
    begin
    let stm_res = new_stmt 
                        (new AST.irstatement (TYP.Computation (exp_id), loc)) in
    close_sequence stm_res op1_id scope loc;
    close_sequence stm_res op2_id scope loc;
    stm_res
    end

let sum_hw_hw exp_id res_id op1_id op2_id scope loc =
  begin match (!nop, !CMD.double_double) with
  | (true, false)  -> sum_hw_hw_nop exp_id res_id op1_id op2_id scope loc
  | (false, false) -> sum_hw_hw2 exp_id res_id op1_id op2_id scope loc
  | (_, true)      -> sum_dw_dw exp_id res_id op1_id op2_id scope loc
  end




(** Product of two floating-point numbers 
    @param exp_id Product expression id
    @param res_id Result variable id
    @param op1_id First operand variable id
    @param op2_id Second operand variable id
    @param scope Scope of variables
    @param loc Location of transformation *)
let product_w_w2 exp_id res_id op1_id op2_id scope loc =
  if !CMD.fma then
    twoProductFMA exp_id res_id "_low" true op1_id op2_id scope loc
  else
    twoProduct exp_id res_id "_low" true op1_id op2_id scope loc

let product_w_w_nop exp_id res_id op1_id op2_id scope loc =
  sum_w_w_nop exp_id res_id op1_id op2_id scope loc

let product_w_w exp_id res_id op1_id op2_id scope loc =
  begin match (!nop, !CMD.double_double) with
  | (true, false)  -> product_w_w_nop exp_id res_id op1_id op2_id scope loc
  | (false, false) -> product_w_w2 exp_id res_id op1_id op2_id scope loc
  | (_, true)      -> product_w_w2 exp_id res_id op1_id op2_id scope loc
  end


(** Product of a comp-word number and a floating-point number 
    @param exp_id Product expression id
    @param res_id Result variable id
    @param op1_id First operand variable id
    @param op2_id Second operand variable id
    @param scope Scope of variables
    @param loc Location of transformation *)
let product_hw_w2 exp_id res_id op1_id op2_id scope loc =
  let stm_twoproduct = 
    if !CMD.fma then
      twoProductFMA exp_id res_id "_twoprod" false op1_id op2_id scope loc
    else 
      twoProduct exp_id res_id "_twoprod" false op1_id op2_id scope loc
  in

  let var1_id = PSC.low_exp res_id "_twoprod" scope false in
  let var2_id = PSC.low_exp res_id "_low" scope false in
  let var3_id = PSC.low_exp op1_id "_low" scope true in

  let exp1_id = new_expr (new AST.irexpression 
                             (TYP.Binary (binary_mul, var3_id, op2_id), loc)) in
  let exp2_id = new_expr (new AST.irexpression 
                            (TYP.Binary (binary_add, var1_id, exp1_id), loc)) in
  let exp3_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_assign, var2_id, exp2_id), loc)) in

  let stm1_id = new_stmt 
                       (new AST.irstatement (TYP.Computation (exp3_id), loc)) in
  let stm2_id = new_stmt 
          (new AST.irstatement (TYP.Sequence (stm_twoproduct, stm1_id), loc)) in

  stm2_id 

let product_hw_w_nop exp_id res_id op1_id op2_id scope loc =
  let var1_id = PSC.low_exp res_id "_low" scope false in
  let var2_id = PSC.low_exp op1_id "_low" scope true in

  if !CMD.to_prop then begin (* case of error propagation *)
    let exp1_id = new_expr (new AST.irexpression 
                             (TYP.Binary (binary_mul, var2_id, op2_id), loc)) in
    let exp2_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_assign, var1_id, exp1_id), loc)) in

    let stm0_id = new_stmt 
                        (new AST.irstatement (TYP.Computation (exp_id), loc)) in
    let stm1_id = new_stmt 
                       (new AST.irstatement (TYP.Computation (exp2_id), loc)) in
    let stm2_id = new_stmt 
                 (new AST.irstatement (TYP.Sequence (stm0_id, stm1_id), loc)) in

    stm2_id
    end 
  else 
    begin
    let stm_res = new_stmt 
                        (new AST.irstatement (TYP.Computation (exp_id), loc)) in
    close_sequence stm_res op1_id scope loc;
    stm_res
    end

let product_hw_w exp_id res_id op1_id op2_id scope loc =
  begin match (!nop, !CMD.double_double) with
  | (true, false)  -> product_hw_w_nop exp_id res_id op1_id op2_id scope loc
  | (false, false) -> product_hw_w2 exp_id res_id op1_id op2_id scope loc
  | (_, true)      -> product_dw_w exp_id res_id op1_id op2_id scope loc
  end


(** Product of two comp-word numbers 
    @param exp_id Product expression id
    @param res_id Result variable id
    @param op1_id First operand variable id
    @param op2_id Second operand variable id
    @param scope Scope of variables
    @param loc Location of transformation *)
let product_hw_hw2 exp_id res_id op1_id op2_id scope loc =
  let stm_twoproduct = 
    if !CMD.fma then
      twoProductFMA exp_id res_id "_twoprod" false op1_id op2_id scope loc
    else 
      twoProduct exp_id res_id "_twoprod" false op1_id op2_id scope loc
  in

  let var1_id = PSC.low_exp res_id "_twoprod" scope false in
  let var2_id = PSC.low_exp res_id "_low" scope false in
  let var3_id = PSC.low_exp op1_id "_low" scope true in
  let var4_id = PSC.low_exp op2_id "_low" scope true in

  let exp1_id = new_expr (new AST.irexpression 
                             (TYP.Binary (binary_mul, var4_id, op1_id), loc)) in
  let exp2_id = new_expr (new AST.irexpression 
                             (TYP.Binary (binary_mul, var3_id, op2_id), loc)) in
  let exp3_id = new_expr (new AST.irexpression 
                            (TYP.Binary (binary_add, exp2_id, exp1_id), loc)) in
  let exp4_id = new_expr (new AST.irexpression 
                            (TYP.Binary (binary_add, var1_id, exp3_id), loc)) in
  let exp5_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_assign, var2_id, exp4_id), loc)) in

  let stm1_id = new_stmt 
                       (new AST.irstatement (TYP.Computation (exp5_id), loc)) in
  let stm2_id = new_stmt 
          (new AST.irstatement (TYP.Sequence (stm_twoproduct, stm1_id), loc)) in

  stm2_id 

let product_hw_hw_nop exp_id res_id op1_id op2_id scope loc =
  let var1_id = PSC.low_exp res_id "_low" scope false in
  let var2_id = PSC.low_exp op1_id "_low" scope true in
  let var3_id = PSC.low_exp op2_id "_low" scope true in

  if !CMD.to_prop then begin (* case of error propagation *)

    let exp1_id = new_expr (new AST.irexpression 
                             (TYP.Binary (binary_mul, op1_id, var3_id), loc)) in
    let exp2_id = new_expr (new AST.irexpression 
                             (TYP.Binary (binary_mul, op2_id, var2_id), loc)) in
    let exp3_id = new_expr (new AST.irexpression 
                            (TYP.Binary (binary_add, exp1_id, exp2_id), loc)) in
    let exp4_id = new_expr (new AST.irexpression 
                         (TYP.Binary (binary_assign, var1_id, exp3_id), loc)) in

    let stm0_id = new_stmt 
                        (new AST.irstatement (TYP.Computation (exp_id), loc)) in
    let stm1_id = new_stmt 
                       (new AST.irstatement (TYP.Computation (exp4_id), loc)) in
    let stm2_id = new_stmt 
                 (new AST.irstatement (TYP.Sequence (stm0_id, stm1_id), loc)) in
  
    stm2_id
    end 
  else 
    begin
    let stm_res = new_stmt 
                        (new AST.irstatement (TYP.Computation (exp_id), loc)) in
    close_sequence stm_res op1_id scope loc;
    close_sequence stm_res op2_id scope loc;
    stm_res
    end

let product_hw_hw exp_id res_id op1_id op2_id scope loc =
  begin match (!nop, !CMD.double_double) with
  | (true, false)  -> product_hw_hw_nop exp_id res_id op1_id op2_id scope loc
  | (false, false) -> product_hw_hw2 exp_id res_id op1_id op2_id scope loc
  | (_, true)      -> product_dw_dw exp_id res_id op1_id op2_id scope loc
  end
