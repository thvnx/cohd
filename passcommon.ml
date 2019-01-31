open Libcommon
open Astcommon

module TYP = Types
module AST = Irtree

exception Variable_found
exception Variable_not_found
exception Statement_found of string
exception Statement_not_found



(******************************************************************************)
(******************************************************************************)
(** Hashtables stuff **********************************************************)
(******************************************************************************)
(******************************************************************************)

let variable_tbl:((string, (string * AST.irvariable)) Hashtbl.t) = 
  Hashtbl.create 0 (*** (key, (scope * variable object)) *)

let statement_tbl:((string, (string * AST.irstatement)) Hashtbl.t) = 
  Hashtbl.create 0 (*** (key, (scope * statement object)) *)

let function_body_tbl:((string, AST.irbody) Hashtbl.t) = 
  Hashtbl.create 0 (*** (scope * body object) *)


let clear_tbl () =
  Hashtbl.clear variable_tbl;
  Hashtbl.clear statement_tbl;
  Hashtbl.clear function_body_tbl


(******************************************************************************)
(******************************************************************************)
(** AST searchs ***************************************************************)
(******************************************************************************)
(******************************************************************************)


(* Get the statement id of line_number given by the parameter. 
   If there is no statement at the given line number, search at the following line. *)
let rec get_statement_id_by_line_number line_number =
  try 
  begin
    Hashtbl.iter (
      fun id stmt ->
        if (stmt#get_line_number = line_number) then raise (Statement_found id)
    ) AST.statement_tbl;
    raise Statement_not_found
  end
  with 
  | Statement_found (stm) -> stm 
  | Statement_not_found   -> 
    if line_number < !idx 
      then get_statement_id_by_line_number (line_number + 1)
      else failwith ("No statement after line number : " ^ (string_of_int line_number))


(* Get the statement id of line_number given by the parameter. 
   If there is no statement at the given line number, search at the following line. *)
let rec get_statement_blk_id_by_line_number line_number =
  try 
  begin
    Hashtbl.iter (
      fun id stmt ->
        if (stmt#get_line_number = line_number && stmt#get_type = "block") then raise (Statement_found id)
    ) AST.statement_tbl;
    raise Statement_not_found
  end
  with 
  | Statement_found (stm) -> stm 
  | Statement_not_found   -> 
    if line_number < !idx 
      then get_statement_blk_id_by_line_number (line_number + 1)
      else failwith ("No block statement after line number : " ^ (string_of_int line_number))



(** Return true if the variable name is a floating point variable 
 @param var string name of the variable 
 @param scp string name of the scope variable *)
let is_floating_point var scp =
  try (
    let check_variable var var_name =
      if var#get_name = var_name then 
        begin
          let variable_type = var#type_to_string () in
	  let type_list = ["float"; "float[]"; "double"; "double[]"; "long double"; 
                            "long double[]"; "float*"; "double*"; "long double*"] in
          if (List.mem variable_type type_list) then
            raise Variable_found
	end 
    in 
    Hashtbl.iter (
      fun key (scope, variable) ->
      if scope = scp then (* variable lives in a function *)
        check_variable variable var
      else begin (* maybe it lives at global *)
        if scope = "globals" then 
          check_variable variable var
      end
    ) variable_tbl;
    false
  ) with Variable_found -> true



(** Build the variable hash table
 @param dcl an irdefinition object list
 @param scope current scope of the variable definition *)
let buildVariableHashTable (dcl:AST.irdefinition list) (scope:string) =
  let rec add_def_list dfl scp =
  List.iter (fun i -> 
   match i#get_definition with 
    | TYP.Declaration id -> 
      List.iter ( fun i -> 
        let var = AST.find_tbl AST.variable_tbl i in
        Hashtbl.add variable_tbl i (scp, var)
      ) (AST.find_tbl AST.namegroup_tbl id)#get_names
    | TYP.Function    id -> 
      let func = AST.find_tbl AST.function_tbl id in
      let body = AST.find_tbl AST.body_tbl (func#get_body) in
      add_def_list body#get_body_dcl func#get_name
    | _ -> ()
  ) dfl
  in 
  add_def_list dcl scope;
  (* adding arguments of functions *)
  Hashtbl.iter (
    fun key pro ->
      List.iter (
        fun i ->
          let id = i#get_name in
	  let var = AST.find_tbl AST.variable_tbl id in
	  let scp = pro#get_function_name in
          Hashtbl.add variable_tbl id (scp, var)
      ) pro#get_argument_list;
  ) AST.proto_tbl




(** Build the statement hash table and body table
 statement must be a computation statement with a floating-point return type 
 @param dcl an irdefinition object list *)
let buildFPStatementHashTable (dcl:AST.irdefinition list) =
  let rec check_type exp scope =
    match (AST.find_tbl AST.expression_tbl exp)#get_type with
    | TYP.Constant c -> 
      begin match (AST.find_tbl AST.constant_tbl c)#get_const with
      | TYP.CFloat _ -> true
      | _ -> false
      end
    | TYP.Variable v -> is_floating_point v scope
    | TYP.Index (e, _) | TYP.Exp e | TYP.Unary (_,e) ->
      check_type e scope
    | TYP.Binary (_,e1,e2) -> (check_type e1 scope) || (check_type e2 scope)
    | TYP.Call (_,le) -> 
      let res = ref false in
      List.iter (
        fun i ->
        if check_type i scope then res := true
      ) le;
      !res
    | _ -> false
  in
  let rec add_stmt stm scope =
  let stmt = (AST.find_tbl AST.statement_tbl stm) in
  match stmt#get_stmt_type with
  | TYP.Sequence (s1, s2) | TYP.If (_, s1, s2) -> add_stmt s1 scope; add_stmt s2 scope
  | TYP.While (_, s) | TYP.DoWhile (_, s) | TYP.For (_, _, _, s) | TYP.Switch (_, s)
    | TYP.Case (_, s) | TYP.Default s | TYP.Label (_, s) | TYP.Stat s -> add_stmt s scope
  | TYP.Block b -> 
    let body = AST.find_tbl AST.body_tbl b in
    add_stmt body#get_body_stm scope
  | TYP.Computation e 
  | TYP.Return      e -> 
    if check_type e scope then Hashtbl.add statement_tbl stm (scope, stmt)
    else ()
  | _ -> ()
  in
  let add_body body scope =
    Hashtbl.add function_body_tbl scope (AST.find_tbl AST.body_tbl body)
  in
  List.iter (fun i -> 
  match i#get_definition with 
    | TYP.Function    id -> 
      let func = AST.find_tbl AST.function_tbl id in
      let body = AST.find_tbl AST.body_tbl (func#get_body) in
      add_body func#get_body func#get_name;
      add_stmt body#get_body_stm func#get_name
    | _ -> ()
  ) dcl









(******************************************************************************)
(******************************************************************************)
(** others ********************************************************************)
(******************************************************************************)
(******************************************************************************)


let rec get_return_type exp scope =
  let loc = TYP.Location (scope, -1) in 
  let expression = AST.find_tbl AST.expression_tbl exp in
  match expression#get_type with
  | TYP.Constant c -> 
    let typ_id = new_id idx in 
      AST.add_type typ_id (new AST.irtype(TYP.Double (false), loc)); 
    typ_id

  | TYP.Variable v -> 
    begin
    let res = ref "" in
    try (
      let check_variable var var_name =
        if var#get_name = var_name then 
        begin
          res := var#get_type;
	  raise Variable_found
	end 
      in 
      Hashtbl.iter (
        fun key (scopee, variable) ->
        if scopee = scope then 
          check_variable variable v
        else begin 
          if scopee = "globals" then 
            check_variable variable v
        end
      ) variable_tbl;
      raise Variable_not_found
    ) with 
      | Variable_found     -> !res
      | Variable_not_found ->
       begin
         let typ_id = new_id idx in
           AST.add_type typ_id (new AST.irtype(TYP.Double (false), loc));
         typ_id
      end
    end 

  | TYP.Index (e, _) 
  | TYP.Exp e 
  | TYP.Unary (_,e) 
  | TYP.Binary (_,e,_) -> get_return_type e scope

  | TYP.SizeofType _ 
  | TYP.SizeofExpr _ -> 
    let typ_id = new_id idx in
      AST.add_type typ_id (new AST.irtype(TYP.Int ("", ""), loc));
    typ_id

  | _ -> 
    warnwith ("get_return_type : " ^ expression#to_string ());
    let typ_id = new_id idx in 
      AST.add_type typ_id (new AST.irtype(TYP.Int ("", ""), loc)); 
    typ_id


let added_variables = ref []

let add_variable name exp scope bool_array =
  (* If the variable has already added then do nothing, else : *)
  if not (List.mem (name ^ scope) !added_variables )
  then (* Add variable to the AST variable list *)
  begin
    added_variables := !added_variables @ [(name ^ scope)];


    (* Determine the type variable *)(*pas net ça encore*)
    let typ_id =
    if bool_array then (* If added variable is an array *)
    begin (* Return the variable type *)
      get_return_type exp scope
    end
    else (* Return the array type *)
    begin
      let typ = (get_return_type exp scope) in
      let rec pp tt =
      begin try ( 
      match (AST.find_tbl AST.type_tbl tt)#get_type with
      | TYP.Array (_,t) -> pp t
      | _ -> tt  
      ) with Not_found -> failwith (name^"ici"^tt)
      end
      in pp typ
    end
    in


    (* Add the variable of the right declaration list *)
    let var_id = new_id idx in
      AST.add_variable var_id (new AST.irvariable(name, typ_id, nop_expr));
    let nmg_id = new_id idx in  
      AST.add_namegroup nmg_id (new AST.irnamegroup(typ_id, "", [var_id]));
    let dec = new AST.irdefinition(TYP.Declaration (nmg_id)) in
    let body = Hashtbl.find function_body_tbl scope in
    body#add_body_dcl (dec)
  end 













(** get a new expression 
 @param exp identifier of original expression
 @param suffix string to add at the end of the left part expression name 
 @param scope scope of the expression 
 @param bool_table if false then left part of expression is a temporary scalar variable
 @return id of the new expression *)

let rec low_exp exp suffix scope bool_table = 
  let loc = TYP.Location (scope, -1) in 
  let exp_id = new_id idx in
  let expression = AST.find_tbl AST.expression_tbl exp in
  begin match expression#get_type with
  | TYP.Constant c   -> 
    (* If expression is a constant then return the following expression :
       a variable named -> "_" ^ <constant name> ^ suffix *)
    let str = (Libcommon.replace_character (
      (AST.find_tbl AST.constant_tbl c)#to_string ()) '.' '_') in
    let name = "_" ^ str ^ suffix in
    AST.add_expression exp_id (new AST.irexpression (TYP.Variable (name), loc));
    add_variable name exp scope false

  | TYP.Variable v   -> 
    (* If expression is a variable then return the following expression : 
       a variable named -> <variable name> ^ suffix *)
    let name = v ^ suffix in
    AST.add_expression exp_id (new AST.irexpression (TYP.Variable (name), loc));
    add_variable name exp scope bool_table

  | TYP.Index (t, i)  -> 

    if bool_table then
      AST.add_expression exp_id (new AST.irexpression (TYP.Index ((low_exp t suffix scope bool_table),i), loc))
    else
      AST.add_expression exp_id (new AST.irexpression (TYP.Exp (low_exp t suffix scope bool_table), loc))
  | TYP.Unary (op,e) ->
      AST.add_expression exp_id (new AST.irexpression (TYP.Unary (op, (low_exp e suffix scope bool_table)), loc));
  | _                -> failwith ("Pattern matching missing here 323 : " ^ expression#to_string ())
  end;
  exp_id





let get_var_name exp =
  let expr = AST.find_tbl AST.expression_tbl exp in
  match expr#get_type with
  | TYP.Constant _ 
  | TYP.Variable _
  | TYP.Index _ -> expr#to_string ()
  | _ -> failwith (expr#to_string () ^ " get_var_name")




(* let rec exp_name exp = *)
(*   let expression = (AST.find_tbl AST.expression_tbl exp) in *)
(*   begin match expression#get_type with *)
(*   | TYP.Exp e       -> exp_name e *)
(*   | TYP.Index (t,i) -> exp_name t *)
(*   | _               -> expression#to_string ()     *)
(*   end *)

