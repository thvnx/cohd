(******************************************************************************)
(* History *********************************************************************

- September 25 2013 - revision 1.0 : first revision


******************************************************************** /History *)
(******************************************************************************)

(** Common Abstract Syntax Tree manipulation. *)

open Libcommon
module TYP = Types
module AST = Irtree

(******************************************************************************)

(** Widely used operators. *)

let binary_assign = ("=", false)
let binary_add    = ("+", true)
let binary_sub    = ("-", true)
let binary_mul    = ("*", true)
let binary_div    = ("/", true)
let unary_neg     = ("-", false)


(******************************************************************************)

(** Adders *)

(** Add a constant to the AST 
    @param cnst Constant to add 
    @return Id of the constant *)
let new_cnst cnst =
  let cst = new_id idx in AST.add_constant cst cnst;
  cst

(** Add an expression to the AST 
    @param expr Expression to add 
    @return Id of the expression *)
let new_expr expr =
  let exp = new_id idx in AST.add_expression exp expr;
  exp

(** Add a statement to the AST 
    @param stmt Statement to add 
    @return Id of the statement *)
let new_stmt stmt =
  let stm = new_id idx in AST.add_statement stm stmt;
  stm


(******************************************************************************)

(** Specific nodes. *)

                                                           (** Constant nodes *)

(** An integer constant 
    @param integer string of the integer *)
let int_cnst integer =
  new_cnst (new AST.irconstant(TYP.CInt integer))

(** A floating-point constant 
    @param float string of the floating-point number *)
let float_cnst float =
  new_cnst (new AST.irconstant(TYP.CFloat float))


                                                         (** Expression nodes *)

(** A null expression *)
let nop_expr =
  new_expr (new AST.irexpression (TYP.Nothing, TYP.NoLocation))
  
(** An integer constant expression 
    @param integer string of the integer 
    @param location location of expression *)
let int_cnst_expr integer location =
  new_expr (new AST.irexpression(TYP.Constant (int_cnst integer), location))

(** A floating-point constant expression 
    @param float string of the floating-point number 
    @param location location of expression *)
let float_cnst_expr float location =
  new_expr (new AST.irexpression(TYP.Constant (float_cnst float), location))


(******************************************************************************)

(** AST transformations. *)

(** Replace a statement by a sequence of statement
    @param stmt_pos_id Statement to replace id
    @param stmt_new_id Statement to add id
    @param loc Location of statements 
    @return A sequence of id stmt_pos_id *)
let replace_stmt_by_sequence stmt_pos_id stmt_new_id loc =
  (* find the statement *)
  let stm = AST.find_tbl AST.statement_tbl stmt_pos_id in
  let tmp = new_stmt stm in (* get a new id for this statement *)
  (* build the new sequence statement *)
  let seq = (new AST.irstatement (TYP.Sequence (stmt_new_id, tmp), loc)) in
    Hashtbl.replace AST.statement_tbl stmt_pos_id seq (* replace it *) 


(** Replace an expression by another one
    @param expr_id Expression id to replace
    @param expr Expression to add to add id *)
let replace_expr_by_expr expr_id expr =
  if not (Hashtbl.mem AST.expression_tbl expr_id) then
    prerr_string "Replace expression that doesn't exist (astcommon)...\
                   Expression Added...\n";
  Hashtbl.replace AST.expression_tbl expr_id expr

(******************************************************************************)

(** AST printers. *)

(** Print an expression
    @param expr Expression to print *)
let print_expr expr =
  try
    print_endline ((AST.find_tbl AST.expression_tbl expr)#to_string ())
  with 
    Not_found -> failwith ("Expression " ^ expr ^ " not found...")
