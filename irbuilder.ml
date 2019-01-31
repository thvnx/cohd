(*TODO: ameliorer le type location pour prendre en compte le scope *)

open Cabs (* Abstrat syntax tree definition of Frontc *)

open Libcommon 

module IR = Irtree
module CMD = Commandline
module TYP = Types


let function_name = ref ""

let no_ANSIC89 msg =
  failwith ("Not ANSI C 89 ==> " ^ msg ^ "...")
let no_GNU_Support msg =
  failwith ("No GNU support ==> " ^ msg ^ "...")


let get_sign s =
  match s with
  | NO_SIGN   -> ""
  | SIGNED    -> "signed "
  | UNSIGNED  -> "unsigned "

let get_size s =
  match s with
  | NO_SIZE   -> ""
  | SHORT     -> "short "
  | LONG      -> "long "
  | LONG_LONG -> "long long "

let get_storage s =
  match s with
  | NO_STORAGE -> ""
  | AUTO       -> "auto"
  | STATIC     -> "static"
  | EXTERN     -> "extern"
  | REGISTER   -> "register"

let get_unary_op op =
  match op with
  | MINUS   -> ("-", false)
  | PLUS    -> ("+", false)
  | NOT     -> ("!", false)
  | BNOT    -> ("~", false)
  | MEMOF   -> ("*", false)
  | ADDROF  -> ("&", false)
  | PREINCR -> ("++", false)
  | POSINCR -> ("++", true)
  | PREDECR -> ("--", false)
  | POSDECR -> ("--", true)

let get_binary_op op =
  match op with
  | ADD         -> ("+", true)
  | SUB         -> ("-", true)
  | MUL         -> ("*", true)
  | DIV         -> ("/", true)
  | MOD         -> ("%", true)
  | AND         -> ("&&", true)
  | OR          -> ("||", true)
  | BAND        -> ("&", true)
  | BOR         -> ("|", true)
  | XOR         -> ("^", true)
  | SHL         -> ("<<", true)
  | SHR         -> (">>", true)
  | EQ          -> ("==", false)
  | NE          -> ("!=", false)
  | LT          -> ("<",  false)
  | GT          -> (">",  false)
  | LE          -> ("<=", false)
  | GE          -> (">=", false)
  | ASSIGN      -> ("=",  false)
  | ADD_ASSIGN  -> ("+=", false)
  | SUB_ASSIGN  -> ("-=", false)
  | MUL_ASSIGN  -> ("*=", false)
  | DIV_ASSIGN  -> ("/=", false)
  | MOD_ASSIGN  -> ("%=", false)
  | BAND_ASSIGN -> ("&=", false)
  | BOR_ASSIGN  -> ("|=", false)
  | XOR_ASSIGN  -> ("^=", false)
  | SHL_ASSIGN  -> ("<<=", false)
  | SHR_ASSIGN  -> (">>=", false)




let rec get_type typ loc =
  let id = new_id idx in begin
  match typ with
  | NO_TYPE        -> no_ANSIC89 ("old declaration without type") 
  | VOID           -> IR.add_type id (new IR.irtype(TYP.Void, loc))
  | CHAR sgn       -> IR.add_type id (new IR.irtype(TYP.Char (get_sign sgn), loc))
  | INT (siz, sgn) -> 
    IR.add_type id (new IR.irtype(TYP.Int ((get_sign sgn), (get_size siz)), loc))
  | BITFIELD (sgn, exp) -> 
    let id2 = new_id idx in
    IR.add_bitfield id2 (new IR.irbitfield(id2, (get_sign sgn), (get_exp exp loc)));
    IR.add_type id (new IR.irtype(TYP.BitField (id2), loc))
  | FLOAT boo           -> IR.add_type id (new IR.irtype(TYP.Float (boo), loc))
  | DOUBLE boo          -> IR.add_type id (new IR.irtype(TYP.Double (boo), loc))
  | RESTRICT_PTR t 
  | PTR t               -> IR.add_type id (new IR.irtype(TYP.Ptr (get_type t loc), loc))
  | ARRAY (t, exp) -> 
    let id2 = new_id idx in 
    IR.add_array id2 (new IR.irarray(id2, (get_exp exp loc)));
    IR.add_type id (new IR.irtype(TYP.Array (id2, (get_type t loc)), loc))
  | STRUCT (str, ngl) -> 

    let id2 = new_id idx in 
    IR.add_struct id2 (new IR.irstruct(id2, str, (get_name_grouplist ngl)));
    IR.add_type id (new IR.irtype(TYP.Struct (id2), loc))
  | UNION (str, ngl)  -> 
 
    let id2 = new_id idx in 
    IR.add_union id2 (new IR.irunion(id2, str, (get_name_grouplist ngl)));
    IR.add_type id (new IR.irtype(TYP.Union (id2), loc))
  | PROTO p           -> get_proto id p
  | OLD_PROTO _       -> no_ANSIC89 ("old prototype declaration") 
  | NAMED_TYPE s      -> IR.add_type id (new IR.irtype(TYP.NamedType (s), loc))
  | ENUM (str, eil)   -> 
 
    let id2 = new_id idx in 
    IR.add_enum id2 (new IR.irenum(id2, str, (get_enumlist eil)));
    IR.add_type id (new IR.irtype(TYP.Enum (id2), loc))
  | CONST t             -> IR.add_type id (new IR.irtype(TYP.Const (get_type t loc), loc))
  | VOLATILE t          -> IR.add_type id (new IR.irtype(TYP.Volatile (get_type t loc), loc))
  | GNU_TYPE _          -> no_GNU_Support "GNU_TYPE"
  | TYPE_LINE (s, i, t) -> 
    let l = TYP.Location (s,i) in
    IR.add_type id (new IR.irtype(TYP.Type (get_type t l), l))
  end;
  id


and get_exp exp loc =
  let id = new_id idx in begin
  match exp with
  | NOTHING                     -> IR.add_expression id (new IR.irexpression(TYP.Nothing, loc))
  | UNARY (op, exp)             -> 
    IR.add_expression id (new IR.irexpression(TYP.Unary ((get_unary_op op), (get_exp exp loc)), loc))
  | BINARY (op, exp1, exp2)     -> 
    IR.add_expression id (new IR.irexpression(TYP.Binary ((get_binary_op op), (get_exp exp1 loc), (get_exp exp2 loc)), loc))
  | QUESTION (exp1, exp2, exp3) -> 
    IR.add_expression id (new IR.irexpression(TYP.Question ((get_exp exp1 loc), (get_exp exp2 loc), (get_exp exp3 loc)), loc))
  | CAST (t, exp)    -> 
    IR.add_expression id (new IR.irexpression(TYP.Cast ((get_type t loc), (get_exp exp loc)), loc))
  | CALL (exp, expl) -> 
    IR.add_expression id (new IR.irexpression(TYP.Call ((get_exp exp loc), (get_exp_list expl loc)), loc))
  | COMMA expl       -> 
    IR.add_expression id (new IR.irexpression(TYP.Comma (get_exp_list expl loc), loc))
  | CONSTANT c       -> 
    IR.add_expression id (new IR.irexpression(TYP.Constant (get_constant_id c loc), loc))
  | VARIABLE s       -> 
    IR.add_expression id (new IR.irexpression(TYP.Variable s, loc))
  | EXPR_SIZEOF exp  -> 
    IR.add_expression id (new IR.irexpression(TYP.SizeofExpr (get_exp exp loc), loc))
  | TYPE_SIZEOF t    -> 
    IR.add_expression id (new IR.irexpression(TYP.SizeofType (get_type t loc), loc))
  | INDEX (exp1, exp2)    -> 
    IR.add_expression id (new IR.irexpression(TYP.Index ((get_exp exp1 loc), (get_exp exp2 loc)), loc))
  | MEMBEROF (exp, s)     -> 
   IR.add_expression id ( new IR.irexpression(TYP.MemberOf (s, (get_exp exp loc)), loc))
  | MEMBEROFPTR (exp, s)  -> 
   IR.add_expression id ( new IR.irexpression(TYP.MemberOfPtr (s, (get_exp exp loc)), loc))
  | GNU_BODY b            -> 
    IR.add_expression id (new IR.irexpression(TYP.Body (get_body b), loc))
  | EXPR_LINE (exp, s, i) -> 
    let l = TYP.Location (s,i) in
    IR.add_expression id (new IR.irexpression(TYP.Exp (get_exp exp l), l))
  end;
  id

and get_exp_list el loc =
  let expl = ref [] in
  List.iter ( fun n -> expl := !expl @[ (get_exp n loc)] ) el;
  !expl


and get_constant_id cst loc =
  let id = new_id idx in begin
  match cst with 
  | CONST_INT s         -> IR.add_constant id (new IR.irconstant(TYP.CInt s))
  | CONST_FLOAT s       -> IR.add_constant id (new IR.irconstant(TYP.CFloat s))
  | CONST_CHAR s        -> IR.add_constant id (new IR.irconstant(TYP.CChar s))
  | CONST_STRING s      -> IR.add_constant id (new IR.irconstant(TYP.CString s))
  | CONST_COMPOUND expl -> IR.add_constant id (new IR.irconstant(TYP.CExpList (get_exp_list expl loc)))
  end;
  id


and get_statement stm loc =
  let id = new_id idx in begin
  match stm with
  | NOP             ->
    IR.add_statement id (new IR.irstatement (TYP.Nop, loc))
  | COMPUTATION exp ->
    IR.add_statement id (new IR.irstatement (TYP.Computation (get_exp exp loc), loc))
  | BLOCK blk       ->
    IR.add_statement id (new IR.irstatement (TYP.Block (get_body blk), loc))
  | SEQUENCE (stm1, stm2) ->
    IR.add_statement id (new IR.irstatement (TYP.Sequence ((get_statement stm1 loc), (get_statement stm2 loc)), loc))
  | IF (exp, stm1, stm2)  -> 
    IR.add_statement id (new IR.irstatement (TYP.If 
       ((get_exp exp loc),  (get_statement stm1 loc), (get_statement stm2 loc)), loc))
  | WHILE (exp, stm)      ->  
    IR.add_statement id (new IR.irstatement (TYP.While ((get_exp exp loc), (get_statement stm loc)), loc))
  | DOWHILE (exp, stm)   ->  
    IR.add_statement id (new IR.irstatement (TYP.DoWhile ((get_exp exp loc), (get_statement stm loc)), loc)) 
  | FOR (exp1, exp2, exp3, stm) ->
    IR.add_statement id (new IR.irstatement (TYP.For
       ((get_exp exp1 loc), (get_exp exp2 loc), (get_exp exp3 loc), (get_statement stm loc)), loc))
  | BREAK      ->
    IR.add_statement id (new IR.irstatement (TYP.Break, loc))
  | CONTINUE   ->
    IR.add_statement id (new IR.irstatement (TYP.Continue, loc))
  | RETURN exp ->
    IR.add_statement id (new IR.irstatement (TYP.Return (get_exp exp loc), loc))
  | SWITCH (exp, stm) -> 
    IR.add_statement id (new IR.irstatement (TYP.Switch ((get_exp exp loc), (get_statement stm loc)), loc))
  | CASE  (exp, stm)  ->
    IR.add_statement id (new IR.irstatement (TYP.Case ((get_exp exp loc), (get_statement stm loc)), loc))
  | DEFAULT stm       ->
    IR.add_statement id (new IR.irstatement (TYP.Default (get_statement stm loc), loc))
  | LABEL (lab, stm)  ->
    IR.add_statement id (new IR.irstatement (TYP.Label (lab, (get_statement stm loc)), loc))
  | GOTO str ->
    IR.add_statement id (new IR.irstatement (TYP.Goto (str), loc))
  | ASM str  ->
    IR.add_statement id (new IR.irstatement (TYP.Asm (str), loc))
  | GNU_ASM _              -> no_GNU_Support "GNU_ASM"
  | STAT_LINE (stm, f, l)  ->
    let ll = TYP.Location (f,l) in
    IR.add_statement id (new IR.irstatement (TYP.Stat (get_statement stm ll), ll))
  end;
  id


and get_body bod =
  match bod with
  | (defl, stmt) -> 
     let id = new_id idx in 
     IR.add_body id (new IR.irbody ((get_definitionlist defl), (get_statement stmt TYP.NoLocation)));
     id

and get_name nme =
  match nme with
  | (s, t, _, exp) -> 
    let id = new_id idx in 
    IR.add_variable id (new IR.irvariable(s, (get_type t TYP.NoLocation), (get_exp exp TYP.NoLocation)));
    id


and get_namelist nl =
  let names = ref [] in
  List.iter (fun n -> names := !names @ [(get_name n)]) nl;
  !names


and get_single_name nme =
  match nme with
  | (typ, stor, name) -> new IR.irsinglename((get_type typ TYP.NoLocation), (get_storage stor), (get_name name))


and get_single_namelist snl =
  let names = ref [] in
  List.iter (fun n -> names := !names @ [(get_single_name n)]) snl;
  !names

and get_enumlist enl =
  let items = ref [] in
  List.iter (fun (str, exp) -> items := !items @ [(str, (get_exp exp TYP.NoLocation))]) enl;
  !items

and get_proto id pro =
  match pro with
  | (typ, snl, b) ->
    let id2 = new_id idx in
    IR.add_proto id2 (new IR.irprototype (b, (get_single_namelist snl), !function_name));
    IR.add_type id (new IR.irtype (TYP.Proto (id2, (get_type typ TYP.NoLocation)), TYP.NoLocation))

and get_name_group nmg =
  match nmg with (typ, str, nml) ->
  let id = new_id idx in 
  IR.add_namegroup id (new IR.irnamegroup ((get_type typ TYP.NoLocation), get_storage str, get_namelist nml));
  id

and get_name_grouplist ngl =
  let names = ref [] in
  List.iter (fun n -> names := !names @ [(get_name_group n)]) ngl;
  !names


and get_definition def =
  match def with
  | FUNDEF (name, body) -> 
    begin match name with
    | (_, _, (s, _, _, _)) -> function_name := s 
    end;
    let id = new_id idx in 
    IR.add_function id (new IR.irfunction ((get_single_name name), (get_body body)));
    new IR.irdefinition(TYP.Function id)

  | OLDFUNDEF _         -> 
    no_ANSIC89 "old function definition"
  | DECDEF nmg          -> 
    new IR.irdefinition(TYP.Declaration (get_name_group nmg));
  | TYPEDEF (nmg, _)    ->
 
    new IR.irdefinition(TYP.TypeDef (get_name_group nmg))
  | ONLYTYPEDEF nmg     -> 

    new IR.irdefinition(TYP.OnlyTypeDef (get_name_group nmg))


and get_definitionlist dfl =
  let defs = ref [] in
  List.iter (fun n -> defs := !defs @ [(get_definition n)]) dfl;
  !defs





let files = ref []
(* Build intermediate representation *)
let build (ast:file) (nme:string) = 
  files := new IR.irfile(nme, (get_definitionlist ast)) :: !files
