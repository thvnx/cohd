
open Types
open Libcommon

module TOC = Irtoc
module DOT = Irtodot

let variable_tbl   = Hashtbl.create 0
let constant_tbl   = Hashtbl.create 0
let type_tbl       = Hashtbl.create 0

let find_tbl hash id =
  try Hashtbl.find hash id 
  with Not_found -> failwith ("find_tbl "^id)

let remove_tbl hash id =
  Hashtbl.remove hash id


let array_tbl      = Hashtbl.create 0
let bitfield_tbl   = Hashtbl.create 0
let struct_tbl     = Hashtbl.create 0
let union_tbl      = Hashtbl.create 0
let enum_tbl       = Hashtbl.create 0
let proto_tbl      = Hashtbl.create 0
let expression_tbl = Hashtbl.create 0
let body_tbl       = Hashtbl.create 0
let statement_tbl  = Hashtbl.create 0
let function_tbl   = Hashtbl.create 0
let namegroup_tbl  = Hashtbl.create 0





(******************************************)
(** Intermediate representation of a type *)
(******************************************)
class irtype ((typ:ttype), (loc:location)) =
object

  val type_type   = typ
  val type_loc    = loc

  method get_type = type_type
  method get_loc  = type_loc

  (** Get the declaration line number in source file *)
  method get_line_number = 
    match type_loc with NoLocation -> -1 | Location (_,l) -> l

   method to_string () =
    match type_type with
    | Type typ        -> (find_tbl type_tbl typ)#to_string ()
    | Void            -> "void"
    | Char sgn        -> sgn ^ "char"
    | Int (sze, sgn)  -> sgn ^ sze ^ "int"
    | BitField id     -> (find_tbl bitfield_tbl id)#to_string ()
    | Float bl        -> "float"
    | Double bl       -> if bl then "long double" else "double"
    | Ptr typ         -> (find_tbl type_tbl typ)#to_string () ^ "*"
    | Array (id, typ) -> (find_tbl type_tbl typ)#to_string () ^ "[" ^ ((find_tbl array_tbl id)#to_string ()) ^ "]"
(*!!    | Array (id, typ) -> (find_tbl type_tbl typ)#to_string () ^ "[]"*)
    | Struct id       -> (find_tbl struct_tbl id)#to_string ()
    | Union id        -> (find_tbl union_tbl id)#to_string ()
    | Proto (id, typ) -> (find_tbl proto_tbl id)#to_string ()
    | NamedType str   -> str 
    | Enum id         -> (find_tbl enum_tbl id)#to_string ()
    | Const typ       -> (find_tbl type_tbl typ)#to_string ()
    | Volatile typ    -> (find_tbl type_tbl typ)#to_string ()


end;;
(** Add a IR type in the type hash table *)
let add_type (idx:string) (typ:irtype) = Hashtbl.add type_tbl idx typ 
(******************************************************************************)
(******************************************************************************)


(**********************************************)
(** Intermediate representation of a variable *)
(**********************************************)
class irvariable ((nme:string), (typ:string), (vle:string)) =
object (self)

  val var_type  = typ
  val var_value = vle
  val var_name  = nme
 
  method get_name  = var_name
  method get_type  = var_type
  method get_value = var_value


  method get_line () = (find_tbl type_tbl var_type)#get_line_number
  method type_to_string () = (find_tbl type_tbl var_type)#to_string ()
  method value_to_string () = (find_tbl expression_tbl var_value)#to_string ()


  method to_string () = 
    let value = self#value_to_string () in
    "#var " ^ var_name ^ 
    " (" ^ self#type_to_string () ^ ")" ^
    (if String.length value = 0 then "" else " := " ^ value)

  method get_c () =
    match (find_tbl type_tbl var_type)#get_type with
    | BitField _     -> TOC.var_bitfield (self#type_to_string ()) var_name 
    | Proto (t, tid) -> TOC.var_string ((find_tbl type_tbl tid)#to_string ()) 
                                       (var_name ^ self#type_to_string ()) 
                                       (self#value_to_string ())
    | _              -> TOC.var_string (self#type_to_string ()) 
                                       var_name 
                                       (self#value_to_string ())

end;;
let add_variable (idx:string) (var:irvariable) = Hashtbl.add variable_tbl idx var 
(******************************************************************************)
(******************************************************************************)




(**********************************************)
(** Intermediate representation of a constant *)
(**********************************************)
class irconstant (cst:tconst) =
object

  val const = cst

  method get_const = const

  
  method to_string () =
    match const with
    | CExpList el -> 
      let str = ref "" in
      str := "{" ^ (find_tbl expression_tbl (List.hd el))#to_string ();
      List.iter (fun i -> str := !str ^ ", " ^  (find_tbl expression_tbl i)#to_string ()) (List.tl el);
      !str^"}"
    | CInt c | CFloat c -> c
    | CChar c           -> "'" ^ c ^ "'"
    | CString c         -> "\"" ^ (Libcommon.conserve_special_characters c) ^ "\""

end ;;
(** Add a IR constant in the constant hash table *)
let add_constant (idx:string) (cst:irconstant) = Hashtbl.add constant_tbl idx cst 
(******************************************************************************)
(******************************************************************************)


(********************************************)
(** Intermediate representation of an array *)
(********************************************)
class irarray ((idx:string), (exp:string)) =
object

  val array_idx = idx
  val array_exp = exp

  
  method to_string () = 
    (find_tbl expression_tbl exp)#to_string ()

end;;
(** Add a IR array in the array hash table *)
let add_array (idx:string) (arr:irarray) = Hashtbl.add array_tbl idx arr 
(******************************************************************************)
(******************************************************************************)


(**********************************************)
(** Intermediate representation of a bitfield *)
(**********************************************)
class irbitfield ((idx:string), (sgn:string), (exp:string)) =
object (self)

  val bitf_idx = idx
  val bitf_sgn = sgn
  val bitf_exp = exp


  method to_string () =
    sgn ^ "int : " ^ (find_tbl expression_tbl exp)#to_string ()

end;;
(** Add a IR bitfield in the bitfield hash table *)
let add_bitfield (idx:string) (bit:irbitfield) = Hashtbl.add bitfield_tbl idx bit 
(******************************************************************************)
(******************************************************************************)


(*************************************************)
(** Intermediate representation of a enumeration *)
(*************************************************)
class irenum ((idx:string), (nme:string), (eil:(string * string) list)) =
object (self)

  val enum_idx   = idx
  val enum_nme   = nme
  val enum_items = eil


  method to_string () = 
    let res = ref "" in 
    List.iter (fun (str, exp) -> 
      res := !res ^ (TOC.enum_item str ((find_tbl expression_tbl exp)#to_string ()))
    ) enum_items;
    (TOC.begin_enum "") ^ (if (String.length !res > 0) then (String.sub !res 0 ((String.length !res) -2)) else !res) ^ 
    (TOC.end_enum enum_nme)

end;;
(** Add a IR enum in the enum hash table *)
let add_enum (idx:string) (enu:irenum) = Hashtbl.add enum_tbl idx enu
(******************************************************************************)
(******************************************************************************)


(********************************************)
(** Intermediate representation of a struct *)
(********************************************)
class irstruct ((idx:string), (nme:string), (def:string list)) =
object (self)

  val struct_idx = idx
  val struct_nme = nme
  val struct_exl = def


  method to_string () = 
    let str_beg = (TOC.begin_struct struct_nme) in
    let res = ref "" in
    List.iter (fun i -> res := !res ^ TOC.print_tabs !TOC.t ^ (find_tbl namegroup_tbl i)#get_c () ^ TOC.semicolon ^ TOC.newline) struct_exl;
    str_beg ^ !res ^ (TOC.end_struct "")


end;;
(** Add a IR struct in the struct hash table *)
let add_struct (idx:string) (stt:irstruct) = Hashtbl.add struct_tbl idx stt
(******************************************************************************)
(******************************************************************************)


(*********************************************)
(** Intermediate representation of an union  *)
(*********************************************)
class irunion ((idx:string), (nme:string), (def:string list)) =
object (self)

  val union_idx = idx
  val union_nme = nme
  val union_exl = def


  method to_string () = 
    let res = ref "" in
    let str_beg = (TOC.begin_union union_nme) in
    List.iter (fun i -> res := !res ^ TOC.print_tabs !TOC.t ^ (find_tbl namegroup_tbl i)#get_c () ^ TOC.semicolon ^ TOC.newline) union_exl;
    str_beg ^ !res ^ (TOC.end_union "")


end;;
(** Add a IR union in the union hash table *)
let add_union (idx:string) (uni:irunion) = Hashtbl.add union_tbl idx uni
(******************************************************************************)
(******************************************************************************)


(************************************************************)
(** Intermediate representation of a singlename (prototype) *)
(************************************************************)
class irsinglename ((typ:string), (sto:string), (nme:string)) =
object (self)

  val sn_type    = typ
  val sn_storage = sto
  val sn_name    = nme

  method get_type    = sn_type
  method get_name    = sn_name 
  method get_storage = sn_storage


  method name_to_string () = (find_tbl variable_tbl sn_name)#get_name
  method type_to_string () = (find_tbl variable_tbl sn_name)#type_to_string ()

  method to_string () = 
    (if sn_storage = "" then "" else sn_storage ^ " ") ^ 
      ((find_tbl type_tbl sn_type)#to_string ()) ^ " " ^ 
      self#name_to_string () ^ " " ^ self#type_to_string ()
  
  method gen_c (chan:out_channel) =
    output_string chan (TOC.function_header sn_storage 
      ((find_tbl type_tbl sn_type)#to_string ()) 
      (self#name_to_string ())
      (self#type_to_string ()))

end;;
(******************************************************************************)
(******************************************************************************)


(***********************************************)
(** Intermediate representation of a namegroup *)
(***********************************************)
class irnamegroup ((typ:string), (str:string), (nml:string list)) =
object (self)

  val nmg_typ = typ
  val nmg_sto = str
  val nmg_nml = nml


  method get_type () = find_tbl type_tbl nmg_typ
  method get_names = nmg_nml

  method type_to_string () = (self#get_type ())#to_string ()


  method to_string () = 
    let res = ref "" in
    List.iter (fun i -> res := !res ^ (nmg_sto ^ ((find_tbl variable_tbl i)#to_string ()) )) nmg_nml;
    !res

  method get_c () =
    let res = ref "" in
    List.iter (fun i -> res := !res ^ (nmg_sto ^ ((find_tbl variable_tbl i)#get_c ()))) nmg_nml;
    !res

  method gen_c (chan:out_channel) = 
    List.iter (fun i -> 
      output_string chan (TOC.variable nmg_sto ((find_tbl variable_tbl i)#get_c ()) )
    ) nmg_nml

end;;
(** Add a IR namegroup in the namegroup hash table *)
let add_namegroup (idx:string) (nmg:irnamegroup) = Hashtbl.add namegroup_tbl idx nmg
(******************************************************************************)
(******************************************************************************)


(************************************************)
(** Intermediate representation of a definition *)
(************************************************)
class irdefinition ((def:tdefinition)) =
object (self)

  val def_typ = def


  method get_definition = def_typ


  method gen_c (chan:out_channel) =
    match def_typ with
    | Function f    -> (find_tbl function_tbl f)#gen_c (chan)
    | Declaration d -> (find_tbl namegroup_tbl d)#gen_c (chan)
    | TypeDef t     -> output_string chan (TOC.typedef ""); 
                       (find_tbl namegroup_tbl t)#gen_c (chan)
    | OnlyTypeDef o -> output_string chan (((find_tbl namegroup_tbl o)#type_to_string ()) ^ TOC.semicolon ^ TOC.newline)


  method gen_dot (chan:out_channel) =
    match def_typ with
    | Function f    -> (find_tbl function_tbl f)#gen_dot (chan)
    | Declaration d -> ()
    | TypeDef t     -> ()
    | OnlyTypeDef o -> ()



  method to_string () =
    match def_typ with
    | Function f    -> (find_tbl function_tbl f)#to_string ()
    | Declaration d -> (find_tbl namegroup_tbl d)#to_string ()
    | TypeDef t     -> "typedef " ^ (find_tbl namegroup_tbl t)#to_string ()
    | OnlyTypeDef o -> ((find_tbl namegroup_tbl o)#type_to_string ())

end;;
(******************************************************************************)
(******************************************************************************)



(*************************************************) 
(** Intermediate representation of an expression *)
(*************************************************)
class irexpression ((knd:expression), (loc:location)) =
object (self)

  val exp_kind     = knd
  val exp_location = loc

  val mutable paren  = false

  method get_type = exp_kind
  method set_par b = 
    match exp_kind with
    | Exp exp -> (find_tbl expression_tbl exp)#set_par (b) 
    | _ -> paren <- b

  method get_copy () =
    let add_expression (idx:string) (exp:irexpression) = Hashtbl.add expression_tbl idx exp in
    let copy_id = new_id idx in
    begin match exp_kind with
    | Exp exp ->
      let cp = (find_tbl expression_tbl exp)#get_copy () in 
      add_expression copy_id (new irexpression (Exp (cp), exp_location))
    | Nothing -> 
      add_expression copy_id (new irexpression (Nothing, exp_location))
    | Unary ((str, bln), exp) ->
      let cp = (find_tbl expression_tbl exp)#get_copy () in 
      add_expression copy_id (new irexpression (Unary ((str, bln), cp), exp_location))
    | Binary ((str, bln), exp1, exp2) ->
      let cp1 = (find_tbl expression_tbl exp1)#get_copy () in 
      let cp2 = (find_tbl expression_tbl exp2)#get_copy () in 
      add_expression copy_id (new irexpression (Binary ((str, bln), cp1, cp2), exp_location))
    | Question (exp1, exp2, exp3) ->
      let cp1 = (find_tbl expression_tbl exp1)#get_copy () in 
      let cp2 = (find_tbl expression_tbl exp2)#get_copy () in 
      let cp3 = (find_tbl expression_tbl exp3)#get_copy () in 
      add_expression copy_id (new irexpression (Question (cp1, cp2, cp3), exp_location))
    | Cast (typ, exp) -> (*copy typ*)
      let cp = (find_tbl expression_tbl exp)#get_copy () in 
      add_expression copy_id (new irexpression (Cast (typ, cp), exp_location))
    | Call (exp, expl) ->
      let cp = (find_tbl expression_tbl exp)#get_copy () in 
      let cpl = ref [] in 
      List.iter (fun i -> cpl := !cpl @ [(find_tbl expression_tbl i)#get_copy ()]) expl;
      add_expression copy_id (new irexpression (Call (cp, !cpl), exp_location))
    | Comma expl ->
      let cpl = ref [] in 
      List.iter (fun i -> cpl := !cpl @ [(find_tbl expression_tbl i)#get_copy ()]) expl;
      add_expression copy_id (new irexpression (Comma (!cpl), exp_location))
    | Constant cst -> (* copy cst*)
      add_expression copy_id (new irexpression (Constant cst, exp_location))
    | Variable str ->
      add_expression copy_id (new irexpression (Variable str, exp_location))
    | SizeofExpr exp ->
      let cp = (find_tbl expression_tbl exp)#get_copy () in 
      add_expression copy_id (new irexpression (SizeofExpr cp, exp_location))
    | SizeofType typ -> (*copy typ*)
      add_expression copy_id (new irexpression (SizeofType typ, exp_location))
    | Index (exp1, exp2) ->
      let cp1 = (find_tbl expression_tbl exp1)#get_copy () in 
      let cp2 = (find_tbl expression_tbl exp2)#get_copy () in 
      add_expression copy_id (new irexpression (Index (cp1, cp2), exp_location))
    | MemberOf (str, exp) ->
       let cp = (find_tbl expression_tbl exp)#get_copy () in 
      add_expression copy_id (new irexpression (MemberOf (str, cp), exp_location))
    | MemberOfPtr (str, exp) ->
      let cp = (find_tbl expression_tbl exp)#get_copy () in 
      add_expression copy_id (new irexpression (MemberOfPtr (str, cp), exp_location))
    | Body bod -> 
      let cp_bod = (find_tbl body_tbl bod)#get_copy () in
      add_expression copy_id (new irexpression (Body cp_bod, exp_location))
    end;
    copy_id



  method to_string  () =
    match exp_kind with
    | Exp eid                 -> (find_tbl expression_tbl eid)#to_string ()
    | Nothing                 -> ""
    | Unary ((str, pre), eid) -> 
      let exp_str = (find_tbl expression_tbl eid)#to_string () in
      (if paren then "(" else "") ^ (if pre then exp_str ^ str else str ^ exp_str) ^ (if paren then ")" else "")
    | Binary ((str, par), eid1, eid2) ->
      (* self#set_par (par); *)
      (find_tbl expression_tbl eid1)#set_par (par);
      (find_tbl expression_tbl eid2)#set_par (par); 

(* print_string (new_id idx ^ " " ^ ((find_tbl expression_tbl eid1)#to_string ()) ^ " " ^ str ^ " " ^ ((find_tbl expression_tbl eid2)#to_string ()) ^ " " ^ (if paren then "T" else "F")  ^ "\n"); *)

      let exp_str = ((find_tbl expression_tbl eid1)#to_string ()) ^ " " ^ str ^ " " ^ ((find_tbl expression_tbl eid2)#to_string ()) in

      (if paren then "(" else "") ^ exp_str ^ (if paren then ")" else "")
    | Question (qid, tid, eid) -> 
        ((find_tbl expression_tbl qid)#to_string ()) ^ " ? " ^ 
        ((find_tbl expression_tbl tid)#to_string ()) ^ " : " ^ 
        ((find_tbl expression_tbl eid)#to_string ())
    | Cast (id, eid) -> 
      "(" ^ (find_tbl type_tbl id)#to_string () ^ ") " ^ ((find_tbl expression_tbl eid)#to_string ())
    | Call (eid, lid)    -> 
      let callto = ((find_tbl expression_tbl eid)#to_string ()) in
      let str = ref "" in
      List.iter (fun i -> str := !str ^ (find_tbl expression_tbl i)#to_string () ^ ", ") (lid);
      str := String.sub !str 0 ((String.length !str)-2);
      callto ^ "(" ^ !str ^ ")"
    | Comma lid   -> 
      let str = ref "" in
      List.iter (fun i -> str := !str ^ (find_tbl expression_tbl i)#to_string () ^ ", ") (lid);
      str := String.sub !str 0 ((String.length !str)-2);
      !str
    | Constant str    -> (find_tbl constant_tbl str)#to_string ()
    | Variable str    -> str
    | SizeofExpr eid  -> "sizeof( " ^ (find_tbl expression_tbl eid)#to_string () ^ " )"
    | SizeofType str  -> "sizeof( " ^ ((find_tbl type_tbl str)#to_string ()) ^ " )"
    | Index (eid1, eid2)     ->
      ((find_tbl expression_tbl eid1)#to_string ()) ^ "[" ^ ((find_tbl expression_tbl eid2)#to_string ()) ^ "]"
    | MemberOf (str, eid)    -> ((find_tbl expression_tbl eid)#to_string ()) ^ "." ^ str
    | MemberOfPtr (str, eid) -> ((find_tbl expression_tbl eid)#to_string ()) ^ "->" ^ str
    | Body b                 -> (find_tbl body_tbl b)#to_string ()


  method gen_dot (chan, exp_id) = 
    match exp_kind with
    | Exp eid                 -> (find_tbl expression_tbl eid)#gen_dot (chan, exp_id)
    | Nothing                 -> ()
    | Unary ((str, pre), eid) ->
      output_string chan (DOT.expression_node str exp_id);
      (find_tbl expression_tbl eid)#gen_dot (chan, eid);
      output_string chan (DOT.relation exp_id eid)
    | Binary ((str, par), eid1, eid2) ->
      output_string chan (DOT.expression_node str exp_id);
      (find_tbl expression_tbl eid1)#gen_dot (chan, eid1);
      (find_tbl expression_tbl eid2)#gen_dot (chan, eid2);
      output_string chan (DOT.relation exp_id eid1);
      output_string chan (DOT.relation exp_id eid2)
    | Question (qid, tid, eid) ->
      output_string chan (DOT.expression_node "question" exp_id);
      (find_tbl expression_tbl qid)#gen_dot (chan, qid);
      (find_tbl expression_tbl tid)#gen_dot (chan, tid);
      (find_tbl expression_tbl eid)#gen_dot (chan, eid);
      output_string chan (DOT.relation exp_id qid);
      output_string chan (DOT.relation exp_id tid);
      output_string chan (DOT.relation exp_id eid)
    | Cast (id, eid) ->
      output_string chan (DOT.expression_node ((find_tbl type_tbl id)#to_string ()) exp_id);
      (find_tbl expression_tbl eid)#gen_dot (chan, eid);
      output_string chan (DOT.relation exp_id eid);

    | Call (eid, lid)  ->
()
    | Comma lid ->
()
    | Constant str -> output_string chan (DOT.terminal_expression_node ((find_tbl constant_tbl str)#to_string ()) exp_id)
    | Variable str  ->  output_string chan (DOT.terminal_expression_node str exp_id)      
    | SizeofExpr eid ->
() 
    | SizeofType str ->
()  
    | Index (eid1, eid2) -> 
      let str = ((find_tbl expression_tbl exp_id)#to_string ()) in
      output_string chan (DOT.terminal_expression_node str exp_id)
    | MemberOf (str, eid) ->
()
    | MemberOfPtr (str, eid)  ->
()
    | Body b -> (find_tbl body_tbl b)#gen_dot (chan, exp_id)


      


end;;
(** Add a IR expression in the expression hash table *)
let add_expression (idx:string) (exp:irexpression) = Hashtbl.add expression_tbl idx exp 
(******************************************************************************)
(******************************************************************************)



(***********************************************)
(** Intermediate representation of a statement *)
(***********************************************)
class irstatement ((stm:tstatement), (loc:location)) =
object (self)

  val stm_stm = stm
  val stm_loc = loc


  method get_line_number = 
    match stm_loc with NoLocation -> -1 | Location (_,l) -> l
  method get_stmt_type = stm_stm

  method get_type =
    match stm_stm with
    | Stat s -> (find_tbl statement_tbl s)#get_type
    | Nop -> "nop"
    | Computation _ -> "computation"
    | Block _ -> "block"
    | _   -> "other"


  method get_copy () =
    let stm_loc = NoLocation in
    let add_statement (idx:string) (stm:irstatement) = Hashtbl.add statement_tbl idx stm in
    let copy_id = new_id idx in
    begin match stm_stm with
    | Stat stm        -> 
      let cp = (find_tbl statement_tbl stm)#get_copy () in 
      add_statement copy_id (new irstatement (Stat cp, stm_loc))
    | Nop  -> add_statement copy_id (new irstatement (Nop, stm_loc))
    | Computation exp ->
      let cp_exp = (find_tbl expression_tbl exp)#get_copy () in 
      add_statement copy_id (new irstatement (Computation(cp_exp), stm_loc))
    | Block blk -> 
      let cp_blk = (find_tbl body_tbl blk)#get_copy () in
      add_statement copy_id (new irstatement (Block(cp_blk), stm_loc))
    | Sequence (stm1, stm2) -> 
      let cp1 = (find_tbl statement_tbl stm1)#get_copy () in 
      let cp2 = (find_tbl statement_tbl stm2)#get_copy () in 
      add_statement copy_id (new irstatement (Sequence (cp1, cp2), stm_loc))
    | If (exp, stm1, stm2) ->
      let cp_exp = (find_tbl expression_tbl exp)#get_copy () in 
      let cp1 = (find_tbl statement_tbl stm1)#get_copy () in 
      let cp2 = (find_tbl statement_tbl stm2)#get_copy () in 
      add_statement copy_id (new irstatement (If (cp_exp, cp1, cp2), stm_loc))
    | While (exp, stm) ->
      let cp_exp = (find_tbl expression_tbl exp)#get_copy () in 
      let cp = (find_tbl statement_tbl stm)#get_copy () in 
      add_statement copy_id (new irstatement (While (cp_exp, cp), stm_loc))
    | DoWhile (exp, stm) ->
      let cp_exp = (find_tbl expression_tbl exp)#get_copy () in 
      let cp = (find_tbl statement_tbl stm)#get_copy () in 
      add_statement copy_id (new irstatement (DoWhile (cp_exp, cp), stm_loc))
    | For (exp1, exp2, exp3, stm) ->
      let cp_exp1 = (find_tbl expression_tbl exp1)#get_copy () in 
      let cp_exp2 = (find_tbl expression_tbl exp2)#get_copy () in 
      let cp_exp3 = (find_tbl expression_tbl exp3)#get_copy () in 
      let cp = (find_tbl statement_tbl stm)#get_copy () in 
      add_statement copy_id (new irstatement (For (cp_exp1, cp_exp2, cp_exp3, cp), stm_loc))
    | Break -> add_statement copy_id (new irstatement (Break, stm_loc))
    | Continue -> add_statement copy_id (new irstatement (Continue, stm_loc))
    | Return exp ->
      let cp_exp = (find_tbl expression_tbl exp)#get_copy () in 
      add_statement copy_id (new irstatement (Return cp_exp, stm_loc))
    | Switch (exp, stm) -> 
      let cp_exp = (find_tbl expression_tbl exp)#get_copy () in 
      let cp = (find_tbl statement_tbl stm)#get_copy () in 
      add_statement copy_id (new irstatement (Switch (cp_exp, cp), stm_loc))
    | Case (exp, stm) ->
      let cp_exp = (find_tbl expression_tbl exp)#get_copy () in 
      let cp = (find_tbl statement_tbl stm)#get_copy () in 
      add_statement copy_id (new irstatement (Case (cp_exp, cp), stm_loc))
    | Default stm -> 
      let cp = (find_tbl statement_tbl stm)#get_copy () in 
      add_statement copy_id (new irstatement (Default cp, stm_loc))
    | Label (str, stm) -> 
      let cp = (find_tbl statement_tbl stm)#get_copy () in 
      add_statement copy_id (new irstatement (Label (str, cp), stm_loc))
    | Goto str -> add_statement copy_id (new irstatement (Goto str, stm_loc))
    | Asm str -> add_statement copy_id (new irstatement (Asm str, stm_loc))
    end;
    copy_id


  method to_string () =
    match stm_stm with
    | Stat stm        -> (find_tbl statement_tbl stm)#to_string ()
    | Nop             -> ""
    | Computation exp -> "" ^ (find_tbl expression_tbl exp)#to_string () ^ "\n"
    | Block blk       -> (find_tbl body_tbl blk)#to_string () ^ "\n"
    | Sequence (stm1, stm2) -> 
      (find_tbl statement_tbl stm1)#to_string () ^ (find_tbl statement_tbl stm2)#to_string ()
    | If (exp, stm1, stm2)  -> 
      "if(" ^ (find_tbl expression_tbl exp)#to_string () ^ ")\n" ^ 
        (find_tbl statement_tbl stm1)#to_string () ^ 
        "\telse\n{\n" ^ (find_tbl statement_tbl stm2)#to_string () ^ "}\n"
    | While (exp, stm)      -> 
      "while(" ^ (find_tbl expression_tbl  exp)#to_string () ^ ")\n" ^ 
        (find_tbl statement_tbl stm)#to_string ()
    | DoWhile (exp, stm)    ->
      "do\n" ^ (find_tbl statement_tbl stm)#to_string () ^ "\twhile(" ^ 
        (find_tbl expression_tbl exp)#to_string () ^ ")\n"
    | For (exp1, exp2, exp3, stm) ->
      "for(" ^ (find_tbl expression_tbl exp1)#to_string () ^ "; " ^ 
        (find_tbl expression_tbl exp2)#to_string () ^ "; " ^ 
        (find_tbl expression_tbl exp3)#to_string () ^ ")\n" ^ 
        (find_tbl statement_tbl stm)#to_string ()
    | Break -> "\tbreak\n"
    | Continue -> "\tcontinue\n"
    | Return exp -> 
      "return " ^ (find_tbl expression_tbl exp)#to_string () ^ "\n"
    | Switch (exp, stm) -> 
      "switch(" ^ (find_tbl expression_tbl exp)#to_string () ^ ")\n " ^ 
        (find_tbl statement_tbl stm)#to_string ()
    | Case (exp, stm)   -> 
      "case :" ^ (find_tbl expression_tbl exp)#to_string () ^ "\n " ^ 
        (find_tbl statement_tbl stm)#to_string ()
    | Default stm       -> 
      "default :\n " ^ (find_tbl statement_tbl stm)#to_string ()
    | Label (str, stm)  -> "label : " ^ str ^ " \n"
    | Goto str          -> "goto : " ^ str ^ " \n"
    | Asm str           -> "asm : " ^ str ^ " \n"
 


  method gen_c (chan:out_channel) =
    match stm_stm with
    | Stat stm              -> (find_tbl statement_tbl stm)#gen_c (chan)
    | Nop                   -> output_string chan TOC.nothing
    | Computation exp       -> output_string chan (TOC.computation ((find_tbl expression_tbl exp)#to_string ()))
    | Block blk             -> (find_tbl body_tbl blk)#gen_c (chan)
    | Sequence (stm1, stm2) -> 
      (find_tbl statement_tbl stm1)#gen_c (chan);
      (find_tbl statement_tbl stm2)#gen_c (chan)
    | If (exp, stm1, stm2) ->
      output_string chan (TOC.condition_then ((find_tbl expression_tbl exp)#to_string ()));
      (find_tbl statement_tbl stm1)#gen_c (chan);
      let else_stm = (find_tbl statement_tbl stm2) in
      if(else_stm#get_type <> "nop") then begin output_string chan (TOC.condition_else_begin "") end;
      else_stm#gen_c (chan);
    | While (exp, stm)     ->
      output_string chan (TOC.loop_while ((find_tbl expression_tbl exp)#to_string ()));
      (find_tbl statement_tbl stm)#gen_c (chan)
    | DoWhile (exp, stm)   ->
      output_string chan (TOC.loop_dowhile_begin "");
      (find_tbl statement_tbl stm)#gen_c (chan);
      output_string chan (TOC.loop_dowhile_end ((find_tbl expression_tbl exp)#to_string ()))
    | For (exp1, exp2, exp3, stm) ->
      output_string chan (TOC.loop_for ((find_tbl expression_tbl exp1)#to_string ()) 
                                       ((find_tbl expression_tbl exp2)#to_string ()) 
                                       ((find_tbl expression_tbl exp3)#to_string ()));
      (find_tbl statement_tbl stm)#gen_c (chan)
    | Break                       -> output_string chan (TOC.break "")
    | Continue                    -> output_string chan (TOC.continue "")
    | Return exp                  -> output_string chan (TOC.return ((find_tbl expression_tbl exp)#to_string ()));
    | Switch (exp, stm)           ->
      output_string chan (TOC.switch ((find_tbl expression_tbl exp)#to_string ()));
      (find_tbl statement_tbl stm)#gen_c (chan)
    | Case (exp, stm)             ->
      output_string chan (TOC.case ((find_tbl expression_tbl exp)#to_string ()));
      (find_tbl statement_tbl stm)#gen_c (chan)
    | Default stm                 -> 
      output_string chan (TOC.default "");
      (find_tbl statement_tbl stm)#gen_c (chan)
    | Label (str, stm) -> output_string chan (TOC.label str)
    | Goto str         -> output_string chan (TOC.goto str)
    | Asm str          -> output_string chan (TOC.asm str)



  method gen_dot ((chan:out_channel), (stm_id:string)) =
    match stm_stm with
    | Stat stm              -> 
      (find_tbl statement_tbl stm)#gen_dot (chan, stm_id)
    | Nop                   -> output_string chan (DOT.statement_node "NOP" stm_id)
    | Computation exp       -> 
      output_string chan (DOT.statement_node "COMP" stm_id);
      (find_tbl expression_tbl exp)#gen_dot (chan, exp);
      output_string chan (DOT.relation stm_id exp)
    | Block blk             -> 
      (find_tbl body_tbl blk)#gen_dot (chan, stm_id)
    | Sequence (stm1, stm2) ->
      output_string chan (DOT.statement_node "SEQ" stm_id);
      (find_tbl statement_tbl stm1)#gen_dot (chan, stm1);
      (find_tbl statement_tbl stm2)#gen_dot (chan, stm2);
      output_string chan (DOT.relation stm_id stm1);
      output_string chan (DOT.relation stm_id stm2)
    | If (exp, stm1, stm2) ->
(*      (find_tbl expression_tbl exp)#gen_dot (chan, exp);*)
      let cond =
        (find_tbl expression_tbl exp)#to_string ()
      in 
      output_string chan (DOT.statement_node ("IF\\n"^cond) stm_id);
      (find_tbl statement_tbl stm1)#gen_dot (chan, stm1);
      (find_tbl statement_tbl stm2)#gen_dot (chan, stm2);
      output_string chan (DOT.relation stm_id stm1);
      output_string chan (DOT.relation stm_id stm2)(*;
      output_string chan (DOT.relation stm_id exp)*)
    | While (exp, stm)     ->
(*      (find_tbl expression_tbl exp)#gen_dot (chan, exp);*)
      let cond =
        (find_tbl expression_tbl exp)#to_string ()
      in 
      output_string chan (DOT.statement_node ("WHILE\\n"^cond) stm_id);
      (find_tbl statement_tbl stm)#gen_dot (chan, stm);
      output_string chan (DOT.relation stm_id stm)(*x;
      output_string chan (DOT.relation stm_id exp)*)
    | DoWhile (exp, stm)   ->
      (* (find_tbl expression_tbl exp)#gen_dot (chan, exp); *)
      let cond =
        (find_tbl expression_tbl exp)#to_string ()
      in 
      output_string chan (DOT.statement_node ("DOWHL\\n"^cond) stm_id);
      (find_tbl statement_tbl stm)#gen_dot (chan, stm);
      output_string chan (DOT.relation stm_id stm)(*;
      output_string chan (DOT.relation stm_id exp)*)
    | For (exp1, exp2, exp3, stm) ->
      (*(find_tbl expression_tbl exp1)#gen_dot (chan, exp1);
      (find_tbl expression_tbl exp2)#gen_dot (chan, exp2);
      (find_tbl expression_tbl exp3)#gen_dot (chan, exp3);*)
      let cond = 
      (find_tbl expression_tbl exp1)#to_string () ^ ":" ^
      (find_tbl expression_tbl exp3)#to_string () ^ ":" ^
      (find_tbl expression_tbl exp2)#to_string ()
      in
      output_string chan (DOT.statement_node ("FOR\\n"^cond) stm_id);
      (find_tbl statement_tbl stm)#gen_dot (chan, stm);
      output_string chan (DOT.relation stm_id stm)
(*      output_string chan (DOT.relation stm_id exp1);
      output_string chan (DOT.relation stm_id exp2);
      output_string chan (DOT.relation stm_id exp3)*)
    | Break                       -> 
      output_string chan (DOT.statement_node "BRK" stm_id)
    | Continue                    -> 
      output_string chan (DOT.statement_node "CNTN" stm_id)
    | Return exp                  ->                            (*TODO TRAITER LES OPERATIONS DIFFEREMMENT COMME LE FOR PAR EXEMPLE*)
      (find_tbl expression_tbl exp)#gen_dot (chan, exp);
      output_string chan (DOT.statement_node "RET" stm_id);
      output_string chan (DOT.relation stm_id exp)
    | Switch (exp, stm)           ->
      (find_tbl expression_tbl exp)#gen_dot (chan, exp);
      output_string chan (DOT.statement_node "SWTCH" stm_id);
      (find_tbl statement_tbl stm)#gen_dot (chan, stm);
      output_string chan (DOT.relation stm_id stm);
      output_string chan (DOT.relation stm_id exp)
    | Case (exp, stm)             ->
      (find_tbl expression_tbl exp)#gen_dot (chan, exp);
      output_string chan (DOT.statement_node "CASE" stm_id);
      (find_tbl statement_tbl stm)#gen_dot (chan, stm);
      output_string chan (DOT.relation stm_id stm);
      output_string chan (DOT.relation stm_id exp)
    | Default stm                 -> 
      output_string chan (DOT.statement_node "DFLT" stm_id);
      (find_tbl statement_tbl stm)#gen_dot (chan, stm);
      output_string chan (DOT.relation stm_id stm)
    | Label (str, stm) -> 
      output_string chan (DOT.statement_node "LABEL" stm_id);
      (find_tbl statement_tbl stm)#gen_dot (chan, stm);
      output_string chan (DOT.relation stm_id stm);
      output_string chan (DOT.relation stm str)
    | Goto str         -> 
      output_string chan (DOT.statement_node "GOTO" stm_id);
      output_string chan (DOT.relation stm_id str)
    | Asm str          -> 
      output_string chan (DOT.statement_node "ASM" stm_id);
      output_string chan (DOT.relation stm_id str)


end;;
(** Add a IR statement in the statement hash table *)
let add_statement (idx:string) (stm:irstatement) = Hashtbl.add statement_tbl idx stm 
(******************************************************************************)
(******************************************************************************)












(********************************************************)
(** Intermediate representation of a function prototype *)
(********************************************************)
class irprototype ((boo:bool), (snl:irsinglename list), (str:string)) =
object (self)

  val proto_bool = boo
  val proto_snl  = snl
  val proto_func = str

  method get_function_name = proto_func
  method get_argument_list = proto_snl

  method to_string () =
    let str = ref "" in
    if List.length snl > 0 then 
    begin
      let arg sn = (sn#type_to_string ()) ^ " " ^ sn#name_to_string () in
      str := "(" ^ (arg (List.hd snl)) ;
      List.iter (fun i -> str := !str ^ ", " ^ arg i) (List.tl proto_snl);
    end  
    else str := "(";
    !str ^ ")" 

end;;
(** Add a IR prototype in the prototype hash table *)
let add_proto (idx:string) (pro:irprototype) = Hashtbl.add proto_tbl idx pro
(******************************************************************************)
(******************************************************************************)



(**********************************************)
(** Intermediate representation of a function *)
(**********************************************)
class irfunction ((nme:irsinglename), (bdy:string)) =
object (self)

  val function_name = nme
  val function_body = bdy

  method get_name = function_name#name_to_string ()
  method get_body = function_body

 
  method to_string () =
    "#function: " ^
    function_name#to_string () ^ "\n" ^ 
    (find_tbl body_tbl function_body)#to_string ()


  method gen_c (chan:out_channel) =
    output_string chan TOC.newline;
    function_name#gen_c (chan);
    (find_tbl body_tbl function_body)#gen_c (chan);
    output_string chan TOC.newline 

  method gen_dot (chan:out_channel) =
    output_string chan (DOT.begin_function self#get_name);
    (find_tbl body_tbl function_body)#gen_dot (chan, function_body);
    output_string chan (DOT.end_function self#get_name)


end;;
let add_function (idx:string) (fct:irfunction) = Hashtbl.add function_tbl idx fct
(******************************************************************************)
(******************************************************************************)



(******************************************)
(** Intermediate representation of a body *)
(******************************************)
class irbody ((def:irdefinition list), (stm:string)) =
object (self)

  val mutable body_def = def
  val body_stm = stm

  method get_body_dcl = body_def
  method get_body_stm = body_stm

  method add_body_dcl dcl =
    body_def <- body_def @ [(dcl)] 

  method get_copy () =
    let stm_body = (find_tbl statement_tbl body_stm) in
    let cp = new_id idx in (*copy def list*)
    Hashtbl.add body_tbl cp (new irbody (body_def, stm_body#get_copy ()));
    cp


  method to_string () = 
    let str = ref "" in
    List.iter (fun i -> str := !str ^ i#to_string () ^ "\n") body_def;
    !str ^ "\n" ^ (find_tbl statement_tbl body_stm)#to_string ()

  method gen_c (chan:out_channel) =
    output_string chan (TOC.begin_block "");
    TOC.add_tab 1;
    List.iter (fun i -> i#gen_c (chan)) body_def;
    (find_tbl statement_tbl body_stm)#gen_c (chan);
    TOC.sub_tab 1;
    output_string chan (TOC.end_block "")


  method gen_dot ((chan:out_channel), (stm_id:string)) =
    List.iter (fun i -> i#gen_dot (chan)) body_def;
    output_string chan (DOT.block_node stm_id);
    output_string chan (DOT.relation stm_id body_stm);
    (find_tbl statement_tbl body_stm)#gen_dot (chan, body_stm)

end;;
(** Add a IR body in the body hash table *)
let add_body (idx:string) (bod:irbody) = Hashtbl.add body_tbl idx bod 
(******************************************************************************)
(******************************************************************************)



(******************************************)
(** Intermediate representation of a file *)
(******************************************)
class irfile ((nme:string), (dcl:irdefinition list)) =
object (self)

  val file_name    = nme
  val mutable file_dcl     = dcl

  method get_name = file_name
  method get_file_dcl = file_dcl

  method add_dcl dcl = file_dcl <- [dcl] @ file_dcl

  method to_string () =
    let decl = ref "" in
    List.iter (fun var -> decl := !decl ^ (var#to_string ()) ^ "\n\n") file_dcl;
    "###file:" ^ file_name ^ "\n\n" ^
    "###globals:" ^ "\n" ^
    !decl
 

  method gen_c (chan:out_channel) =
    output_string chan TOC.header;
    output_string chan (TOC.includes file_name);
    List.iter (fun var -> var#gen_c (chan)) file_dcl;


  method gen_dot (chan:out_channel) =
    output_string chan (DOT.header file_name);
    List.iter (fun var -> var#gen_dot (chan)) file_dcl;
    output_string chan DOT.footer

end;;
(******************************************************************************)
(******************************************************************************)
