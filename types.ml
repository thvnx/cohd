


(** Location *)
type location = 
  | NoLocation                (** Unknown *) 
  | Location of string * int  (** Located in (file, line) *)

(** Type *)
type ttype = 
  | Type of string            (** Just a link toward the true type *)
  | Void                      (** "void" *)
  | Char of string            (** Type "char" with sign modifier *)
  | Int of string * string    (** Type "int" with (size, sign) modifier *) 
  | BitField of string        (** Bitfiled with identifier *)
  | Float of bool             (** Type "float" with long (true) modifier *)
  | Double of bool            (** Type "double" with long (true) modifier *)
  | Ptr of string             (** Pointer "*" with pointed type identifier *)
  | Array of string * string  (** Array with its identifier and type identifier *)
  | Struct of string          (** Struct with identifier *)
  | Union of string           (** Union with identifier *)
  | Proto of string * string  (** Prototype with its identifier and type identifier *)
  | NamedType of string       (** Named type coming from "typedef" *)
  | Enum of string            (** Enum with identifier *)
  | Const of string           (** "const" modifier with type identifier *)
  | Volatile of string        (** "volatile" modifier type identifier *)

(** Expression *)
type expression = 
  | Exp of string                                (** Just a link toward the true expression *)
  | Nothing                                      (** Nothing, rien, nada, niet *)
  | Unary of (string * bool) * string            (** Unary operator, true if post operator and expression identifier *)
  | Binary of (string * bool) * string * string  (** Binary operator, true if parenthesized computation and expressions identifiers*)
  | Question of string * string * string         (** "cond ? then : else" operator and expressions identifiers *)
  | Cast of string * string                      (** Cast operator with type identifier and expression identifier *)
  | Call of string * string list                 (** Function call and expression identifier *)
  | Comma of string list                         (** Identifiers of expressions separated by a comma *)
  | Constant of string                           (** Constant value *)
  | Variable of string                           (** Variable identifier *)
  | SizeofExpr of string                         (** "sizeof" an expression identifier *)
  | SizeofType of string                         (** "sizeof" a type identifier *)
  | Index of string * string                     (** Acces to an array item *)
  | MemberOf of string * string                  (** Member of an identifier "." and an expression identifier *)
  | MemberOfPtr of string * string               (** Member of an pointer identifier "->" and an expression identifier *) 
  | Body of string                               (** A GNU body identifier inside an expression *)


(** Constant *)
type tconst = 
  | CInt of string            (** Int constant *)
  | CFloat of string          (** Float constant *)
  | CChar of string           (** Char constant *)
  | CString of string         (** String constant *)
  | CExpList of string list   (** Expressions identifiers *)


(** Statement *)
type tstatement = 
  | Stat of string                            (** Just a link toward the true statement *)
  | Nop                                       (** No operation : useful in empty else-part in condition *)
  | Computation of string                     (** An expression *)
  | Block of string                           (** A body *)
  | Sequence of string * string               (** A statement followed by a statement *)
  | If of string * string * string            (** if(expression) then statement else statement *)
  | While of string * string                  (** while(expression) { statement } *)
  | DoWhile of string * string                (** do { statement } while ( expression ) *)
  | For of string * string * string * string  (** for(expression; expression; expression) { statement } *)
  | Break | Continue                          (** "break" or "continue " *)
  | Return of string                          (** "return" expression *)
  | Switch of string * string                 (** switch (expression) { statement } *)
  | Case of string * string                   (** "case" expression : statement *)
  | Default of string                         (** "default" expression : statement *) 
  | Label of string * string                  (** "label :" statement *)
  | Goto of string                            (** "goto" statement *)
  | Asm of string                             (** ASM string *)


(** Definition types *)
type tdefinition = 
  | Function of string      (** Function definition identifier *)
  | Declaration of string   (** Namegroup identifier : declaration of function or variable *)
  | TypeDef of string       (** Namegroup identifier of "typedef" definition *)
  | OnlyTypeDef of string   (** Namegroup identifier of lonely "struct", "union" or "enum" *)

