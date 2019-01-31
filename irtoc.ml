module CF = Config
module CMD = Commandline

open Libcommon


let warning msg = msg

let t = ref 0
let add_tab i =
  t := !t + i
let sub_tab i = 
  t := !t - i

let print_tabs i = (*a tab is 4 spaces *)
  let res = ref "" in
  for j=1 to i do res := !res ^ "    " done;
  !res 


let header =
  "/* \n \
    * \n \
    * cohd version: " ^ CF.version ^ "\n \
    * \n \
    */\n\n"


let nothing   = ""
let newline   = "\n"
let semicolon = ";"

let flush line =

  let rec split_line acc str =

    let nb_char = (!t * 4) + String.length str - 1 in
    if nb_char > 80 then
      begin

      let re = Str.regexp "[' ' ',']" in
   
      try

      split_line (acc ^ Str.string_before str (Str.search_backward re str (80-(!t*4))) ^ "\n" ^ print_tabs !t)
                 ("  " ^ Str.string_after str (Str.search_backward re str (80-(!t*4))))

      with Not_found -> acc ^ str
      end
    else acc ^ str
  in
 

  split_line "" line






let begin_block str = (print_tabs !t) ^ str ^ "{" ^ newline
let end_block _     = (print_tabs !t) ^       "}" ^ newline

let begin_struct str = 
  add_tab 1;
  "struct " ^ str ^ " {" ^ newline
let end_struct _     = 
  sub_tab 1;
  (print_tabs !t) ^ "}"

let begin_union str = 
  add_tab 1;
  "union " ^ str ^ " {" ^ newline
let end_union _     = 
  sub_tab 1;
  (print_tabs !t) ^ "}"


let begin_enum _ = "enum " ^ "{"
let end_enum str = "} " ^ str

let enum_item nme vle = 
  nme ^ (if ((String.length vle) > 0) then (" = " ^ vle) else "") ^ ", "

let break    _ = (print_tabs !t) ^ "break"    ^ semicolon ^ newline
let continue _ = (print_tabs !t) ^ "continue" ^ semicolon ^ newline

let typedef _ = (print_tabs !t) ^ "typedef "


let label str = (print_tabs !t) ^ str ^ ":" ^ newline
let goto  str = (print_tabs !t) ^ "goto " ^ str ^ semicolon ^ newline

let asm str = (print_tabs !t) ^ str


let var_string vtype vname vval =
  let name = ref vname in
  let value = if vval <> "" then " = " ^ vval else vval in
  let typ = if String.contains vtype '[' then begin
    let str =  ref "" in
    str := String.sub vtype 0 ((String.length vtype));
    str := Libcommon.insert_before (String.index !str '[') (" "^vname) !str;
    name := "";
    !str
  end else vtype in
  typ ^ " " ^ !name ^ value


let var_bitfield vtype vname =
  Libcommon.insert_before (String.index vtype ':') vname vtype


let variable s var = 
  if String.length s <> 0 then
  (print_tabs !t ) ^ flush (s ^ " " ^ var) ^ semicolon ^ newline
  else
  (print_tabs !t ) ^ flush var ^ semicolon ^ newline


let computation comp =
  (print_tabs !t) ^ flush comp ^ semicolon ^ newline

let condition_then cond =
  (print_tabs !t) ^ "if(" ^ cond ^ ")\n"

let condition_else_begin s = (print_tabs !t) ^ "else\n"

let loop_while loop =
  (print_tabs !t) ^ "while(" ^ loop ^ ")\n"

let loop_dowhile_begin s = (print_tabs !t) ^ "do\n"
let loop_dowhile_end loop =
  (print_tabs !t) ^ "while(" ^ loop ^ ") ;\n"

let loop_for exp1 exp2 exp3 =
  (print_tabs !t) ^ "for(" ^ exp1 ^ "; " ^ exp2 ^ "; " ^ exp3 ^ ")\n"
  


let switch exp = (print_tabs !t) ^ "switch ( " ^ exp ^ " )\n"
let case exp = (print_tabs !t) ^ "case " ^ exp ^ " :\n"
let default s = (print_tabs !t) ^ "default :\n"

let return exp =
  (print_tabs !t) ^ "return " ^ exp ^ semicolon ^ newline


let function_header fstor ftype fname fval =
  fstor ^ ftype ^ " " ^ fname ^ fval ^ "\n"









let (call_list:string list ref) = ref []

let add_call file =
  let chan = open_in file in
  let detect_include line =
    try (
      if Str.string_match (Str.regexp "#include") line (String.index line '#') 
      then call_list := line :: !call_list
    ) with Not_found -> ()
  in
  let rec read_file ch =
    try (
      let line = input_line ch in
      detect_include line;
      read_file ch
    )
    with End_of_file -> ()
  in
  read_file chan;
  close_in chan


let includes file =
  add_call file;
  let str = ref "" in
  List.iter (fun i -> str := !str ^ "\n" ^ i) !call_list;
  !str ^ 
  (if !CMD.fma then "\n#include <math.h>" ^ "\n" else "") ^ 
  (if !CMD.p_performances then "\n#include <papi.h>" ^ "\n" else "") ^ 
  "\n\n"
