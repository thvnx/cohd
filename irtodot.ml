
module CF = Config


let font = "Helvetica"



let header name =
  "#cohd version: " ^ CF.version ^ "\n\
   digraph \"" ^ name ^ "\" {\n\
   graph [fontname=" ^ font ^ ", fontsize=10, bgcolor=transparent, ordering=out];\n\
   node [fontname=" ^ font ^ ", fontsize=8, ordering=out];\n\
   edge [fontname=" ^ font ^ ", fontsize=8, arrowsize=0.75];\n\n"  

let footer =
  "}\n"


let begin_function name =
  "subgraph \"cluster" ^ name ^ "\" {\n"


let end_function name =
  "label = \"" ^ name ^ "\";\
   \n\
   }\n\n"


let statement_node str id =
  "" ^ id  ^ " [label=\"" ^ str ^ "\" shape=diamond style=filled color=gray fillcolor=gray];\n"


let expression_node str id =
  "" ^ id  ^ " [label=\"" ^ str ^ "\" shape=circle style=filled color=lightgray fillcolor=lightgray];\n"

let terminal_expression_node str id =
  "" ^ id  ^ " [label=\"" ^ str ^ "\" shape=circle style=filled color=gray fillcolor=whitesmoke];\n"


let block_node id =
  "" ^ id  ^ " [label=BLCK shape=rectangle style=filled color=gray fillcolor=slategray];\n"


let relation src dst =
  src ^ " -> " ^ dst ^ ";\n"
