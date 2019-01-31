(******************************************************************************)
(* History *********************************************************************

- September 25 2013 - revision 1.0 : first revision


******************************************************************** /History *)
(******************************************************************************)

(** Scan the command line and set some program variables. 
    Use the Arg module of ocaml *)

open Config

(******************************************************************************)

(** Text printed if there is something wrong with the command arguments. *)
let usage_text = 
  "usage :\tcohd <file> [options...]\n" ^
  "\nCo-HD version : " ^ Config.version ^ "\n\n"


let files:string list ref = ref []         (** C files list from command line *)
let out_file = ref ""                                  (** Result C file name *)

let dot = ref false                               (** Output AST to DOT graph *)
                                                     (** Output DOT file name *)
let dot_file _ = 
  (String.sub !out_file 0 (String.rindex !out_file '.') ^ ".dot")


let verbose = ref false

let fma = ref false                                (** TwoProduct can use FMA *)
let high_double   = ref false      (** If true then launch HighPrecision pass *)
                       (** High precision operators are Double-Word operators *)
let double_double = ref false

let tac = ref false           (** If true then launch three-address code pass *)
let p_iterator = ref 1                    (** Preprocess performances pragmas *)
let p_performances = ref false            (** Preprocess performances pragmas *)
let p_accuracy = ref false                    (** Preprocess accuracy pragmas *)
                                      (** If true then launch preprocess pass *)
let preprocess _ = !p_performances || !p_accuracy

let print_intermediate_results = ref false (** Print all FP intermediate 
                                                                      results *)
let names = ref []                         (** List of variables to transform *)

let to_block = ref false      (** If true then launch the block tradeoff pass *)
let to_alter = ref false(** If true then launch the alternation tradeoff pass *)
let to_prec  = ref false       (** If true then launch accuracy tradeoff pass *)
let tradeoff _ = !to_block || !to_alter

                                               (** Tradeoff passes parameters *)
let to_num = ref 1 
let to_den = ref 2
let to_first = ref true
let to_prop  = ref true


(******************************************************************************)

let scan_options = Arg.align
[
  ("-o"  , Arg.Set_string out_file, "<output> Set the output in the given file \
                                                                     <output>");

  ("-dot", Arg.Set dot            , " Output the AST graph");

  ("-v"  , Arg.Set verbose        , " Be verbose");

  ("-pp" , Arg.Set p_performances , " Process performances #pragma");
  ("-pa" , Arg.Set p_accuracy     , " Process accuracy #pragma");
  ("-pi" , Arg.Set print_intermediate_results 
                                  , " Print all FP intermediate results");
  ("-pn" , Arg.Set_int p_iterator , "<x> Set the number of the papi loop \
                                                                    iteration");

  ("-fma", Arg.Set fma            , " Enable TwoProductFMA");
  ("-dd" , Arg.Set double_double  , " Set Double-Word operators as the default \
                                                    High-Definition operators");

  ("-tac", Arg.Set tac            , " Synthetize AST in a 3-address code form");
  ("-hd" , Arg.Set high_double    , " Synthetize AST with High-Definition \
                                                                   arithmetic");

  ("-t0" , Arg.Unit (fun () -> 
    to_block := true; 
    to_alter := false)            , " Enable accuracy and execution-time \
                                                             tradeoff (block)");
  ("-t1" , Arg.Unit (fun () ->  
    to_alter := true;
    to_block := false)            , " Enable accuracy and execution-time \
                                                       tradeoff (alternation)");
  ("-t2" , Arg.String (
    fun x -> 
      to_prec := true;
      names := Str.split (Str.regexp "[,]+") x
  )                               , "<x,y,z,...> Enable manual precision \
                                            tradeoff of <x y z ...> variables");

  ("-n"  , Arg.Set_int to_num     , "<x> Set the iterations numerator ratio to \
                                                              optimize to <x>");
  ("-m"  , Arg.Set_int to_den     , "<x> Set the iterations denominator ratio \
                                                           to optimize to <x>");
  ("-p"  , Arg.Clear to_prop      , " Do not propagate High-Definition words");
  ("-f"  , Arg.Set to_first       , " Apply High-Definition operators to the \
                                                                     begining");
  ("-e"  , Arg.Clear to_first     , " Apply High-Definition operators to the \
                                                                       ending");
]


(** Scan the command line. *)
let scan_cmd_line =
  Arg.parse scan_options 
    (fun file -> files := file :: !files) usage_text;
  if(!files = []) then 
    begin
      Arg.usage scan_options usage_text;
      exit 2;
    end
