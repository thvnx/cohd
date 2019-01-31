(******************************************************************************)
(* History *********************************************************************

- September 25 2013 - revision 1.0 : first revision


******************************************************************** /History *)
(******************************************************************************)

(** Main code for Co-HD program. *)

open Frontc

module CMD = Commandline
module BLD = Irbuilder
module PSS = Passes


(******************************************************************************)

let _ =
  CMD.scan_cmd_line;                             (** Process the command line *)


  (**********************************************************)
  (** Convert Frontc AST to my intermediate representation. *)
  let process options filename =
    match Frontc.parse options with
      | PARSING_ERROR  -> ()
      | PARSING_OK ast -> BLD.build ast filename 
    in
    (*** Info : see Frontc to get details about options for Frontc.parse *)
    List.iter (
      fun file -> process [(FROM_FILE file); (LINE_RECORD true)] file
    ) !CMD.files;


  (** Print the AST (in verbose mode *)
  if(!CMD.verbose) then 
    List.iter (fun f -> print_string (f#to_string ())) !BLD.files;

  (** Launch compiler passes *)
  List.iter (fun f -> PSS.launch_passes f ) !BLD.files;


  (*************************)
  (** Output AST to C code *)
  if(!CMD.out_file <> "") then 
  begin
    let chan = open_out !CMD.out_file in
    List.iter (fun f -> (f#gen_c (chan))) !BLD.files;
    close_out chan
  end;


  (***************************)
  (** Output AST to DOT code *)
  if !CMD.dot then begin
    let chan = open_out (CMD.dot_file()) in
    List.iter (fun f -> (f#gen_dot (chan))) !BLD.files;
    close_out chan
  end

