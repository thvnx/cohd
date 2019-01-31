module CL = Commandline
module AST = Irtree
module HP = Highprec
module TAC = Tac
module TO = Tradeoff
module PP = Preprocess


let launch_passes (file:AST.irfile) =
   if CL.preprocess () then 
   begin PP.launch_pass file end;   

   if !CL.tac then 
   begin TAC.launch_pass file end;

   if !CL.print_intermediate_results then
   begin
     TAC.launch_pass file;
     PP.print_intermediate_results file;
     (*HP.launch_pass file *)
   end; 

(*else
   begin*)
     if CL.tradeoff () then 
       begin
       TAC.launch_pass file;
       TO.launch_pass file;
       let inter = TO.get_inter_stm () in
       let loop_pos = !TO.loop_nop_id in
       HP.set_inter inter;
       HP.set_loop_pos loop_pos;
       HP.launch_pass file
     end
     else 
     begin
     if !CL.high_double then begin 
       TAC.launch_pass file;
       HP.launch_pass file
     end;
   end;
  (* end *)

  if !CL.to_prec then
    begin
       TAC.launch_pass file;
       HP.launch_pass file   
    end
