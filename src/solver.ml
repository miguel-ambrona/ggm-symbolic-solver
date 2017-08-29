(* Main function of the tool *)

open Abbrevs
open Util
open Expressions
open Equations
open PrintLatex
open Sage

let rec analyze () =
  let input = Pervasives.read_line () in
  (try
     let conjs, sets, atoms, goal = Eval.eval_instrs (Parse.p_cmds input) in
     (let parameters = L.filter atoms ~f:(function | Matrix(_,Param,_,_,_) -> true | _ -> false) in
      let variables  = L.filter atoms ~f:(function | Matrix(_,Var,_,_,_) -> true | _ -> false) in
      F.printf "\\begin{aligned} & \\text{sets:} && ";
      (match sets with
       | [] -> F.printf "\\emptyset\\\\"
       | _  -> F.printf "%s\\\\" (string_of_list ", \\ " string_of_set_latex sets)
      );
      (match parameters with
       | [] -> F.printf " & \\text{parameters:} && \\emptyset\\\\"
       | _  -> F.printf " & \\text{parameters:} && %a \\\\ " pp_group_of_atoms parameters
      );
      (match variables with
       | [] -> F.printf " & \\text{variables:} && \\emptyset\\\\"
       | _  -> F.printf " & \\text{variables:} && %a \\\\ " pp_group_of_atoms variables
      );
      if (L.length conjs) > 0 then
        F.printf " \\\\ & \\text{goal }%d \\text{ out of } %d \\end{aligned}@" (goal+1) (L.length conjs)
      else
        F.printf " \\\\ & \\text{no goals} \\end{aligned}@"
     );
     let conj =
       match L.nth conjs goal with
       | None ->
          if goal = 0 then { conj_exists = []; conj_equations = []; conj_substituted = []; conj_replaced = []; }
          else failwith ("Goal " ^ (string_of_int (goal+1)) ^ " does not exist")
       | Some conj -> conj
     in
     F.printf "%a" pp_conj_latex conj;
     F.printf "\n";
     F.print_flush();
   with
   | err -> F.printf "%s\n" (trim_newlines (Printexc.to_string err)); F.print_flush()
  );
  analyze ()

let main =
  let _ = call_Sage "{'cmd':'GroebnerBasis', 'system':[]}\n" in (* Initialize Sage *)
  let man = F.sprintf "usage: %s\n" Sys.argv.(0) in
  if Array.length Sys.argv > 1 then
    output_string stderr man
  else
    analyze()
