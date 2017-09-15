(* ocamlbuild -use-ocamlfind -j 4 -pkgs 'core_kernel,sexplib,dynlink,camlp5.gramlib,coq.ltac,coq.stm,coq.idetop' -tag 'linkall' -I sertop test/test.native && ./test.native *)

let input_script filename =
  Core_kernel.Std.In_channel.read_lines filename
  |> String.concat "\n"


let print_sentence = function
  | (sentence, None) ->
     print_endline sentence
  | (sentence, Some (solved_goals, new_goals)) ->
     print_endline
       ( "(* solved: " ^ String.concat ", " solved_goals ^ " *) " ^ sentence
         ^ " (* new: " ^ String.concat ", " new_goals ^ " *)" )


let _ =
  if Array.length Sys.argv > 1 then
    input_script Sys.argv.(1)
    |> Helpers.script_with_goal_info
    |> List.iter print_sentence
  else
    exit 1
