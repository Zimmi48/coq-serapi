(* ocamlbuild -use-ocamlfind -j 4 -pkgs 'sexplib,dynlink,camlp5.gramlib,coq.ltac,coq.stm,coq.idetop' -tag 'linkall' -I sertop test/test.native && ./test.native *)

open Helpers

let input_script =
  "Goal forall n m, n + m = m + n. induction n. (* comment *) all: cycle 1. all: cycle 1. { easy. } idtac. Abort."

let print_sentence = function
  | (sentence, None) ->
     print_endline sentence
  | (sentence, Some (solved_goals, new_goals)) ->
     print_endline
       ( "(* solved: " ^ String.concat ", " solved_goals ^ " *) " ^ sentence
         ^ " (* new: " ^ String.concat ", " new_goals ^ " *)" )


let _ =
  script_with_goal_info input_script
  |> List.iter print_sentence
