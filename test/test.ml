(* ocamlbuild -use-ocamlfind -j 4 -pkgs 'sexplib,dynlink,camlp5.gramlib,coq.ltac,coq.stm,coq.idetop' -tag 'linkall' -I sertop test/test.native && ./test.native *)

open Helpers

let input_script =
  "Goal forall n m, n + m = m + n. induction n. (* comment *) all: cycle 1. all: cycle 1. { easy. } idtac. Abort."

let print_goals = function
  | None ->
     print_endline "\t\t(* not in proof mode *)"
  | Some goals ->
     let open Proof in
     let open Serapi_goals in
     "\t\t(* focussed goals: " ^ (
       goals.fg_goals
       |> List.map (fun goal -> goal.name)
       |> String.concat ", "
     ) ^ " *)"
     |> print_endline

let ids_and_locs = exec_script input_script

let sentences_and_goals =
  ids_and_locs
  |> List.map
       (fun (id, loc) ->
         ( Loc.(String.sub input_script loc.bp (loc.ep - loc.bp))
         , query_goal id
         )
       )

let _ =
  sentences_and_goals
  |> List.iter
       (fun (sentence, goals) -> print_string sentence; print_goals goals)
