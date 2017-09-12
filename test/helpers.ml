let ser_prelude_list =
  let aux prefix l =
    ( "Coq" :: l
    , Coq_config.coqlib ^ "/" ^ prefix ^ "/" ^ String.concat "/" l
    , true
    )
  in
  List.map (aux "plugins") Sertop_prelude.coq_init_plugins
  @ List.map (aux "theories") Sertop_prelude.coq_init_theories

[@@@ocaml.warning "-42"] 
let init () =
  Sertop_init.coq_init
    { Sertop_init.fb_handler = (fun _ -> ())
    ; iload_path = ser_prelude_list
    ; require_libs = [Sertop_prelude.coq_prelude_mod Coq_config.coqlib]
    ; implicit_std = false
    ; aopts =
        { Sertop_init.enable_async = None
        ; async_full = false
        ; deep_edits = false
        }
    ; top_name = "Top"
    ; ml_load = None
    }

open Serapi_protocol

let process_error = function
  | [] ->
     failwith "No answer."
  | CoqExn (_, _, CErrors.UserError (_, pp)) :: _ ->
     failwith (Pp.string_of_ppcmds pp)
  | CoqExn (_, _, ExplainErr.EvaluatedError (pp, _)) :: _ ->
     failwith (Pp.string_of_ppcmds pp)
  | CoqExn (_, _, exn) :: _ ->
     raise exn
  | _ ->
     failwith "Unknown answer"

let rec process_add_answers = function
  | [ Completed ] -> []
  | Added (id, loc, _) :: answers -> (id, loc) :: process_add_answers answers
  | error -> process_error error

let process_exec_answers = function
  | [ Completed ] -> ()
  | error -> process_error error

let exec_script script =
  let id = init () in
  let ids =
    exec_cmd
      (Add
         ( { lim = None ; ontop = Some id ; newtip = None ; verb = false }
         , script
         )
      )
    |> process_add_answers
  in
  begin match ids with
  | [] -> ()
  | _ ->
     exec_cmd (Exec (ids |> List.rev |> List.hd |> fst))
     |> process_exec_answers
  end;
  ids

let process_goal_query_answer = function
  | [ ObjList [ CoqGoal goals ] ; Completed ] -> goals
  | error -> process_error error

let query_goal id =
  (Query
     ( { preds = []
       ; limit = None
       ; sid = id
       ; pp =
           { pp_format = PpSer; pp_depth = 0; pp_elide = "..."; pp_margin = 72 }
       ; route = 0
       }
     , Goals
     )
  )
  |> exec_cmd
  |> process_goal_query_answer

(*
(* Proof tree is actually a rooted alternated DAG because of multigoal tactics *)
type prooftree = action_on_goals list
and action_on_goals = { active_goals : goal_info list; action: action }
and goal_info = { goal : Goal.goal; solved : bool }
and action = Tactic of Goal.goal list * tactic_info | Focus of prooftree
(* The Unfocus command is not supported *)
and tactic_info = {
  tactic : Pp.std_ppcmds;
  with_end_tac : bool
}
 *)
