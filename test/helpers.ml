let ser_prelude_list =
  let aux prefix l =
    ( "Coq" :: l
    , Coq_config.coqlib ^ "/" ^ prefix ^ "/" ^ String.concat "/" l
    , true
    )
  in
  List.map (aux "plugins") Sertop_prelude.coq_init_plugins
  @ List.map (aux "theories") Sertop_prelude.coq_init_theories


let require_libs =
  let open Names in
  [
    ( List.rev_map Id.of_string ["Coq";"Init";"Prelude"]
      |> DirPath.make
    , Coq_config.coqlib ^ "/theories/Init/Prelude.vo"
    , Some true
    )
  ;
    ( List.rev_map Id.of_string ["Coq";"Init";"Notations"]
      |> DirPath.make
    , Coq_config.coqlib ^ "/theories/Init/Notations.vo"
    , None
    )
  ]

[@@@ocaml.warning "-42"]
let init () =
  Sertop_init.coq_init
    { Sertop_init.fb_handler = (fun _ -> ())
    ; iload_path = ser_prelude_list
    ; require_libs = require_libs
    ; implicit_std = true
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
  | [ ObjList [ CoqGoal goals ] ; Completed ] -> Some goals
  | [ ObjList [] ; Completed ] -> None
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

let flatten_goals (goals : 'a Proof.pre_goals) : 'a list =
  let open Proof in
  List.fold_left
    (fun goals (before, after) -> before @ goals @ after)
    goals.fg_goals
    goals.bg_goals

let script_with_goal_info script =
  let rec aux goals_before = function
    | [] ->
       []
    | (id, loc) :: t ->
       let goals_after =
         query_goal id
         |> Option.lift
              (fun g ->
                flatten_goals g
                |> List.map (fun goal -> goal.Serapi_goals.name))
       in
       let solved_and_new_goals =
         Option.lift2
           (fun goals_before goals_after ->
             ( List.filter (fun g -> not (List.mem g goals_after)) goals_before
             , List.filter (fun g -> not (List.mem g goals_before)) goals_after
             )
           )
           goals_before goals_after
       in
       ( Loc.(String.sub script loc.bp (loc.ep - loc.bp))
       , solved_and_new_goals
       ) :: aux goals_after t
  in
  exec_script script
  |> aux None
