(* SerAPI initialization boilerplate *)

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

let init () =
  let open Sertop_init in
  coq_init
    { fb_handler = (fun _ -> ())
    ; iload_path = ser_prelude_list
    ; require_libs = require_libs
    ; implicit_std = true
    ; aopts =
        { enable_async = None
        ; async_full = false
        ; deep_edits = false
        }
    ; top_name = "Top"
    ; ml_load = None
    }

(* Interesting stuff starts here *)

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

type 'a coq_document_part
  = Script of 'a
  | Command of (Stateid.t * Loc.t)

(* a refactoring function will generally act on this *)
type unstructured_script =
  { proof_start
    : Stateid.t * Loc.t * Constr.t Serapi_goals.reified_goal Proof.pre_goals
  ; proof_script
    : (Stateid.t * Loc.t * Constr.t Serapi_goals.reified_goal Proof.pre_goals)
        list
  ; proof_end : Stateid.t * Loc.t
  }

type 'a coq_document = { script : string ; parts : 'a coq_document_part list }

let coq_document_of_script script =
  let rec proof_script = function
    | [] ->
       failwith "Unfinished proof script (not yet supported)."
    | (id, loc) :: t ->
       match query_goal id with
       | None ->
          ([], (id, loc), t)
       | Some goals ->
          let (proof_script, proof_end, t) = proof_script t in
          ( (id, loc, goals) :: proof_script, proof_end, t)
  in
  let rec coq_document = function
    | [] ->
       []
    | (id, loc) :: t ->
       match query_goal id with
       | None ->
          Command (id, loc) :: coq_document t
       | Some goals ->
          let (proof_script, proof_end, t) = proof_script t in
          Script { proof_start = (id, loc, goals) ; proof_script ; proof_end }
          :: coq_document t
  in
  { script = script ; parts = exec_script script |> coq_document }
(*
type proof_action = Focus of proof_action list | Tactic of string

type focussed_script =
  { proof_start : Stateid.t * Loc.t
  ; proof_script : proof_action list
  ; proof_end : Loc.t
  }
*)

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
