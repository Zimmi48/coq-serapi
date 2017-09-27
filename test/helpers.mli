val exec_script : string -> (Stateid.t * Loc.t) list

val query_goal :
  Stateid.t -> Constr.t Serapi_goals.reified_goal Proof.pre_goals option

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

val coq_document_of_script : string -> unstructured_script coq_document

val script_with_goal_info :
  string -> (string * (string list * string list) option) list
