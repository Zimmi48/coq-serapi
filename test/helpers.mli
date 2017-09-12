val exec_script : string -> (Stateid.t * Loc.t) list
val query_goal :
  Stateid.t -> Constr.t Serapi_goals.reified_goal Proof.pre_goals option
val script_with_goal_info :
  string -> (string * (string list * string list) option) list
