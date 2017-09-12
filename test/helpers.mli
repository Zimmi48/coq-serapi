val exec_script : string -> (Stateid.t * Loc.t) list
val query_goal : Stateid.t -> Constr.t Serapi_goals.reified_goal Proof.pre_goals
