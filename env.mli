
type t

val empty : unit -> t

val var_in_env : t -> Exp.A.t -> bool

val lookup_ex : t -> Exp.A.t -> Exp.t

val bind : t -> Exp.A.t -> Exp.t -> unit

val set : t -> Exp.A.t -> Exp.t -> unit

val extend : t -> ((Exp.A.t * Exp.t) list) -> t
