
type 'a t [@@deriving sexp]

val empty : unit -> 'a t

val var_in_env : 'a t -> Exp.A.t -> bool

val lookup_ex : 'a t -> Exp.A.t -> 'a

val bind : 'a t -> Exp.A.t -> 'a -> unit

val set : 'a t -> Exp.A.t -> 'a -> unit

val extend : 'a t -> ((Exp.A.t * 'a) list) -> 'a t
