
type 'a t [@@deriving sexp]

val empty : unit -> 'a t

val var_in_env : 'a t -> Symbol.t -> bool

val lookup : 'a t -> Symbol.t -> 'a option
val lookup_ex : 'a t -> Symbol.t -> 'a

val bind : 'a t -> Symbol.t -> 'a -> unit

val set : 'a t -> Symbol.t -> 'a -> unit

val extend : 'a t -> ((Symbol.t * 'a) list) -> 'a t
