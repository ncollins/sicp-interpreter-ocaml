open Core

type t =
  [ `Exp of Exp.t
  | `Function of ((Symbol.t list) * Exp.t * (t Env.t))
  | `Builtin of (t list -> t)
  (* Used for non-derterministic interpreter *)
  | `Failed
  ]
[@@deriving sexp]
