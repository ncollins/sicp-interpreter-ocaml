open Core

module Value = struct

  type t =
    [ `Int of int
    | `String of string
    | `Bool of bool
    ]

end

module Variable = struct

  type t = [ `Variable of string ] 

end

module Env = struct

  type t = (Variable.t, Value.t) Hashtbl.Poly.t list

end

module Exp = struct

  (* TODO this is not good! *)
  type t = Env.t * (Value.t list)

end


type succeed = unit
type fail = unit

type r = unit

module Analyze : sig

  val analyze_if :
    Exp.t ->
    (Env.t -> succeed -> fail -> r)

end = struct

  let analyze_if (_exp : Exp.t) =
    fun (_env : Env.t) (_s : succeed) (_f : fail) ->
      ()
  ;;



end


let () =
  printf !"%{sexp:string}\n" Continuation.test_string
;;

let () =
  Sexp.to_string_hum [%sexp ([3;4;5] : int list)]
  |> print_endline
