open Core

module T = struct
  type t = string [@@deriving sexp, compare]
end
include T
include Sexpable.To_stringable(T)
include Comparable.Make(T)

let if_ = of_string "if"
let begin_ = of_string "begin"
let set_ = of_string "set!"
let define_ = of_string "define"
let let_ = of_string "let"
let lambda_ = of_string "lambda"

let true_ = of_string "true"
let false_ = of_string "false"
let null_ = of_string "null"
