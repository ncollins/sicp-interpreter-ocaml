open Core

type t =
  [ `Int of int
  | `String of string
  | `Symbol of Symbol.t
  | `List of t list
  ]
[@@deriving sexp]

let rec code_sexp_of_t = function
  | `Int i -> Int.sexp_of_t i
  | `String s -> Tuple2.sexp_of_t String.sexp_of_t String.sexp_of_t ("string", s)
  | `Symbol s -> Symbol.sexp_of_t s
  | `List l -> List.sexp_of_t code_sexp_of_t l
;;

let rec t_of_int_sexp s =
  try Some (`Int (Int.t_of_sexp s)) with
  | _ -> None 

and t_of_string_sexp s =
  let open Option.Let_syntax in
  let%bind (tag, s) =
    try Some (Tuple2.t_of_sexp String.t_of_sexp String.t_of_sexp s) with
    | _ -> None
  in
  if String.equal tag "string"
  then Some (`String s)
  else None

and t_of_symbol_sexp s =
  try (Some (`Symbol (Symbol.t_of_sexp s))) with
  | _ -> None

and t_of_list_sexp s =
  try Some (`List (List.t_of_sexp t_of_code_sexp s)) with
  | _ -> None

and t_of_code_sexp s =
  let t_opt =
    List.fold
      ~f:Option.first_some
      ~init:(t_of_int_sexp s)
      [ t_of_string_sexp s
      ; t_of_symbol_sexp s
      ; t_of_list_sexp s
      ]
  in
  if Option.is_some t_opt
  then Option.value_exn t_opt
  else raise_s [%sexp "Failed to parse", (s : Sexp.t)]
;;
