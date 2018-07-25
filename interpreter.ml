open Core

module A = String_id.Make(struct let module_name = "A" end) ()

module Exp = struct

  type t =
    [ `Int of int
    | `String of string
    | `Atom of A.t
    | `Bool of bool
    | `List of t list
    | `Lambda of ((A.t list) * t)
    | `Define of A.t * t
    | `Set of A.t * t
    | `Begin of t list
    ]
  [@@deriving sexp]

  let rec code_sexp_of_t = function
    | `Int i -> Int.sexp_of_t i
    | `Bool b -> Bool.sexp_of_t b
    | `String s -> String.sexp_of_t s
    | `List l -> List.sexp_of_t code_sexp_of_t l
    | `Lambda (params, body) ->
      Sexp.List
        [ Sexp.of_string "lambda"
        ; List.sexp_of_t String.sexp_of_t params
        ; List.sexp_of_t String.sexp_of_t body
        ]
    | `Define (a, t) ->
      Sexp.List
        [ Sexp.of_string "define"
        ; A.sexp_of_t a
        ; code_sexp_of_t t
        ]
    | `Set (a, t) ->
      Sexp.List
        [ Sexp.of_string "set!"
        ; A.sexp_of_t a
        ; code_sexp_of_t t
        ]
    | `Begin ts ->
      let b = Sexp.of_string "begin" in
      let exps = List.map ~f:code_sexp_of_t ts in
      Sexp.List (b::exps)
  ;;

  let rec t_of_int_sexp s =
    try Some (`Int (Int.t_of_sexp s)) with
    | _ -> None 

  and t_of_bool_sexp s =
    try Some (`Bool (Bool.t_of_sexp s)) with
    | _ -> None 

  and t_of_begin_sexp s =
    match t_of_list_sexp s with
    | Some (`List ((`Atom a)::ts)) ->
      if A.equal a (A.of_string "begin")
      then Some (`Begin ts)
      else None
    | _ -> None

  and t_of_list_sexp s =
    printf !"Trying to parse as list: %{sexp:Sexp.t}\n" s;
    try Some (`List (List.t_of_sexp t_of_code_sexp s)) with
    | _ -> None

  and t_of_atom_sexp s =
    printf !"Trying to parse as atom: %{sexp:Sexp.t}\n" s;
    try (Some (`Atom (A.t_of_sexp s))) with
    | _ -> None

  and t_of_three_part_sexp
      ~keyword
      ~validate_and_build_t
      s
    =
    let open Option.Let_syntax in
    let%bind parts =
      try Some (List.t_of_sexp ident s) with
      | _ -> None
    in
    let%bind (k, f, s) =
      match parts with
      | [ k; f; s ] -> Some (k, f, s)
      | _ -> None
    in
    let%bind _ =
      if String.equal keyword (String.t_of_sexp k)
      then Some ()
      else None
    in
    let%bind first =
      try Some (t_of_code_sexp f) with | _ -> None
    in
    let%bind second =
      try Some (t_of_code_sexp s) with | _ -> None
    in
    try Some (validate_and_build_t (first, second)) with | _ -> None

  and t_of_code_sexp s =
    printf !"Trying to parse: %{sexp:Sexp.t}\n" s;
    let t_opt =
      List.fold
        ~f:Option.first_some
        ~init:(t_of_int_sexp s)
        [ t_of_bool_sexp s
        ; t_of_three_part_sexp
            ~keyword:"lambda"
            ~validate_and_build_t:(function
                | (`List l, t) ->
                  let atoms =
                    List.filter_map l
                      ~f:(function | `Atom a -> Some a | _ -> None)
                  in
                  if (List.length atoms) <> (List.length l)
                  then 
                    raise_s [%sexp "lambda must take a list of atom as the first argument", ((`List l):t)]
                  else `Lambda (atoms, t)
                | (x, _) -> raise_s [%sexp "lambda must take a list of atom as the first argument", (x:t)]
              )
            s
        ; t_of_three_part_sexp
            ~keyword:"define"
            ~validate_and_build_t:(function
                | (`Atom a, t) -> (`Define (a,t))
                | (x, _) -> raise_s [%sexp "define must take a single atom as the first argument", (x:t)]
              )
            s
        ; t_of_three_part_sexp
            ~keyword:"set!"
            ~validate_and_build_t:(function
                | (`Atom a, t) -> (`Set (a,t))
                | (x, _) -> raise_s [%sexp "set! must take a single atom as the first argument", (x:t)]
              )
            s
        ; t_of_begin_sexp s
        ; t_of_list_sexp s
        ; t_of_atom_sexp s
        ]
    in
    if Option.is_some t_opt
    then begin
      let t = Option.value_exn t_opt in
      printf !"Parsed: %{sexp:Sexp.t} into %{sexp:t}\n" s t;
      t
    end
    else raise_s [%sexp "Failed to parse", (s : Sexp.t)]
  ;;

end

module Env = struct

  type t =
    (A.t, Exp.t) Hashtbl.Poly.t list
  [@@deriving sexp]

end

let exp =
  let s = "(begin (define a (+ 10 3)) (define f (lambda (x y) (+ a x y))))" in
  let _s = "(+ a b)" in
  Sexp.of_string s
  |> Exp.t_of_code_sexp
;;

let () =
  printf !"%{sexp:string}\n" Continuation.test_string;
  printf !"%{sexp:Exp.t}\n" exp
;;

let () =
  Sexp.to_string_hum [%sexp ([3;4;5] : int list)]
  |> print_endline
