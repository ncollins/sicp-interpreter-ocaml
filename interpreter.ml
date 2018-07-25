open Core

let exp =
  let s =
    "(begin (define a (+ 10 3)) (define f (lambda (x y) (+ a x y))))" in
  Sexp.of_string s
  |> Exp.t_of_code_sexp
;;

let () =
  printf !"%{sexp:string}\n" Continuation.test_string;
  printf !"%{sexp:Exp.t}\n" exp
;;

let () =
  let _env = Env.empty () in
  Sexp.to_string_hum [%sexp ([3;4;5] : int list)]
  |> print_endline
