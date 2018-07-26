open Core

let equal vs =
  match vs with
  | [ v1; v2 ] ->
    if Sexp.equal (Value.sexp_of_t v1) (Value.sexp_of_t v2)
    then `Exp (`Bool true)
    else `Exp (`Bool false)
  | _ ->
    raise_s [%sexp "equal? takes two arguments"]
;;

let int_arithmetic ~f = function
  | [] ->
    raise_s [%sexp "integer functions needs at least 1 argument"]
  | vs ->
    let integers =
      List.map vs ~f:(function
          | `Exp (`Int i) -> i
          | _ -> 
            raise_s [%sexp "+ takes interger arguments", (vs : Value.t list)]
        )
    in
    let result = 
      List.fold ~init:(List.hd_exn integers) ~f (List.tl_exn integers)
    in
    `Exp (`Int result)
;;

let prelude () =
  let env = Env.empty () in
  List.iter
    ~f:(fun (symbol, f) ->
        Env.bind env (Exp.A.of_string symbol) f
      )
    [ "+", (`Builtin (int_arithmetic ~f:( + )))
    ; "*", (`Builtin (int_arithmetic ~f:( * )))
    ; "-", (`Builtin (int_arithmetic ~f:( - )))
    ; "/", (`Builtin (int_arithmetic ~f:( / )))
    ; "equal?", (`Builtin equal)
    ]
  ;
  env
;;
