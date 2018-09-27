open Core

let debug_print vs =
  printf !"%{sexp:Value.t list}" vs;
  (`Exp (`Symbol Symbol.null_))
;;

let debug_print_ln vs =
  printf !"%{sexp:Value.t list}\n" vs;
  (`Exp (`Symbol Symbol.null_))
;;

let rec to_string (v : Value.t) =
  match v with
  | `Exp (`Int i) -> Int.to_string i
  | `Exp (`String s) -> "\"" ^ s ^ "\""
  | `Exp (`Symbol sym) -> Symbol.to_string sym
  | `Exp (`List lst) ->
    "(" ^ (List.map ~f:(fun e -> to_string (`Exp e)) lst |> String.concat ~sep:" ") ^ ")"
  | `Function (args, _, _)  ->
    let l = List.length args in
    if l = 1
    then "<Function of 1 argument>"
    else sprintf !"<Function of %d arguments>" (List.length args)
  | `Builtin _ ->
    "<Built-in-function"
;;

let print vs =
  List.iter vs ~f:(fun v -> printf !"%s" (to_string v));
  (`Exp (`Symbol Symbol.null_))
;;

let print_ln vs =
  List.iter vs ~f:(fun v -> printf !"%s" (to_string v));
  printf !"\n";
  (`Exp (`Symbol Symbol.null_))
;;

let equal vs =
  match vs with
  | [ v1; v2 ] ->
    if Sexp.equal (Value.sexp_of_t v1) (Value.sexp_of_t v2)
    then `Exp (`Symbol (Symbol.true_))
    else `Exp (`Symbol (Symbol.false_))
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
        Env.bind env (Symbol.of_string symbol) f
      )
    (* constants *)
    [ "null", (`Exp (`Symbol Symbol.null_))
    ; "true", (`Exp (`Symbol Symbol.true_))
    ; "false", (`Exp (`Symbol Symbol.false_))
    (* integer functions *)
    ; "+", (`Builtin (int_arithmetic ~f:( + )))
    ; "*", (`Builtin (int_arithmetic ~f:( * )))
    ; "-", (`Builtin (int_arithmetic ~f:( - )))
    ; "/", (`Builtin (int_arithmetic ~f:( / )))
    (* general functions *)
    ; "equal?", (`Builtin equal)
    (* IO functions *)
    ; "debug-print",  (`Builtin debug_print)
    ; "debug-println", (`Builtin debug_print_ln)
    ; "print",  (`Builtin print)
    ; "println", (`Builtin print_ln)
    ]
  ;
  env
;;
