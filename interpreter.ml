open Core

(* TODO:

   - should we have unit in additon to null?

   - add builtins for list/pair functions

   - string functions?

*)

let rec eval_in_env (env : Value.t Env.t) (v : Value.t) : Value.t =
  match v with
  | `Exp (`Int i)      -> `Exp (`Int i)
  | `Exp (`String str) -> `Exp (`String str)
  | `Exp (`Symbol sym) ->
    Env.lookup_ex env sym
  | `Exp (`List ((`Symbol sym)::body)) ->
    let v = Env.lookup env sym in
    if Option.is_some v
    then eval_application_in_env env (Option.value_exn v, body)
    else if Symbol.(equal sym Symbol.if_)
    then eval_if_in_env env body
    else if Symbol.(equal sym Symbol.define_)
    then eval_define_in_env env body
    else if Symbol.(equal sym Symbol.set_)
    then eval_set_in_env env body
    else if Symbol.(equal sym Symbol.let_)
    then eval_let_in_env env body
    else if Symbol.(equal sym Symbol.begin_)
    then eval_begin_in_env env body
    else if Symbol.(equal sym Symbol.lambda_)
    then eval_lambda_in_env env body
    else raise_s [%sexp "Symbol was not found in environement", (sym:Symbol.t)]
  (* These cases should never occur *)
  | `Exp _
  | `Function _
  | `Builtin _ -> assert false

and eval_if_in_env env = function
  | [ pred; exp_true; exp_false] ->
    begin
      match eval_in_env env (`Exp pred) with
      | `Exp (`Symbol s) ->
          if Symbol.((equal false_ s) || (equal null_ s))
          then eval_in_env env (`Exp exp_false)
          else eval_in_env env (`Exp exp_true)
      | _ ->
        eval_in_env env (`Exp exp_false)
    end
  | _ -> raise_s [%sexp "bad arguments for 'if' form"]

and eval_define_in_env env = function
  | [ `Symbol s; exp] ->
    let v = eval_in_env env (`Exp exp) in
    let () = Env.bind env s v in
    (* TODO should this be unit? *)
    `Exp (` Symbol Symbol.null_)
  | _ -> raise_s [%sexp "bad arguments for 'define' form"]

and eval_set_in_env env = function
  | [ `Symbol s; exp] ->
    let v = eval_in_env env (`Exp exp) in
    let () = Env.set env s v in
    (* TODO should this be unit? *)
    `Exp (` Symbol Symbol.null_)
  | _ -> raise_s [%sexp "bad arguments for 'define' form"]

and eval_begin_in_env env exps = 
    List.map exps ~f:(fun e -> `Exp e)
    (* TODO should this be unit? *)
    |> List.fold ~init:(`Exp (`Symbol Symbol.null_), env)
      ~f:(fun (_prev, env) exp -> (eval_in_env env exp, env))
    |> fst

and eval_let_in_env env = function
  | [ `List bindings ; body ] ->
    let valid_bindings =
      List.filter_map bindings ~f:(function
          | `List [ `Symbol s ; exp ] -> Some (s, exp)
          | e ->
            raise_s
              [%sexp "let binding should be a list of symbol, exp pairs"
                   , (e : Exp.t)]
        )
    in
    let evaled_bindings =
      List.map valid_bindings ~f:(fun (a, e) -> (a, eval_in_env env (`Exp e)))
    in
    let new_env = Env.extend env evaled_bindings in
    eval_in_env new_env (`Exp body)
  | _ -> raise_s [%sexp "bad arguments for 'let' form"]

and eval_lambda_in_env env = function
  | [ `List args ; body ] ->
    let valid_args =
      List.filter_map args ~f:(function
          | `Symbol s -> Some s
          | e ->
            raise_s
              [%sexp "first argument to lambda should be a list of symbols"
                   , (e : Exp.t)]
        )
    in
    `Function (valid_args, body, env)
  | _ -> raise_s [%sexp "bad arguments for 'lambda' form"]

and eval_application_in_env env ((f : Value.t), (args: Exp.t list)) =
  match (f, args) with
  | (`Function (params, body, fenv), args) ->
    let evaled_args =
      List.map args ~f:(fun e -> eval_in_env env (`Exp e))
    in
    let new_bindings = List.zip_exn params evaled_args in
    let new_env = Env.extend fenv new_bindings in
    eval_in_env new_env (`Exp body) 
  | (`Builtin f, args) ->
    f (List.map args ~f:(fun e -> eval_in_env env (`Exp e)))
  | (v, _) ->
    raise_s [%sexp "could not apply as function", (v : Value.t)]
;;


let () =
  let env = Prelude.prelude () in
  let program = Sexp.load_sexp "test.scm" |> Exp.t_of_code_sexp in
  let result = eval_in_env env (`Exp program) in
  printf !"%{sexp:Value.t}\n" result
;;
