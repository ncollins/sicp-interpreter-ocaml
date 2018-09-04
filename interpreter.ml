open Core

(* TODO:

   - should we have unit in additon to null?

   - add builtins for list/pair functions

   - string functions?

*)

let rec eval_in_env (env : Value.t Env.t) (cont: Value.t -> Value.t) (v : Value.t) : Value.t =
  match v with
  | `Exp (`Int i)      -> cont (`Exp (`Int i))
  | `Exp (`String str) -> cont (`Exp (`String str))
  | `Exp (`Symbol sym) ->
    cont (Env.lookup_ex env sym)
  | `Exp (`List ((`Symbol sym)::body)) ->
    let v = Env.lookup env sym in
    if Option.is_some v
    then eval_application_in_env env cont (Option.value_exn v, body)
    else if Symbol.(equal sym Symbol.if_)
    then eval_if_in_env env cont body 
    else if Symbol.(equal sym Symbol.define_)
    then eval_define_in_env env cont body 
    else if Symbol.(equal sym Symbol.set_)
    then eval_set_in_env env cont body
    else if Symbol.(equal sym Symbol.let_)
    then eval_let_in_env env cont body 
    else if Symbol.(equal sym Symbol.begin_)
    then eval_begin_in_env env cont body
    else if Symbol.(equal sym Symbol.lambda_)
    then eval_lambda_in_env env cont body
    else raise_s [%sexp "Symbol was not found in environement", (sym:Symbol.t)]
  (* These cases should never occur *)
  | `Exp _
  | `Function _
  | `Builtin _ -> assert false

and eval_if_in_env env cont = function
  | [ pred; exp_true; exp_false] ->
    begin
      let post_predicate_cont pred_v = 
        printf !"post_predicate_cont with %{sexp:Value.t}\n" pred_v;
        match pred_v with
        | `Exp (`Symbol s) ->
          if Symbol.((equal false_ s) || (equal null_ s))
          then eval_in_env env cont (`Exp exp_false)
          else eval_in_env env cont (`Exp exp_true)
        | _ ->
          eval_in_env env cont (`Exp exp_false)
      in
      let () = printf !"eval_if_in_env with predicate: %{sexp:Exp.t}\n" pred in
      eval_in_env env post_predicate_cont (`Exp pred)
    end
  | _ -> raise_s [%sexp "bad arguments for 'if' form"]

and eval_define_in_env env cont = function
  | [ `Symbol s; exp] ->
    (*
    let v = eval_in_env env cont (`Exp exp) in (* TODO NOT TAIL CALL *)
    let () = Env.bind env s v in
    (* TODO should this be unit? *)
    cont (`Exp (` Symbol Symbol.null_))
    *)
    let define_continuation v =
      printf !"define_continuation with %{sexp:Value.t}\n" v;
      let () = Env.bind env s v in
      cont (`Exp (`Symbol Symbol.null_))
    in
    eval_in_env env define_continuation (`Exp exp)
  | _ -> raise_s [%sexp "bad arguments for 'define' form"]

and eval_set_in_env env cont = function
  | [ `Symbol s; exp] ->
    let set_continuation v =
      let () = Env.set env s v in
      cont (`Exp (` Symbol Symbol.null_))
    in
    eval_in_env env set_continuation (`Exp exp)
  | _ -> raise_s [%sexp "bad arguments for 'define' form"]

and eval_begin_in_env env cont exps = 
    List.map exps ~f:(fun e -> `Exp e)
    (* TODO should this be unit? *)
    |> List.fold ~init:(`Exp (`Symbol Symbol.null_), env)
      (* TODO how should the continuations be handled in this fold ? *)
      ~f:(fun (_prev, env) exp -> (eval_in_env env cont exp, env)) (* TODO NOT TAIL CALL *)
    |> fst

and eval_let_in_env env cont = function
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
      (* TODO fold down this list with continuations that deal with the cons-ing of each new evaluated binding *)
      List.map valid_bindings ~f:(fun (a, e) -> (a, eval_in_env env (fun v -> v) (`Exp e))) (* TODO NOT TAIL CALL ?? *)
    in
    let new_env = Env.extend env evaled_bindings in
    eval_in_env new_env cont (`Exp body)
  | _ -> raise_s [%sexp "bad arguments for 'let' form"]

and eval_lambda_in_env env cont = function
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
    cont (`Function (valid_args, body, env))
  | _ -> raise_s [%sexp "bad arguments for 'lambda' form"]

and eval_application_in_env env cont ((f : Value.t), (args: Exp.t list)) =
  let rec make_cont ~args_eval (rev_evaled_args, unevaled_args) =
    fun v ->
      let rev_evaled_args = v::rev_evaled_args in
      match unevaled_args with
      | [] ->
        let evaled_args = List.rev rev_evaled_args in
        args_eval evaled_args
      | exp::unevaled_args ->
        let next_cont = make_cont ~args_eval (rev_evaled_args, unevaled_args) in
        eval_in_env env next_cont (`Exp exp)
  in
  let args_eval, unevaled_args =
    match (f, args) with
    | (`Function (params, body, fenv), args) ->
      ((fun evaled_args ->
          let new_bindings = List.zip_exn params evaled_args in
          let new_env = Env.extend fenv new_bindings in
          eval_in_env new_env cont (`Exp body))
      , args)
    | (`Builtin builtin_f, args) ->
      ((fun evaled_args ->
          cont (builtin_f evaled_args))
      , args)
    | (v, _) ->
      raise_s [%sexp "could not apply as function", (v : Value.t)]
  in
  match unevaled_args with
  | [] -> args_eval []
  | exp::rest ->
    let next_cont = make_cont ~args_eval ([], rest) in
    eval_in_env env next_cont (`Exp exp)
;;


let () =
  let env = Prelude.prelude () in
  let program = Sexp.load_sexp "test.scm" |> Exp.t_of_code_sexp in
  let result = eval_in_env env (fun v -> v) (`Exp program) in
  printf !"%{sexp:Value.t}\n" result
;;
