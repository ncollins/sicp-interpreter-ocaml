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
        match pred_v with
        | `Exp (`Symbol s) ->
          if Symbol.((equal false_ s) || (equal null_ s))
          then eval_in_env env cont (`Exp exp_false)
          else eval_in_env env cont (`Exp exp_true)
        | _ ->
          eval_in_env env cont (`Exp exp_false)
      in
      eval_in_env env post_predicate_cont (`Exp pred)
    end
  | _ -> raise_s [%sexp "bad arguments for 'if' form"]

and eval_define_in_env env cont = function
  | [ `Symbol s; exp] ->
    let define_continuation v =
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
    let eval_with_rev_sequence rev_sequence =
      match rev_sequence with
      | [] -> cont (`Exp (`Symbol Symbol.null_))
      | last_value::_ -> cont (last_value)
    in
    eval_sequence_in_env ~eval_with_rev_sequence env exps

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
    let symbols = List.map ~f:fst valid_bindings in
    let expressions = List.map ~f:snd valid_bindings in
    let eval_with_rev_sequence rev_sequence =
      let expressions = List.rev rev_sequence in
      let evaled_bindings = List.zip_exn symbols expressions in
      let new_env = Env.extend env evaled_bindings in
      eval_in_env new_env cont (`Exp body)
    in
    eval_sequence_in_env ~eval_with_rev_sequence env expressions
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

and eval_sequence_in_env ~eval_with_rev_sequence env exp_list =
  let rec make_cont (rev_evaled_args, unevaled_args) =
    fun v ->
      let rev_evaled_args = v::rev_evaled_args in
      match unevaled_args with
      | [] ->
        eval_with_rev_sequence rev_evaled_args
      | exp::unevaled_args ->
        let next_cont = make_cont (rev_evaled_args, unevaled_args) in
        eval_in_env env next_cont (`Exp exp)
  in
  match exp_list with
  | [] -> eval_with_rev_sequence []
  | exp::rest ->
    let next_cont = make_cont ([], rest) in
    eval_in_env env next_cont (`Exp exp)

and eval_application_in_env env cont ((f : Value.t), (args: Exp.t list)) =
  let eval_rev_args, unevaled_args =
    match (f, args) with
    | (`Function (params, body, fenv), args) ->
      ((fun rev_evaled_args ->
          let new_bindings = List.zip_exn params (List.rev rev_evaled_args) in
          let new_env = Env.extend fenv new_bindings in
          eval_in_env new_env cont (`Exp body))
      , args)
    | (`Builtin builtin_f, args) ->
      ((fun rev_evaled_args ->
          cont (builtin_f (List.rev rev_evaled_args)))
      , args)
    | (v, _) ->
      raise_s [%sexp "could not apply as function", (v : Value.t)]
  in
  eval_sequence_in_env ~eval_with_rev_sequence:eval_rev_args env unevaled_args
;;


let () =
  let env = Prelude.prelude () in
  let program = Sexp.load_sexp "test.scm" |> Exp.t_of_code_sexp in
  let result = eval_in_env env (fun v -> v) (`Exp program) in
  printf !"%{sexp:Value.t}\n" result
;;
