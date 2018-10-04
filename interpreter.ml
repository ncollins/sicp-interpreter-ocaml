open Core
open Continuation

(* TODO:

   - should we have unit in additon to null?

   - add builtins for list/pair functions

   - string functions?

*)

type state = Value.t Env.t * Value.t

type cont = (state, state) Continuation.Raw_cont.t

module C = Continuation.C

let rec eval_in_env ((env, v) : state) : cont =
  match v with
  | `Exp (`Int i)      -> C.return (env, `Exp (`Int i))
  | `Exp (`String str) -> C.return (env, `Exp (`String str))
  | `Exp (`Symbol sym) ->
    C.return (env, Env.lookup_ex env sym)
  | `Exp (`List ((`Symbol sym)::body)) ->
    let v = Env.lookup env sym in
    if Option.is_some v
    then eval_application_in_env (env, (Option.value_exn v, body))
    else if Symbol.(equal sym Symbol.if_)
    then eval_if_in_env (env, body)
    else if Symbol.(equal sym Symbol.define_)
    then eval_define_in_env (env, body)
    else if Symbol.(equal sym Symbol.set_)
    then eval_set_in_env (env, body)
    else if Symbol.(equal sym Symbol.let_)
    then eval_let_in_env (env, body)
    else if Symbol.(equal sym Symbol.begin_)
    then eval_begin_in_env (env, body)
    else if Symbol.(equal sym Symbol.lambda_)
    then eval_lambda_in_env (env, body)
    else raise_s [%sexp "Symbol was not found in environement", (sym:Symbol.t)]
  (* These cases should never occur *)
  | `Exp _
  | `Function _
  | `Builtin _ -> assert false

and eval_if_in_env (env, exps) =
  match exps with
  | [ pred; exp_true; exp_false] ->
    begin
      let open C.Let_syntax in
      let%bind (new_env, pred_v) = eval_in_env (env, `Exp pred) in
        match pred_v with
        | `Exp (`Symbol s) ->
          if Symbol.((equal false_ s) || (equal null_ s))
          then eval_in_env (new_env, `Exp exp_false)
          else eval_in_env (new_env, `Exp exp_true)
        | _ ->
          eval_in_env (new_env, `Exp exp_false)
    end
  | _ -> raise_s [%sexp "bad arguments for 'if' form"]

and eval_define_in_env (env, exps) =
  match (exps : Exp.t list) with
  | [ `Symbol s; exp] ->
    let open C.Let_syntax in
    let%bind (_env, v) = eval_in_env (env, `Exp exp) in
    let () = Env.bind env s v in
    C.return (env, `Exp (`Symbol Symbol.null_))
  | _ -> raise_s [%sexp "bad arguments for 'define' form"]

and eval_set_in_env (env, exps) =
  match exps with
  | [ `Symbol s; exp] ->
    let open C.Let_syntax in
    let%bind (new_env, v) = eval_in_env (env, `Exp exp) in
    let () = Env.set new_env s v in
    C.return (new_env, `Exp (` Symbol Symbol.null_))
  | _ -> raise_s [%sexp "bad arguments for 'define' form"]

and eval_begin_in_env (env, exps) =
  let rec recur (env, xs, last) : (state, state) Raw_cont.t =
    match xs with
    | [] -> C.return (env, last)
    | x::xs' ->
      let open C.Let_syntax in
      let%bind (new_env, v) = eval_in_env (env, `Exp x) in
      recur (new_env, xs', v)
  in
  recur (env, exps, `Exp (`Symbol Symbol.null_))

and eval_let_in_env (env, exps) =
  match exps with
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
    let unevaled_expressions = List.map ~f:snd valid_bindings in
    let open C.Let_syntax in
    let%bind evaled =
      List.map unevaled_expressions ~f:(fun exp -> eval_in_env (env, `Exp exp))
      |> C.all
    in
    let evaled_expressions = List.map ~f:snd evaled in
    let evaled_bindings = List.zip_exn symbols evaled_expressions in
    let new_env = Env.extend env evaled_bindings in
    eval_in_env (new_env, `Exp body)
  | _ -> raise_s [%sexp "bad arguments for 'let' form"]

and eval_lambda_in_env (env, v) =
  match v with
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
    C.return (env, `Function (valid_args, body, env))
  | _ -> raise_s [%sexp "bad arguments for 'lambda' form"]

(*
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
*)

and eval_application_in_env (env, ((f : Value.t), (args: Exp.t list))) =
  match (f, args) with
  | (`Function (params, body, fenv), args) ->
    let open C.Let_syntax in
    let%bind evaled =
      List.map args ~f:(fun exp -> eval_in_env (env, `Exp exp))
      |> C.all
    in
    let evaled_args = List.map ~f:snd evaled in
    let evaled_bindings = List.zip_exn params evaled_args in
    let new_env = Env.extend fenv evaled_bindings in
    eval_in_env (new_env, `Exp body)
  | (`Builtin builtin_f, args) ->
    let open C.Let_syntax in
    let%bind evaled =
      List.map args ~f:(fun exp -> eval_in_env (env, `Exp exp))
      |> C.all
    in
    let evaled_args = List.map ~f:snd evaled in
    C.return (env, builtin_f evaled_args)
  | (v, _) ->
    raise_s [%sexp "could not apply as function", (v : Value.t)]
;;


let () =
  let env = Prelude.prelude () in
  let program = Sexp.load_sexp "test.scm" |> Exp.t_of_code_sexp in
  let cont = eval_in_env (env, `Exp program) in
  let (_final_env, final_value) = cont ident in
  printf !"%{sexp:Value.t}\n" final_value
;;
