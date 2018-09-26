open Core

(* TODO:

   - should we have unit in additon to null?

   - add builtins for list/pair functions

   - string functions?

*)

let rec eval_in_env (env : Value.t Env.t) (succeed: Value.t -> (unit -> Value.t) -> Value.t) (fail: unit -> Value.t) (v : Value.t) : Value.t =
  match v with
  | `Exp (`Int i)      -> succeed (`Exp (`Int i)) fail
  | `Exp (`String str) -> succeed (`Exp (`String str)) fail
  | `Exp (`Symbol sym) ->
    succeed (Env.lookup_ex env sym) fail
  | `Exp (`List ((`Symbol sym)::body)) ->
    let v = Env.lookup env sym in
    if Option.is_some v
    then eval_application_in_env env succeed fail (Option.value_exn v, body)
    else if Symbol.(equal sym Symbol.if_)
    then eval_if_in_env env succeed fail body 
    else if Symbol.(equal sym Symbol.define_)
    then eval_define_in_env env succeed fail body 
    else if Symbol.(equal sym Symbol.set_)
    then eval_set_in_env env succeed fail body
    else if Symbol.(equal sym Symbol.let_)
    then eval_let_in_env env succeed fail body 
    else if Symbol.(equal sym Symbol.begin_)
    then eval_begin_in_env env succeed fail body
    else if Symbol.(equal sym Symbol.lambda_)
    then eval_lambda_in_env env succeed fail body
    else if Symbol.(equal sym Symbol.amb_)
    then eval_amb_in_env env succeed fail body
    else if Symbol.(equal sym Symbol.fail_)
    then fail ()
    else raise_s [%sexp "Symbol was not found in environement", (sym:Symbol.t)]
  (* These cases should never occur *)
  | `Exp _
  | `Function _
  | `Builtin _ -> assert false

and eval_if_in_env env succeed fail = function
  | [ pred; exp_true; exp_false] ->
    begin
      let post_predicate_succeed pred_v fail = 
        match pred_v with
        | `Exp (`Symbol s) ->
          if Symbol.((equal false_ s) || (equal null_ s))
          then eval_in_env env succeed fail (`Exp exp_false)
          else eval_in_env env succeed fail (`Exp exp_true)
        | _ ->
          eval_in_env env succeed fail (`Exp exp_false)
      in
      eval_in_env env post_predicate_succeed fail (`Exp pred)
    end
  | _ -> raise_s [%sexp "bad arguments for 'if' form"]

and eval_define_in_env env succeed fail = function
  | [ `Symbol s; exp] ->
    let define_succeed v =
      let () = Env.bind env s v in
      succeed (`Exp (`Symbol Symbol.null_))
    in
    eval_in_env env define_succeed fail (`Exp exp)
  | _ -> raise_s [%sexp "bad arguments for 'define' form"]

and eval_set_in_env env succeed fail = function
  | [ `Symbol s; exp] ->
    let set_succeed v =
      let () = Env.set env s v in
      succeed (`Exp (` Symbol Symbol.null_))
    in
    eval_in_env env set_succeed fail (`Exp exp)
  | _ -> raise_s [%sexp "bad arguments for 'define' form"]

and eval_begin_in_env env succeed fail exps = 
    let eval_with_rev_sequence rev_sequence =
      match rev_sequence with
      | [] -> succeed (`Exp (`Symbol Symbol.null_))
      | last_value::_ -> succeed (last_value)
    in
    eval_sequence_in_env ~eval_with_rev_sequence env fail exps

and eval_let_in_env env succeed fail = function
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
    let eval_with_rev_sequence rev_sequence fail =
      let expressions = List.rev rev_sequence in
      let evaled_bindings = List.zip_exn symbols expressions in
      let new_env = Env.extend env evaled_bindings in
      eval_in_env new_env succeed fail (`Exp body)
    in
    eval_sequence_in_env ~eval_with_rev_sequence env fail expressions
  | _ -> raise_s [%sexp "bad arguments for 'let' form"]

and eval_lambda_in_env env succeed fail = function
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
    succeed (`Function (valid_args, body, env)) fail
  | _ -> raise_s [%sexp "bad arguments for 'lambda' form"]

and eval_sequence_in_env ~eval_with_rev_sequence env fail exp_list =
  let rec make_succeed (rev_evaled_args, unevaled_args) =
    fun v fail ->
      let rev_evaled_args = v::rev_evaled_args in
      match unevaled_args with
      | [] ->
        eval_with_rev_sequence rev_evaled_args fail
      | exp::unevaled_args ->
        let next_succeed = make_succeed (rev_evaled_args, unevaled_args) in
        eval_in_env env next_succeed fail (`Exp exp)
  in
  match exp_list with
  | [] -> eval_with_rev_sequence [] fail
  | exp::rest ->
    let next_succeed = make_succeed ([], rest) in
    eval_in_env env next_succeed fail (`Exp exp)

and eval_application_in_env env succeed fail ((f : Value.t), (args: Exp.t list)) =
  let eval_rev_args, unevaled_args =
    match (f, args) with
    | (`Function (params, body, fenv), args) ->
      ((fun rev_evaled_args fail ->
          let new_bindings = List.zip_exn params (List.rev rev_evaled_args) in
          let new_env = Env.extend fenv new_bindings in
          eval_in_env new_env succeed fail (`Exp body))
      , args)
    | (`Builtin builtin_f, args) ->
      ((fun rev_evaled_args ->
          succeed (builtin_f (List.rev rev_evaled_args)))
      , args)
    | (v, _) ->
      raise_s [%sexp "could not apply as function", (v : Value.t)]
  in
  eval_sequence_in_env ~eval_with_rev_sequence:eval_rev_args env fail unevaled_args

and eval_amb_in_env env succeed fail exps =
  let eval_with_rev_sequence rev_sequence fail =
    let choices = List.rev rev_sequence in
    let rec try_next choices =
      match choices with
      | [] -> fail ()
      | first::rest -> eval_in_env env succeed (fun () -> try_next rest) first
    in
    try_next choices
  in
  eval_sequence_in_env ~eval_with_rev_sequence env fail exps
;;


(*
let () =
  let env = Prelude.prelude () in
  let program = Sexp.load_sexp "test.scm" |> Exp.t_of_code_sexp in
  let result =
    eval_in_env env
      (fun v -> printf !"Top succeed cont: %{sexp:Value.t}\n" v; (`Exp (`Symbol Symbol.fail_)))
      (fun () -> failwith "Real failure")
      (`Exp program)
  in
  printf !"%{sexp:Value.t}\n" result
;;
*)

let env =
  Prelude.prelude ()
;;

let rec repl_loop () =
  let rec loop (fail_and_try_again_function: unit -> Value.t) () : Value.t =
    let () = printf "Input > " in
    Out_channel.(flush stdout);
    match In_channel.input_line In_channel.stdin with
    | Some "exit" -> (`Exp (`Symbol Symbol.fail_))
    | Some "try-again" ->
      fail_and_try_again_function ()
    | Some line ->
      let exp = Sexp.of_string line |> Exp.t_of_code_sexp in
      let v = 
        eval_in_env
          env
          (* success function *)
          (fun v fail ->
             printf !"Top succeed cont: %{sexp:Value.t}\n" v;
             (*
              (`Exp (`Symbol Symbol.fail_))
             *)
             fail ()
          )
          (* failure function*)
          (fun () -> printf !"No more values of: %{sexp:Exp.t}\n" exp; repl_loop ())
          (`Exp exp)
      in
      let () = printf !"%{sexp:Value.t}\n" v in
      loop fail_and_try_again_function ()
    | None -> (`Exp (`Symbol Symbol.fail_))
  in
  loop (fun () -> repl_loop ()) ()
;;

let () =
  ignore (repl_loop ())
;;
