open Core

(* TODO:

   - should we have unit in additon to null?

   - add builtins for list/pair functions

   - string functions?

*)

module Fail = struct
  type t = unit ->  Value.t
end

module Succeed = struct
  type t = Value.t -> Value.t
end


module Venv = struct
  type t = Value.t Env.t
end

module type T = sig

  val basic_eval : Value.t -> Venv.t -> Value.t

  val monadic_eval : Value.t -> Venv.t -> (Value.t -> Fail.t) -> Fail.t -> Fail.t

  val analyze : Value.t -> (Venv.t -> Succeed.t -> Fail.t -> Value.t)

  val full_analyze : Value.t -> (Venv.t -> ((Value.t * Fail.t) -> Value.t) -> (unit -> Value.t) -> Value.t)

end


(* Need eval to return a function, that can then be applied *)

let rec eval_in_env
    ~env:(env : Value.t Env.t)
    (v : Value.t)
    (succeed: Succeed.t)
  =
  let () = printf !"eval_in_env:\n%{sexp:Value.t}\n%{sexp:Value.t Env.t}\n--------------\n" v env in
  match v with
  | `Exp (`Int i)      -> succeed (`Exp (`Int i))
  | `Exp (`String str) -> succeed (`Exp (`String str))
  | `Exp (`Symbol sym) ->
    succeed (Env.lookup_ex env sym)
  | `Exp (`List ((`Symbol sym)::body)) ->
    let v = Env.lookup env sym in
    if Option.is_some v
    then eval_application_in_env
        ~env
        (Option.value_exn v, body)
        succeed
    else if Symbol.(equal sym Symbol.if_)
    then eval_if_in_env ~env  body succeed
    else if Symbol.(equal sym Symbol.define_)
    then  eval_define_in_env ~env  body succeed
    else if Symbol.(equal sym Symbol.set_)
    then eval_set_in_env ~env  body succeed
    else if Symbol.(equal sym Symbol.let_)
    then eval_let_in_env ~env  body succeed
    else if Symbol.(equal sym Symbol.begin_)
    then eval_begin_in_env ~env  body succeed
    else if Symbol.(equal sym Symbol.lambda_)
    then eval_lambda_in_env ~env  body succeed
    else if Symbol.(equal sym Symbol.amb_)
    then eval_amb_in_env ~env ~succeed  body
    else raise_s [%sexp "Symbol was not found in environement", (sym:Symbol.t)]
  | `Failed -> failwith ""
  (* These cases should never occur *)
  | `Exp _
  | `Function _
  | `Builtin _ -> assert false

and eval_if_in_env
    ~env
    value
    (succeed: Succeed.t)
  =
  match value with 
  | [ pred; exp_true; exp_false] ->
    printf !"eval_if_in_env: %{sexp:Exp.t list}\n" [pred; exp_true; exp_false];
    begin
      let new_succeed pred_value =
        (*
        printf !"%{sexp:Value.t}\n" pred_value;
        *)
        match pred_value with
        | `Exp (`Symbol s) ->
          if Symbol.((equal false_ s) || (equal null_ s))
          then eval_in_env ~env (`Exp exp_false) succeed
          else eval_in_env ~env (`Exp exp_true) succeed
        | _ ->
          eval_in_env ~env (`Exp exp_false) succeed
      in
      eval_in_env
        ~env
        (`Exp pred)
        new_succeed
    end
  | _ -> raise_s [%sexp "bad arguments for 'if' form"]

and eval_define_in_env
    ~env
    value
    (succeed: Succeed.t)
  =
  match value with
  | [ `Symbol s; exp] ->
    let () = printf !"eval_define_in_env: %{sexp:Symbol.t}\n--------------\n" s in
    let () = printf !"*****\nDefine: %{sexp:Symbol.t} %{sexp:Exp.t}\n****\n" s exp in
    let define_succeed v =
      let () = printf !"*****\nDefine val: %{sexp:Symbol.t} %{sexp:Value.t}\n****\n" s v in
      let () = Env.bind env s v in
      let () = printf !"*****\nDefined: %{sexp:Symbol.t} %{sexp:Value.t}\n****\n" s v in
      v
    in
    let v = eval_in_env ~env  (`Exp exp) define_succeed in
    succeed v
  | _ -> raise_s [%sexp "bad arguments for 'define' form"]

and eval_set_in_env
    ~env
    value
    (succeed: Succeed.t)
  =
  match value with
  | [ `Symbol s; exp] ->
    let v = eval_in_env ~env  (`Exp exp) succeed in
    let () = Env.set env s v in
    (* TODO should this be unit? *)
    `Exp (` Symbol Symbol.null_)
  | _ -> raise_s [%sexp "bad arguments for 'define' form"]

and eval_begin_in_env
    ~env
    exps
    (succeed: Succeed.t)
  =
  let eval v e s =
    eval_in_env ~env:e v s
  in
  let eval_functions = 
    let values : Value.t list = List.map exps ~f:(fun e -> `Exp e) in
    let functions : (Value.t Env.t -> Succeed.t -> Value.t) list =
      List.map ~f:(fun v -> eval v) values
    in
    functions
  in
  match eval_functions with
  | [] -> `Exp (`Symbol Symbol.null_)
  | [f] -> f env succeed
  | f0::rest ->
    let f = 
      List.fold_right
        rest
        ~init:f0
        ~f:(fun a b ->
            (fun env succeed ->
               a env
                 (fun _v -> b env succeed))
          )
    in
    f env succeed

and eval_let_in_env
    ~env
    value
    (succeed: Succeed.t)
  =
  match value with
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
      List.map valid_bindings ~f:(fun (a, e) -> (a, eval_in_env ~env  (`Exp e) (fun v -> v)))
    in
    let new_env = Env.extend env evaled_bindings in
    eval_in_env ~env:new_env  (`Exp body) succeed
  | _ -> raise_s [%sexp "bad arguments for 'let' form"]

and eval_lambda_in_env
    ~env
    value
    (succeed: Succeed.t)
  =
  match value with
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
    succeed (`Function (valid_args, body, env))
  | _ -> raise_s [%sexp "bad arguments for 'lambda' form"]

and eval_amb_in_env ~env ~succeed body =
  ignore (succeed, body, env);
  failwith ""

and eval_application_in_env
    ~env
    ((f : Value.t), (args: Exp.t list))
    (succeed: Succeed.t)
  =
  match (f, args) with
  | (`Function (params, body, fenv), args) ->
    let () =
      printf !"Function application %{sexp:Value.t} to %{sexp:Exp.t list}\n"
        (`Function (params, body, fenv))
        args
    in
    let evaled_args =
      List.map args ~f:(fun e -> eval_in_env ~env  (`Exp e) succeed)
    in
    let new_bindings = List.zip_exn params evaled_args in
    let new_env = Env.extend fenv new_bindings in
    eval_in_env ~env:new_env  (`Exp body) succeed
  | (`Builtin f, args) ->
    succeed (f (List.map args ~f:(fun e -> eval_in_env ~env  (`Exp e) (fun v -> v))))
  | (v, _) ->
    raise_s [%sexp "could not apply as function", (v : Value.t)]
;;


let () =
  let env = Prelude.prelude () in
  let program = Sexp.load_sexp "test.scm" |> Exp.t_of_code_sexp in
  let succeed value =
    printf !"First succeed: %{sexp:Value.t}\n" value;
    value
  in
  let result =
    eval_in_env
      ~env
      (`Exp program)
      succeed
  in
  printf !"%{sexp:Value.t}\n" result
;;

(*
let () =
  let s = Continuation.test_string in
  printf !"%s\n" s
;;
*)
