open Core

(* TODO:

   - should we be creating explict forms for
     { if, begin, lambda, define, set }
     as opposed to putting them in lists and matching on that?
     This might be significantly simpler

   - null (reuse unit?) and pairs. pair can just be a function
     that produces a tuple of values.

   - rename Atom to Symbol
   - `Bool _ can be replaced with constants

   - string functions?

*)

let rec eval_in_env (env : Value.t Env.t) (v : Value.t) : Value.t =
  match v with
  | `Exp `Unit       -> `Exp `Unit
  | `Exp (`Int i)    -> `Exp (`Int i)
  | `Exp (`String s) -> `Exp (`String s)
  | `Exp (`Atom a)   ->
    Env.lookup_ex env a
  | `Exp (`Bool b)   -> `Exp (`Bool b)
  | `Exp (`If (pred, exp_true, exp_false)) ->
    begin
      match eval_in_env env (`Exp pred) with
      | `Exp (`Bool false)
      | `Exp `Unit ->
        eval_in_env env (`Exp exp_false)
      | _ ->
        eval_in_env env (`Exp exp_true)
    end
  | `Exp (`Define (atom, exp)) ->
    begin
      let v = eval_in_env env (`Exp exp) in
      Env.bind env atom v;
      `Exp `Unit
    end
  | `Exp (`Set (atom, exp))    ->
    begin
      let v = eval_in_env env (`Exp exp) in
      Env.set env atom v;
      `Exp `Unit
    end
  | `Exp (`Begin exps) ->
    List.map exps ~f:(fun e -> `Exp e)
    |> List.fold ~init:(`Exp `Unit, env)
      ~f:(fun (_prev, env) exp -> (eval_in_env env exp, env))
    |> fst
  | `Exp (`Let (bindings, body)) ->
    let evaled_bindings =
      List.map bindings ~f:(fun (a, e) -> (a, eval_in_env env (`Exp e)))
    in
    let new_env = Env.extend env evaled_bindings in
    eval_in_env new_env (`Exp body)
  (* functions... *)
  | `Exp (`Lambda (args, body)) -> `Function (args, body, env)
  | `Exp (`List (head::args)) ->
    begin
      match eval_in_env env (`Exp head) with
      | `Function (params, body, fenv) ->
        let evaled_args =
          List.map args ~f:(fun e -> eval_in_env env (`Exp e))
        in
        let new_bindings = List.zip_exn params evaled_args in
        let new_env = Env.extend fenv new_bindings in
        eval_in_env new_env (`Exp body) 
      | `Builtin f ->
        f (List.map args ~f:(fun e -> eval_in_env env (`Exp e)))
      | _ ->
        raise_s [%sexp "could not apply as function", (head : Exp.t)]
    end
  (* These cases should never occur *)
  | `Exp (`List [])
  | `Function _
  | `Builtin _ -> assert false
;;


let () =
  let env = Prelude.prelude () in
  let program = Sexp.load_sexp "test.scm" |> Exp.t_of_code_sexp in
  let result = eval_in_env env (`Exp program) in
  printf !"%{sexp:Value.t}\n" result
;;
