open Core

module Value = struct

  type t =
    [ `Exp of Exp.t
    | `Function of ((Exp.A.t list) * Exp.t * (t Env.t))
    | `Int_binary_op of (int -> int -> int)
    ]
  [@@deriving sexp]

end

let rec eval_in_env (env : Value.t Env.t) (v : Value.t) : Value.t =
  match v with
  | `Exp `Unit       -> `Exp `Unit
  | `Exp (`Int i)    -> `Exp (`Int i)
  | `Exp (`String s) -> `Exp (`String s)
  | `Exp (`Atom a)   ->
    Env.lookup_ex env a
  | `Exp (`Bool b)   -> `Exp (`Bool b)
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
      | `Int_binary_op f ->
        begin
          match List.map args ~f:(fun e -> eval_in_env env (`Exp e)) with
          | [`Exp (`Int a); `Exp (`Int b)] -> `Exp (`Int (f a b))
          | _ -> 
            raise_s [%sexp "could not apply integer operation", (head : Exp.t), (args : Exp.t list)]
        end
      | _ ->
        raise_s [%sexp "could not apply as function", (head : Exp.t)]
    end
  | `Exp (`List []) -> failwith ""
  | `Function _ -> failwith ""
  | `Int_binary_op _ -> failwith ""
;;


let prelude =
  let env = Env.empty () in
  Env.bind env (Exp.A.of_string "+") (`Int_binary_op ( + ));
  Env.bind env (Exp.A.of_string "*") (`Int_binary_op ( * ));
  Env.bind env (Exp.A.of_string "-") (`Int_binary_op ( - ));
  Env.bind env (Exp.A.of_string "/") (`Int_binary_op ( / ));
  env
;;


let () =
  let program = Sexp.load_sexp "test.scm" |> Exp.t_of_code_sexp in
  let result = eval_in_env prelude (`Exp program) in
  printf !"%{sexp:Value.t}\n" result
;;
