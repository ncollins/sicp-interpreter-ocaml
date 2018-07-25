open Core

module Value = struct

  type t =
    [ `Exp of Exp.t
    | `Function of ((Exp.A.t list) * Exp.t * (t Env.t))
    | `Builtin of (int -> int -> int)
    ]
  [@@deriving sexp]

end

let rec eval_in_env (env : Value.t Env.t) (v : Value.t) : Value.t =
  printf !"eval_in_env: v = %{sexp:Value.t}\n" v;
  match v with
  | `Exp `Unit       -> `Exp `Unit
  | `Exp (`Int i)    -> `Exp (`Int i)
  | `Exp (`String s) -> `Exp (`String s)
  | `Exp (`Atom a)   ->
    begin
    printf !"Atom! %{sexp:Exp.A.t}\n" a;
      Env.lookup_ex env a
    end
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
  (* functions... *)
  | `Exp (`Lambda (args, body)) -> `Function (args, body, env)
  | `Exp (`List (head::args)) ->
    begin
      printf !"Head = %{sexp:Exp.t}\n" head;
      match eval_in_env env (`Exp head) with
      | `Function (params, body, fenv) ->
        let evaled_args =
          List.map args ~f:(fun e -> eval_in_env env (`Exp e))
        in
        let new_bindings = List.zip_exn params evaled_args in
        let new_env = Env.extend fenv new_bindings in
        eval_in_env new_env (`Exp body) 
      | `Builtin f ->
        begin
          match List.map args ~f:(fun e -> eval_in_env env (`Exp e)) with
          | [`Exp (`Int a); `Exp (`Int b)] -> `Exp (`Int (f a b))
          | _ -> 
            raise_s [%sexp "could not apply builtin", (head : Exp.t), (args : Exp.t list)]
        end
      | _ ->
        raise_s [%sexp "could not apply as function", (head : Exp.t)]
    end
  | `Exp (`List []) -> failwith ""
  | `Function _ -> failwith ""
  | `Builtin _ -> failwith ""
;;


let eval exp =
  let env = Env.empty () in
  Env.bind env (Exp.A.of_string "+") (`Builtin ( + ));
  Env.bind env (Exp.A.of_string "*") (`Builtin ( * ));
  Env.bind env (Exp.A.of_string "-") (`Builtin ( - ));
  Env.bind env (Exp.A.of_string "/") (`Builtin ( / ));
  eval_in_env env (`Exp exp)
;;

let exp =
  let s =
    String.concat ~sep:" "
     ["(begin "
     ; "  (define a (+ 10 3))"
     ; "  (define f (lambda (x y) (+ a (+ x y))))"
     ; "  (f 1 2)"
     ; ")"
     ]
  in
  Sexp.of_string s
  |> Exp.t_of_code_sexp
;;

let () =
  printf !"%{sexp:string}\n" Continuation.test_string;
  printf !"%{sexp:Exp.t}\n" exp;
  printf !"%{sexp:Value.t}\n" (eval exp)
;;

(*
let () =
  let _env = Env.empty () in
  Sexp.to_string_hum [%sexp ([3;4;5] : int list)]
  |> print_endline
  *)
