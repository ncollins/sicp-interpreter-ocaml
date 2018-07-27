open Core

type 'a t =
  (Symbol.t, 'a) Hashtbl.Poly.t list
[@@deriving sexp]

let empty () =
  [ Hashtbl.Poly.create () ]
;;

let var_in_env t v =
  List.exists t ~f:(fun h -> Hashtbl.mem h v)
;;

let lookup t v = 
  List.find_map t ~f:(fun h -> Hashtbl.find h v)
;;

let lookup_ex t v =
  match lookup t v with
  | Some e -> e
  | None ->
    raise_s [%sexp "could not find variable in environment"
                 , (v : Symbol.t)]
;;

let bind t v e =
  let h = List.hd_exn t in
  Hashtbl.update h v ~f:(fun _ -> e)
;;

let set t v e =
  match List.find t ~f:(fun h -> Hashtbl.mem h v) with
  | Some h -> Hashtbl.update h v ~f:(fun _ -> e)
  | None -> 
    raise_s [%sexp "tried to set a variable not present in environment"
                 , (v : Symbol.t)]
;;

let extend t bindings =
  let new_h = Hashtbl.Poly.of_alist_exn bindings in
  new_h :: t
;;
