open Core

(* What is a continuation?

   A function that rather returning its value to the caller,
   passes it onto another function (provided as a callback).

   How do we model this?


*)

module Raw_cont : sig

  (* - take an 'a
     - produce a 'b
     - don't return the 'b!
     - call another function that takes the 'b
       i.e. with type ('b -> 'r)
  *)

  type ('a, 'r) t = ('a -> 'r) -> 'r

  val return : 'a -> ('a, 'r) t

  val _map: ('a, 'r) t -> ('a -> 'b) -> ('b, 'r) t

  val map : [ `Custom of ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
            | `Define_using_bind ]


  val join : (('a, 'r) t, 'r) t -> ('a, 'r) t

  val chain : ('a, 'r) t -> ('a -> ('b, 'r) t) -> ('b, 'r) t

  val bind : ('a, 'r) t -> f:('a -> ('b, 'r) t) -> ('b, 'r) t

  val run : ('r, 'r) t -> 'r

end = struct

  type ('a, 'r) t = ('a -> 'r) -> 'r

  (* map works over 'a as, according to
     http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Cont.html,
     Cont is a monad over 'a rather than 'r.contents

     In Ocaml terms, could we write a functor that produes a Cont monad for
     a given return type?
  *)

  let return x =
    fun ret -> ret x
  ;;

    
  let _map suspended f =
    fun ret ->
      suspended (fun x -> ret (f x))
  ;;

  let map = `Define_using_bind

  let join (suspended : ((('a -> 'r) -> 'r) -> 'r) -> 'r) =
    fun (ret : ('a -> 'r)) ->
      suspended (fun x -> (ident x) ret)
  ;;

  let chain suspended f =
    join (_map suspended f)
  ;;

  let bind suspended ~f = chain suspended f

  let _chain
      (suspended : ('a, 'r) t)
      (f : ('a -> ('b, 'r) t) )
    : ('b, 'r) t
    =
    (* take a function of 'b -> 'r and return 'r *)
    fun ret ->
      let modified_ret_to_take_a x =
        (f x) ret
      in
      suspended modified_ret_to_take_a 
  ;;

  let run c = c ident

end

module Examples : sig

  val add_cps : int -> int -> (int, 'r) Raw_cont.t
  val sqr_cps : int -> (int, 'r) Raw_cont.t

  val test_chaining : int  val test_chaining_2 : bool

end = struct

  let add_cps x y =
    fun ret -> ret (x + y)
  ;;

  let sqr_cps x =
    fun ret -> ret (x * x)
  ;;

  let test_chaining =
    Raw_cont.run begin
      let suspended = add_cps 10 3 in
      Raw_cont.chain suspended (fun i -> sqr_cps i)
    end
  ;;

  let test_chaining_2 =
    Raw_cont.run begin
      let (>>=) = Raw_cont.chain in
      add_cps 10 3 >>= fun total ->
      sqr_cps total >>= fun squared ->
      (fun ret -> ret (squared > 160))
    end
  ;;

end


module C = struct
  include Monad.Make2(Raw_cont)
end

module Examples2 = struct 

  let add_cps x y =
    fun ret -> ret (x + y)
  ;;

  let sqr_cps x =
    fun ret -> ret (x * x)
  ;;

  let cps_1 =
    let calculation = 
      let open C.Monad_infix in
      add_cps 10 3 >>= fun total ->
      sqr_cps total >>= fun squared ->
      C.return (squared > 160)
    in
    calculation Bool.to_string
  ;;

  let cps_2 =
    let calculation = 
      let open C.Let_syntax in
      let%bind total = add_cps 10 3 in
      sqr_cps total
    in
    calculation ident
  ;;
end

let test_string =
  sprintf !"%{sexp:int}, %{sexp:bool}, %{sexp:string}, %{sexp:int}"
    Examples.test_chaining
    Examples.test_chaining_2
    Examples2.cps_1
    Examples2.cps_2
;;
