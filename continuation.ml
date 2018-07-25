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

  val map: ('a, 'r) t -> ('a -> 'b) -> ('b, 'r) t

  val join : (('a, 'r) t, 'r) t -> ('a, 'r) t

  val chain : ('a, 'r) t -> ('a -> ('b, 'r) t) -> ('b, 'r) t

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
    
  let map (suspended : ('a, 'r) t) (f : ('a -> 'b)) =
    fun (ret : ('b -> 'r)) ->
      suspended (fun x -> ret (f x))
  ;;

  let join (suspended : ((('a -> 'r) -> 'r) -> 'r) -> 'r) =
    fun (ret : ('a -> 'r)) ->
      suspended (fun x -> (ident x) ret)
  ;;

  let chain suspended f =
    join (map suspended f)
  ;;

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

module type Cont = sig

  type s

  type 'a t

  val return : 'a -> 'a t

  val map :  [ `Custom of 'a t -> f:('a -> 'b) -> 'b t | `Define_using_bind ]
  (*
  val map : 'a t -> f:('a -> 'b) -> 'b t

  val join : ('a t) t -> ('a t)
  *)

  val bind : ('a t) -> f:('a -> 'b t) -> ('b t)

  val run : ('a t) -> final:('a -> s) -> s

end


module type X = sig
  type t
end

module Make_cont' (M : X) : Cont = struct

  type s = M.t

  type 'a t = ('a, M.t) Raw_cont.t

  let return x = Raw_cont.return x

  let map = `Define_using_bind
  (*
  let map t ~f = Raw_cont.map t f

  let join t = Raw_cont.join t
  *)

  let bind t ~f = Raw_cont.chain t f

  let run t ~final = t final

end

module Make_cont (M : X) = struct

  module C : Cont =  Make_cont'(M)
  include C
  include Monad.Make(C)

end


module Examples2 = struct 

  module String_cont = Make_cont(struct type t = string end)
      
  let add_cps x y =
    fun ret -> ret (x + y)
  ;;

  let sqr_cps x =
    fun ret -> ret (x * x)
  ;;

  let cps_1 =
    let open String_cont.Monad_infix in
    let _c1 = add_cps 10 3 in
    let _c2 = _c1 >>= sqr_cps in
    _c2 (fun i -> Int.to_string i)
  ;;
end

let test_string =
  sprintf !"%{sexp:int}, %{sexp:bool}"
    Examples.test_chaining
    Examples.test_chaining_2
;;
