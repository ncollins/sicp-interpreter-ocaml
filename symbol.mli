open! Core

type t [@@deriving sexp, compare]
include Stringable with type t := t
include Comparable with type t := t

val if_ : t
val begin_ : t
val set_ : t
val define_ : t
val let_ : t
val lambda_ : t

val amb_ : t

val true_ : t
val false_ : t
val null_ : t
