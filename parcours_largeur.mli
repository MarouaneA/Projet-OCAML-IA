module G = Genaration

type 'a t = 'a list * 'a list

exception Empty

val empty : 'a t

val is_empty : 'a t -> bool

val add : 'a -> 'a t -> 'a t

val take : 'a t -> 'a * 'a t

val coord_voisin : int -> int -> int -> int * int

val test_voisin : G.case array array -> 'a t -> int -> int -> int -> bool -> G.case array array * 'a t * bool

val pere_fils : G.case array array -> 'a t -> bool

val solve : G.case array array -> int * int -> int * int -> unit
