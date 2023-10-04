(** Danvy & Filinski (1990)'s shift/reset (and friends) *)

type ('a, 'b) continuation

val resume : ('a, 'b) continuation -> 'a -> 'b

val shift : 'a Prompt.t -> (('b, 'a) continuation -> 'a) -> 'b
val shift0 : 'a Prompt.t -> (('b, 'a) continuation -> 'a) -> 'b
val reset : ('b Prompt.t -> 'a) -> 'a
val dollar0 : ('b Prompt.t -> 'a) -> ('a -> 'c) -> 'c
