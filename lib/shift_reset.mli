(** Danvy & Filinski (1990)'s shift/reset (and friends) *)

type ('a, 'b) continuation

val resume : ('a, 'b) continuation -> 'a -> 'b

val shift : 'a Prompt.t -> (('b, 'a) continuation -> 'a) -> 'b
val shift0 : 'a Prompt.t -> (('b, 'a) continuation -> 'a) -> 'b
val reset : ('a Prompt.t -> 'a) -> 'a
val dollar0 : ('b Prompt.t -> 'a) -> ('a -> 'b) -> 'b

module type SHIFT_RESET = sig
  type ans
  type 'a continuation

  val resume : 'a continuation -> 'a -> ans
  val shift0 : ('a continuation -> ans) -> 'a
  val shift  : ('a continuation -> ans) -> 'a
  val reset  : (unit -> ans) -> ans
  val dollar0 : (unit -> 'a) -> ('a -> ans) -> ans
end

module Make(A : sig type t end) : SHIFT_RESET with type ans := A.t
(** A non-prompt variation of shift/reset. It much be instantiated at a
    fixed intermediate answer type [A.t]. *)

