(** Kiselyov (2012)'s interface for typed multi-prompt
   continuations. *)
type 'a t

val make : unit -> 'a t
val push : 'a t -> (unit -> 'a) -> 'a

module Subcontinuation: sig
  type 'a prompt = 'a t
  type ('a, 'b) t

  val capture : 'b prompt -> (('a, 'b) t -> 'b) -> 'a
  val resume : ('a, 'b) t -> 'a -> 'b
end
