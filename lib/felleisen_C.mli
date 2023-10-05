 (** Felleisen's C operator (ref. Felleisen and Friedman (1987)) *)

module type C = sig
  type ans
  type 'a continuation

  val resume : ans continuation -> ans -> ans
  val c : (ans continuation -> ans) -> ans
  val prompt : (unit -> ans) -> ans
end

module Make(A : sig type t end) : C with type ans := A.t
