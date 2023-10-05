(** Felleisen (1988)'s control/prompt. *)

module type CONTROL_PROMPT = sig
  type ans
  type 'a continuation

  val resume : ans continuation -> ans -> ans
  val prompt : (unit -> ans) -> ans
  val control : (ans continuation -> ans) -> ans
end

module Make(A : sig type t end) : CONTROL_PROMPT with type ans := A.t
