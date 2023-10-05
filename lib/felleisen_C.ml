(** Felleisen's C operator (ref. Felleisen and Friedman (1987)) *)

module type C = sig
  type ans
  type 'a continuation

  val resume : ans continuation -> ans -> ans
  val c : (ans continuation -> ans) -> ans
  val prompt : (unit -> ans) -> ans
end

module Make(A : sig type t end): sig
  include C with type ans := A.t
end = struct
  module SR = Shift_reset.Make(A)
  type 'a continuation = 'a SR.continuation

  let resume k x = SR.shift (fun _ -> SR.resume k x)
  let c f = SR.shift f
  let prompt = SR.reset
end
