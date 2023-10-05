(** Felleisen's C operator (ref. Felleisen and Friedman (1987)) *)

module type C = sig
  type ans
  type 'a continuation

  val throw : ans continuation -> ans -> 'b
  val c : (ans continuation -> ans) -> ans
  val prompt : (unit -> ans) -> ans
end

module Make(A : sig type t end): sig
  include C with type ans := A.t
end = struct
  module SR = Shift_reset.Make(A)
  type 'a continuation = 'a SR.continuation

  let throw k x =
    ignore (SR.shift (fun _ -> SR.resume k x));
    assert false
  let c f = SR.shift f
  let prompt = SR.reset
end
