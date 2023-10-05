(** Felleisen (1988)'s control/prompt

    The implementation technique is due to Shan (2004). *)

module type CONTROL_PROMPT = sig
  type ans
  type 'a continuation

  val resume : ans continuation -> ans -> ans
  val prompt : (unit -> ans) -> ans
  val control : (ans continuation -> ans) -> ans
end

module Make(A : sig type t end): sig
  include CONTROL_PROMPT with type ans := A.t
end = struct
  module Context = struct
    type ('a, 'b) t = Context of ('a -> 'b metacontinuation -> 'b)
    and 'a metacontinuation = ('a, 'a) t option

    let send x = function
      | None -> x
      | Some (Context metacont) ->
         metacont x None

    let rec compose ctx metacont =
      match (ctx, metacont) with
      | (Context _, None) -> ctx
      | (Context ctx, Some mc) ->
         Context (fun x mc' -> ctx x (Some (compose mc mc')))
  end

  module SR = Shift_reset.Make(struct
     type t = A.t Context.metacontinuation -> A.t
  end)

  type ans = A.t
  type 'a continuation = ans -> ans

  let resume : ans continuation -> ans -> ans
    = fun k x -> k x

  let prompt : (unit -> ans) -> ans
    = fun f ->
    SR.reset (fun () ->
        Context.send (f ())) None

  let control : (ans continuation -> ans) -> ans
    = fun f ->
    let open Context in
    SR.shift
      (fun c1 (mc1 : ans Context.metacontinuation) ->
        let k (x : ans) =
          SR.shift (fun c2 (mc2 : ans Context.metacontinuation) ->
              let Context c1' = compose (Context (SR.resume c1)) mc1 in
              c1' x (Some (compose (Context (SR.resume c2)) mc2)))
      in
      SR.reset (fun () -> send (f k)) None)
end
