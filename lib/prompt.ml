type 'a t =
  { capture : 'b. (('b, 'a) Multicont.Deep.resumption -> 'a) -> 'b
  ; resume : (unit -> 'a) -> 'a }

module Subcontinuation = struct
  type 'a prompt = 'a t
  type ('a, 'b) t = ('a, 'b) Multicont.Deep.resumption

  let capture : 'b prompt -> (('a, 'b) t -> 'b) -> 'a
    = fun { capture; _ } -> capture

  let resume : ('a, 'b) t -> 'a -> 'b
    = fun r x -> Multicont.Deep.resume r x
end

let make (type a) : unit -> a t
  = fun () ->
  let module M = struct
      type _ Effect.t += Prompt : (('b, a) Subcontinuation.t -> a) -> 'b Effect.t
    end
  in
  let capture f = Effect.perform (M.Prompt f) in
  let resume f =
    Effect.Deep.match_with f ()
      Effect.Deep.({ retc = (fun ans -> ans)
                   ; exnc = raise
                   ; effc = (fun (type a) (eff : a Effect.t) ->
                     match eff with
                     | M.Prompt f ->
                        Some (fun (k : (a, _) continuation) ->
                            f (Multicont.Deep.promote k))
                     | _ -> None) })
  in
  { capture; resume }

let push { resume; _ } = resume
