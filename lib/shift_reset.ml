(** Danvy & Filinski (1990)'s shift/reset *)

type ('a, 'b) continuation = ('a, 'b) Prompt.Subcontinuation.t

let resume : 'a 'b. ('a, 'b) continuation -> 'a -> 'b
  = fun k x -> Prompt.Subcontinuation.resume k x

let shift : 'a 'b. 'a Prompt.t -> (('b, 'a) continuation -> 'a) -> 'b
  = fun p f ->
  Prompt.Subcontinuation.capture p (fun subcont ->
      Prompt.push p (fun () -> f subcont))

let shift0 : 'a 'b. 'a Prompt.t -> (('b, 'a) continuation -> 'a) -> 'b
  = fun p f ->
  Prompt.Subcontinuation.capture p f

let reset : 'a. ('a Prompt.t -> 'a) -> 'a
  = fun f ->
  let p = Prompt.make () in
  Prompt.push p (fun () -> f p)

(* This encoding is adapted from Materzok and Biernacki (2012). *)
let dollar0 : 'a 'b. ('b Prompt.t -> 'a) -> ('a -> 'b) -> 'b
  = fun f g ->
  reset (fun p ->
      let ans = f p in
      shift0 p (fun _ -> g ans))

module type SHIFT_RESET = sig
  type ans
  type 'a continuation

  val resume : 'a continuation -> 'a -> ans
  val shift0 : ('a continuation -> ans) -> 'a
  val shift  : ('a continuation -> ans) -> 'a
  val reset  : (unit -> ans) -> ans
  val dollar0 : (unit -> 'a) -> ('a -> ans) -> ans
end

(* A non-prompt variation of shift/reset. It much be instantiated at a
   fixed intermediate answer type [A.t]. *)
module Make(A : sig type t end): sig
  include SHIFT_RESET with type ans := A.t
end = struct
  type ans = A.t
  type 'a continuation = ('a, ans) Multicont.Deep.resumption

  type _ Effect.t += Shift0 : ('a continuation -> ans) -> 'a Effect.t
                   | Shift : ('a continuation -> ans) -> 'a Effect.t

  let resume : 'a. 'a continuation -> 'a -> ans
    = Multicont.Deep.resume
  let shift0 : 'a. ('a continuation -> ans) -> 'a
    = fun f -> Effect.perform (Shift0 f)
  let shift : 'a. ('a continuation -> ans) -> 'a
    = fun f -> Effect.perform (Shift f)

  let rec reset f =
    Effect.Deep.match_with f ()
      Effect.Deep.({ retc = (fun ans -> ans)
                   ; exnc = raise
                   ; effc = (fun (type a) (eff : a Effect.t) ->
                     match eff with
                     | Shift0 f ->
                        Some (fun (k : (a, ans) continuation) ->
                            f (Multicont.Deep.promote k))
                     | Shift f ->
                        Some (fun (k : (a, ans) continuation) ->
                            reset (fun () -> f (Multicont.Deep.promote k)))
                     | _ -> None) })

  let dollar0 : 'a. (unit -> 'a) -> ('a -> ans) -> ans
    = fun f g ->
    reset (fun () ->
        let ans = f () in
        shift0 (fun _ -> g ans))
end
