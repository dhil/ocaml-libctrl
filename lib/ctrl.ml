(** This module is largely based on Appendix A of
    https://dhil.net/research/papers/thesis.pdf *)

(** Call/cc may very well be the most "famous" control operator. It is
    Reynolds' escape operator in a more functional
    disguise. Technically it is an undelimited operator, however, the
    implementation given here is delimited (an application of [callcc]
    must happen under a [prompt]). The undelimited behaviour can be
    simulated by installing a prompt at the top-level of a program.

    Callcc has a non-abortive capture mechanism, i.e.

      (Capture)     E[callcc f] ~> E[f `E`],

    where `E` denotes the reified evaluation context. Meanwhile, the
    elimination of callcc continuations is abortive, i.e.

      (Resume)      E[throw `E'` x] ~> E'[x],

    in other words: [throw] erases the current evaluation context E
    and reinstates the reified context E' instead. *)
module Callcc: sig
  type 'a cont
  (** The type of a continuation that returns a value of type ['a]. *)

  val throw : 'a cont -> 'a -> 'b
  (** [throw k x] applies the continuation [k] to the value [x] *)

  val callcc : ('a cont -> 'a) -> 'a
  (** [callcc f] captures and passes the current continuation to
      [f].

      Note: any invocation of [callcc] must happen in the scope of a
      [prompt]. *)

  val call1cc : ('a cont -> 'a) -> 'a
  (** [call1cc f] is the same as [callcc f] except that the provided
      continuation is one-shot *)

  val prompt : (unit -> 'a) -> 'a
  (** [prompt f] delimits invocations of [callcc] within the body of
      [f]. *)
end = struct
  (* Internally, we model callcc continuations as functions with some
     fixed domain A and polymorphic codomain, i.e forall b. A -> b. *)
  type 'a cont = { k: 'b. 'a -> 'b }

  (* [Callcc] is an effectful operation. We rely on the fact that
     performing an operation captures the current continuation of that
     particular operation. *)
  type _ Effect.t += Callcc : ('a cont -> 'a) -> 'a Effect.t
                   | Call1cc : ('a cont -> 'a) -> 'a Effect.t

  (* The type of [throw] is interesting as it seems to materialise an
     element of type [b] out of thin air. This is merely an illusion
     as an invocation of [throw] never returns to its caller. *)
  let throw : 'b. 'a cont -> 'a -> 'b
    = fun { k } x -> k x

  (* [callcc f] is a wrapper for [perform (Callcc f)]. *)
  let callcc : ('a cont -> 'a) -> 'a
    = fun f -> Effect.perform (Callcc f)

  let call1cc : ('a cont -> 'a) -> 'a
    = fun f -> Effect.perform (Call1cc f)

  (* [hprompt ()] generates a fresh prompt for delimiting invocations
     of [callcc]. *)
  let rec hprompt : unit -> ('a, 'a) Effect.Deep.handler
    = fun () ->
    let open Effect.Deep in
    { retc = (fun ans -> ans)
    ; exnc = raise
    ; effc = (fun (type a) (eff : a Effect.t) ->
      match eff with
      | Callcc f ->
         Some (fun (k : (a, _) continuation) ->
             let open Multicont.Deep in
             let r = promote k in
             (* We model the abortive elimination of continuations
                using exceptions.

                We use generative exceptions to make sure that
                controlled gets passed back to *this* particular
                prompt when the continuation has been invoked. Well,
                we are only guaranteed to return to this prompt as
                long as [Throw] doesn't pass through a non-forwarding
                catch-all handler. *)
             let exception Throw of a in
             let cont : a cont
               = { k = fun x -> raise (Throw x) }
             in
             (* The effect handler-captured continuation [r] is
                captured abortively, thus to implement the semantics
                of [callcc] we must invoke it both when [callcc]
                returns successfully and when it returns via an
                invocation of [throw].

                Note: we must reinstate [handle_throw] in the
                exception clause in order to handle any residual
                invocations of [throw] in the continuation.  *)
             let rec handle_throw f =
               try
                 (* we install a fresh prompt here to handle residual
                    applications of [callcc] in the continuation.

                    Note things are set up such that [Throw] will
                    propagate outside the prompt. *)
                 prompt f
             with
             | Throw ans -> handle_throw (fun () -> resume r ans)
             in
             handle_throw (fun () ->
                 let ans = f cont in
                 resume r ans))
      | Call1cc f ->
         Some (fun (k : (a, _) continuation) ->
             let exception Throw of a in
             let cont : a cont
               = let used = ref false in
                 { k = (fun x ->
                     if !used
                     then raise Effect.Continuation_already_resumed (* TODO(dhil): Create a special purpose exception for callcc? *)
                     else (used := true; raise (Throw x))) }
             in
             try
               prompt (fun () ->
                   let ans = f cont in
                   continue k ans)
             with
             | Throw ans -> continue k ans)
      | _ -> None) }
  and prompt : (unit -> 'a) -> 'a
    = fun f -> Effect.Deep.match_with f () (hprompt ())
end

let callcc_ex0 () =
  let open Callcc in
  prompt (fun () ->
      61 + (callcc (fun outer ->
                1 + (callcc (fun inner ->
                         2 + throw outer 3)) + 4)))

let call1cc_ex0 () =
  let open Callcc in
  prompt (fun () ->
      61 + (call1cc (fun outer ->
                1 + (call1cc (fun inner ->
                         2 + throw outer 3)) + 4)))

(* A roundabout way of computing factorial numbers *)
let callcc_fac callcc n =
  Callcc.prompt (fun () ->
      let r = ref (fun _ -> assert false) in
      let c = ref 1 in
      let result = ref 1 in
      let a =
        callcc (fun k ->
            r := Callcc.throw k; !r 1)
      in
      result := a * !result;
      if !c < n
      then (incr c; !r !c)
      else !result)

let callcc_ex1 n = callcc_fac Callcc.callcc n

(* The above method depends on the ability to invoke continuations
   multiple times. The following fails for [n] > 1. *)
let call1cc_ex1 n = callcc_fac Callcc.call1cc n

module type PROMPT = sig
  type 'a t
  type ('a, 'b) continuation

  val make : unit -> 'a t
  val run  : 'a t -> (unit -> 'a) -> 'a
  val reify : 'b t -> (('a, 'b) continuation -> 'b) -> 'a
  val resume : ('a, 'b) continuation -> 'a -> 'b
  val abort : 'a t -> (unit -> 'a) -> 'b
end

module Prompt : PROMPT = struct
  type ('a, 'b) continuation = ('a, 'b) Multicont.Deep.resumption
  type 'a t = { reify: 'b. (('b, 'a) continuation -> 'a) -> 'b
              ; abort: 'b. (unit -> 'a) -> 'b
              ; run: (unit -> 'a) -> 'a }

  let make (type a) : unit -> a t
    = fun () ->
    let module P = struct
        type _ Effect.t += Reify : (('b, a) continuation -> a) -> 'b Effect.t
        exception Abort of (unit -> a)
      end
    in
    let reify f = Effect.perform (P.Reify f) in
    let rec hprompt =
      let open Effect.Deep in
      { retc = (fun x -> x)
      ; exnc = raise
      ; effc = (fun (type b) (eff : b Effect.t) ->
        match eff with
        | P.Reify f ->
           Some
             (fun (k : (b, a) continuation) ->
               let r = Multicont.Deep.promote k in
               try f r with
               | P.Abort f -> f ())
        | _ -> None) }
    in
    let run f =
      Effect.Deep.match_with f () hprompt
    in
    let abort f = raise (P.Abort f) in
    { reify; run; abort }

  let run  : 'a t -> (unit -> 'a) -> 'a
    = fun { run; _ } f -> run f

  let reify : 'b t -> (('a, 'b) continuation -> 'b) -> 'a
    = fun { reify; _ } f -> reify f

  let resume : ('a, 'b) continuation -> 'a -> 'b
    = fun k x -> Multicont.Deep.resume k x

  let abort : 'a t -> (unit -> 'a) -> 'b
    = fun { abort; _ } f -> abort f
end

(* Felleisen's C *)
module C: sig
  type ('a, 'b) continuation

  val resume : 'b Prompt.t -> ('a, 'b) continuation -> 'a -> 'c
  val c : 'a Prompt.t -> (('a, 'a) continuation -> 'a) -> 'a
end = struct
  type ('a, 'b) continuation = ('a, 'b) Prompt.continuation

  let resume p k x = Prompt.abort p (fun () -> Prompt.resume k x)
  let c p f = Prompt.reify p f
end

let c_ex0 () =
  let open C in
  let p = Prompt.make () in
  Prompt.run p (fun () ->
      40 + c p (fun k -> 2))

let c_ex1 () =
  let open C in
  let p = Prompt.make () in
  Prompt.run p (fun () ->
      40 + c p (fun k -> resume p k 2 + resume p k 2))

(* Felleisen's Prompt/Control (or F) *)
module Control: sig
  type ('a, 'b) continuation

  val resume : ('a, 'b) continuation -> 'a -> 'b
  val control : 'a Prompt.t -> (('b, 'a) continuation -> 'a) -> 'b
  val control0 : 'a Prompt.t -> (('b, 'a) continuation -> 'a) -> 'b
  val prompt : ('a Prompt.t -> 'a) -> 'a
end = struct
  type ('a, 'b) continuation = ('a, 'b) Prompt.continuation

  let resume k x = Prompt.resume k x
  let control p f = Prompt.reify p (fun k -> Prompt.run p (fun () -> f k))
  let control0 p f = Prompt.reify p f
  let prompt f =
    let p = Prompt.make () in
    Prompt.run p (fun () -> f p)
end

let f_ex0 () =
  let open Control in
  let p = Prompt.make () in
  Prompt.run p (fun () ->
      40 + control p (fun k -> 2))

let f_ex1 () =
  let open Control in
  let p = Prompt.make () in
  Prompt.run p (fun () ->
      40 + control p (fun k -> resume k 2 + resume k 2))

let f_ex2 () =
  let open Control in
  let p = Prompt.make () in
  Prompt.run p (fun () ->
      40 + control p
             (fun k -> resume k 2 + (control p (fun k -> resume k 2))))

let f_ex2' () =
  let open Control in
  let p = Prompt.make () in
  Prompt.run p (fun () ->
      40 + control0 p
             (fun k -> resume k 2 + (control0 p (fun k -> resume k 2))))


(* Danvy and Filinski's shift/reset *)
(* module Shift: sig *)
(*   type ('a, 'b) continuation *)

(*   val resume : ('a, 'b) continuation -> 'a -> 'b *)
(*   val shift : 'a Prompt.t -> (('b, 'a) continuation -> 'a) -> 'b *)
(*   val shift0 : 'a Prompt.t -> (('b, 'a) continuation -> 'a) -> 'b *)
(*   val reset : ('a Prompt.t -> 'a) -> 'a *)
(* end = struct *)
(*   type ('a, 'b) continuation = { k: 'b. ('a, 'b) Prompt.continuation; p : 'a Prompt.t } *)

(*   let resume { k; p } x = Prompt.resume k x *)
(*   let shift0 p f = *)
(*     Prompt.reify p (fun k -> f { p; k }) *)
(*   let shift f = failwith "TODO" *)

(*   let reset f = *)
(*     let p = Prompt.make () in *)
(*     Prompt.run p (fun () -> f p) *)
(* end *)

(* let shift_ex0 () = *)
(*   let open Shift in *)
(*   1 + reset (fun p -> *)
(*           2 + (shift p (fun k -> 3 + resume k 0)) + (shift p (fun _ -> 4))) *)

(* let shift_ex1 () = *)
(*   let open Shift in *)
(*   2 * reset (fun p -> *)
(*           shift p (fun k -> resume k (resume k 2))) *)

(* let control_ex0 () = *)
(*   let open Control in *)
(*   1 + prompt (fun p -> *)
(*       2 + (control p (fun k -> 3 + resume k 0)) + (control p (fun _ -> 4))) *)
