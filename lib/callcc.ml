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

(* Internally, we model callcc continuations as functions with some
   fixed domain A and polymorphic codomain, i.e forall b. A -> b. *)
type 'a continuation = { resume: 'b. 'a -> 'b }


(* [Callcc] is an effectful operation. We rely on the fact that
   performing an operation captures the current continuation of that
   particular operation. *)
type _ Effect.t += Callcc : ('a continuation -> 'a) -> 'a Effect.t
                 | Call1cc : ('a continuation -> 'a) -> 'a Effect.t


(* This exception raised whenever the continuation provided by
   [call1cc] is invoked more than once. *)
exception Continuation_already_resumed

(* The type of [throw] is interesting as it seems to materialise an
   element of type [b] out of thin air. This is merely an illusion as
   an invocation of [throw] never returns to its caller. *)
let throw : 'a 'b. 'a continuation -> 'a -> 'b
  = fun { resume } x -> resume x

(* [callcc f] is a wrapper for [perform (Callcc f)]. *)
let callcc : ('a continuation -> 'a) -> 'a
  = fun f -> Effect.perform (Callcc f)

let call1cc : ('a continuation -> 'a) -> 'a
  = fun f -> Effect.perform (Call1cc f)

(* [hprompt] generates a fresh prompt for delimiting invocations
     of [callcc]. *)
let rec hprompt : 'a. ('a, 'a) Effect.Deep.handler =
    { Effect.Deep.retc = (fun ans -> ans)
    ; Effect.Deep.exnc = raise
    ; Effect.Deep.effc = (fun (type a) (eff : a Effect.t) ->
      match eff with
      | Callcc f ->
         Some (fun (k : (a, _) Effect.Deep.continuation) ->
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
             let exception Done of a in
             let cont : a continuation
               = { resume = fun x -> raise (Throw x) }
             in
             (* The effect handler-captured continuation [r] is
                captured abortively, thus to implement the semantics
                of [callcc] we must invoke it both when [callcc]
                returns successfully and when it returns via an
                invocation of [throw].

                Note: we must reinstate [handle_throw] in the
                exception clause in order to handle any residual
                invocations of [throw] in the continuation.  *)
             let rec handle_throw r f =
               try
                 (* we install a fresh prompt here to handle residual
                    applications of [callcc] in the continuation.

                    Note things are set up such that [Throw] will
                    propagate outside the prompt. *)
                 prompt f
               with
               | Throw ans -> handle_throw r (fun () -> resume r ans)
               | Done ans -> resume r ans
             in
             handle_throw r (fun () ->
                 let ans = f cont in
                 raise (Done ans)))
      | Call1cc f ->
         Some (fun (k : (a, _) Effect.Deep.continuation) ->
             let exception Throw of a in
             let cont : a continuation
               = let used = ref false in
                 { resume = (fun x ->
                     if !used
                     then raise Continuation_already_resumed
                     else (used := true; raise (Throw x))) }
             in
             try
               prompt (fun () ->
                   let ans = f cont in
                   raise (Throw ans))
             with
             | Throw ans -> Effect.Deep.continue k ans)
      | _ -> None) }
and prompt : (unit -> 'a) -> 'a
  = fun f -> Effect.Deep.match_with f () hprompt
