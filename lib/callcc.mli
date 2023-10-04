(** Call/cc may very well be the most "famous" control operator. It is
    Reynolds' escape operator in a more functional
    disguise. Technically it is an undelimited operator, however, the
    interface presented here is delimited (an application of [callcc]
    or [call1cc] must happen under a [prompt]). The undelimited
    behaviour can be simulated by installing a prompt at the top-level
    of a program.

    Callcc has a non-abortive capture mechanism, i.e.

      (Capture) E[callcc f] ~> E[f `E`],

    where `E` denotes the reified evaluation context. Meanwhile, the
    elimination of callcc continuations is abortive, i.e.

      (Resume) E[throw `E'` x] ~> E'[x],

    in other words: [throw] erases the current evaluation context E
    and reinstates the reified context E' instead. *)

type 'a continuation
(** The type of a continuation reified by either [callcc] or
    [call1cc]. *)

exception Continuation_already_resumed
(** This exception raised whenever a continuation reified by [call1cc]
    is a second time. *)

val throw : 'a continuation -> 'a -> 'b
(** [throw k v] applies the continuation [k] to the value [v] of type
   ['a]. This operation never returns. *)

val callcc : ('a continuation -> 'a) -> 'a
(** [callcc f] captures the current continuation and supplies it to
    the argument [f]. *)

val call1cc : ('a continuation -> 'a) -> 'a
(** [call1cc f] captures the current continuation and supplies it to
    the argument [f]. *)

val prompt : (unit -> 'a) -> 'a
(** [prompt f] delimits the effects of [callcc] and [call1cc] inside [f]. *)
