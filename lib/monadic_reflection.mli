(** Filinski (1994)'s monadic reflection.

   Monadic reflection is a cunning technique for programming with
   monads in direct-style. The technique makes crucial use of
   composable continuations to jump in and out of a given monad. Thus,
   monadic reflection provides a means for getting the best of both
   worlds: the lightweight effect tracking of monads with the
   convenience of programming in direct-style. *)

module type MONAD = sig
  type +'a t
  (** The type of the monad structure. *)

  val return : 'a -> 'a t
  (** [return x] lifts the value [x] into the monad structure. *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** [m >>= k] extracts the value embedded in [m] and supplies
      it to the continuation [k]. *)
end
(** The signature of a monad. *)

module Make(M : MONAD): sig
  val reflect : 'a M.t -> 'a
  (** [reflect m] jumps out of the monad, i.e. it extracts the value
      embedded inside the monad structure. *)

  val reify : (unit -> 'a) -> 'a M.t
  (** [reify f] runs the computation [f] inside the monad structure.
      This operator delimits the extent of the effects of the
      [reflect] operator. *)
end
(** [Make(M)] is a functor which constructs the [reflect] and [reify]
    operators for the monad [M]. *)
