(* McCarthy's ambiguity operator. *)

val amb : 'a list -> 'a
(** [amb xs] returns a member of [xs] *)

val collect : (unit -> 'a) -> 'a list
(** [collect f] enumerates all the possible results of [f]
    arising from internal uses of [amb]. *)
