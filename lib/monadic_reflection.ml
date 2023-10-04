(** Filinski (1994)'s monadic reflection. *)

module type MONAD = sig
  type +'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module Make(M : MONAD) = struct

  type _ Effect.t += Reflect : 'a M.t -> 'a Effect.t

  let reflect : 'a M.t -> 'a
    = fun m -> Effect.perform (Reflect m)

  let reify : (unit -> 'a) -> 'a M.t
    = fun f ->
    let open M in
    Effect.Deep.(match_with f ()
                   { retc = (fun ans -> return ans)
                   ; exnc = raise
                   ; effc = (fun (type a) (eff : a Effect.t) ->
                     match eff with
                     | Reflect m ->
                        Some (fun (k : (a, _) continuation) ->
                            let open Multicont.Deep in
                            m >>= resume (promote k))
                     | _ -> None) })

end
