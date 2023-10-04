(* McCarthy's Amb operator. *)

type _ Effect.t += Choose : 'a list -> 'a Effect.t

let amb : 'a list -> 'a
  = fun xs -> Effect.perform (Choose xs)

let collect : (unit -> 'a) -> 'a list
  = fun f ->
  Effect.Deep.match_with f ()
    Effect.Deep.({ retc = (fun ans -> [ans])
                 ; exnc = (fun _ -> [])
                 ; effc = (fun (type a) (eff : a Effect.t) ->
                   match eff with
                   | Choose xs ->
                      Some (fun (k : (a, _) continuation) ->
                          let open Multicont.Deep in
                          let r = promote k in
                          List.concat (List.map (resume r) xs))
                   | _ -> None) })
