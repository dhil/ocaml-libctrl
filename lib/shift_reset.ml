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

let dollar0 : 'a 'b. ('b Prompt.t -> 'a) -> ('a -> 'b) -> 'b
  = fun f g ->
  reset (fun p ->
      let ans = f p in
      shift0 p (fun _ -> g ans))

