(** Danvy & Filinski (1990)'s shift/reset *)

type ('a, 'b) continuation = ('a, 'b) Prompt.Subcontinuation.t

let resume : 'a 'b. ('a, 'b) continuation -> 'a -> 'b
  = Prompt.Subcontinuation.resume

let shift : 'a 'b. 'a Prompt.t -> (('b, 'a) continuation -> 'a) -> 'b
  = fun p f ->
  Prompt.Subcontinuation.capture p
    (fun subcont ->
      Prompt.push p
        (fun () -> f subcont))

let shift0 : 'a 'b. 'a Prompt.t -> (('b, 'a) continuation -> 'a) -> 'b
  = fun p f ->
  Prompt.Subcontinuation.capture p f

let reset : 'a 'b. ('b Prompt.t -> 'a) -> 'a
  = fun f ->
  let p = Prompt.make () in
  f p

let dollar0 : 'a 'b 'c. ('b Prompt.t -> 'a) -> ('a -> 'c) -> 'c
  = fun f g ->
  let p = Prompt.make () in
  g (f p)

