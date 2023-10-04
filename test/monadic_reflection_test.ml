let state_tests =
  (* The integer state monad *)
  let module State = struct
      type 'a t = int -> 'a * int

      let return v st = (v, st)

      let (>>=) m k st =
        let (ans, st) = m st in
        k ans st

      let get st = (st, st)

      let put st _ = ((), st)

      let run st ~init = st init
    end
  in
  (* Monadic reflection for state *)
  let module StateR = Libctrl.Monadic_reflection.Make(State) in
  let put : int -> unit
    = fun x -> StateR.reflect (State.put x)
  in
  let get : unit -> int
    = fun () -> StateR.reflect State.get
  in
  let run_state f ~init =
    State.run (StateR.reify f) ~init
  in
  let get_get =
    QCheck.Test.make ~count:1000 ~name:"State get-get"
      QCheck.int
      (fun n -> Int.equal (fst (run_state ~init:n (fun () -> ignore (get ()); get ()))) n)
  in
  let get_put =
    QCheck.Test.make ~count:1000 ~name:"State get-put"
      QCheck.int
      (fun n -> Int.equal (snd (run_state ~init:n (fun () -> ignore (get ()); put (n + 1)))) (n + 1))
  in
  let put_get =
    QCheck.Test.make ~count:1000 ~name:"State put-get"
      QCheck.int
      (fun n -> Int.equal (fst (run_state ~init:0 (fun () -> put n; get ()))) n)
  in
  let put_put =
    QCheck.Test.make ~count:1000 ~name:"State put-put"
      QCheck.int
      (fun n -> Int.equal (snd (run_state ~init:0 (fun () -> put n; (put (n + 1))))) (n+1))
  in
  [get_get; get_put; put_get; put_put]

let tests =
  let open OUnit2 in
  "test suite for monadic reflection" >:::
    (List.map QCheck_ounit.to_ounit2_test state_tests)

let _ = OUnit2.run_test_tt_main tests
