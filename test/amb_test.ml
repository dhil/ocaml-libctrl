open Libctrl.Amb

let qcheck_tests =
  let id_tests =
    QCheck.Test.make ~count:1000 ~name:"amb identity"
      QCheck.(list int)
      (fun xs -> List.equal Int.equal (collect (fun () -> amb xs)) xs)
  in
  [id_tests]

let ounit2_tests =
  let coin_toss_tests =
    let module Coin = struct
        type toss = Heads | Tails
        exception Fail

        let toss () =
          amb [Heads; Tails]

        let drunk_toss () =
          if amb [true; false]
          then toss ()
          else raise Fail
      end
    in
    OUnit2.([ "drunken coin toss test" >::
               (fun _ -> assert_equal (collect Coin.drunk_toss) Coin.([Heads; Tails]))
            ; "coin toss test" >::
                (fun _ -> assert_equal (collect Coin.toss) Coin.([Heads; Tails])) ])
  in
  coin_toss_tests

let testsuite =
  let open OUnit2 in
  "test suite for amb" >:::
    (ounit2_tests
     @ (List.map QCheck_ounit.to_ounit2_test qcheck_tests))

let _ = OUnit2.run_test_tt_main testsuite
