open Libctrl.Callcc

let callcc_ex0 callcc =
  prompt (fun () ->
      61 + (callcc (fun outer ->
                1 + (callcc (fun _inner ->
                         2 + throw outer 3)) + 4)))

let test_callcc_ex0  _ = OUnit2.assert_equal (callcc_ex0 callcc)  64
let test_call1cc_ex0 _ = OUnit2.assert_equal (callcc_ex0 call1cc) 64

let callcc_ex1 callcc =
  prompt (fun () ->
      61 + (callcc (fun _outer ->
                1 + (callcc (fun _inner ->
                         2)) + 4)))

let test_callcc_ex1  _ = OUnit2.assert_equal (callcc_ex1 callcc)  68
let test_call1cc_ex1 _ = OUnit2.assert_equal (callcc_ex1 call1cc) 68

let callcc_ex2 callcc =
  prompt (fun () ->
      61 + (callcc (fun outer ->
                1 + (callcc (fun _inner ->
                         2)) + (throw outer 8) + 4)))

let test_callcc_ex2  _ = OUnit2.assert_equal (callcc_ex2 callcc)  69
let test_call1cc_ex2 _ = OUnit2.assert_equal (callcc_ex2 call1cc) 69

(* A roundabout way of computing factorial numbers *)
let callcc_fac callcc n =
  prompt (fun () ->
      let cc = ref (fun _ -> assert false) in
      let i = Sys.opaque_identity (ref 1) in
      let result = Sys.opaque_identity (ref 1) in
      let a =
        callcc (fun k ->
            cc := throw k; !cc 1)
      in
      result := a * !result;
      if !i < n
      then (incr i; !cc !i)
      else !result)

let rec factorial n =
  assert (n >= 0);
  if n = 0 then 1
  else n * (factorial (n - 1))

let call1cc_fac n = callcc_fac call1cc n
let callcc_fac n = callcc_fac callcc n

let test_callcc_fac _ = OUnit2.assert_equal (callcc_fac 5) (factorial 5)

(* The [callcc_fac] function depends on multi-shot continuations. So,
   [call1cc_fac] fails for [n] > 1. *)
let test_call1cc_fac_1 _ = OUnit2.assert_equal (call1cc_fac 1) (factorial 1)
let test_call1cc_fac_2 _ = OUnit2.assert_equal true
                             (try ignore (call1cc_fac 5); false with
                              | Continuation_already_resumed -> true)

let qcheck_tests =
  let factorial_tests =
    let passing =
      QCheck.Test.make ~count:25 ~name:"callcc factorial"
        QCheck.(int_range 0 20)
        (fun n -> Int.equal (callcc_fac n) (factorial n))
    in
    let failing =
      QCheck.(Test.make ~count:25 ~name:"call1cc factorial"
                (int_range 0 20)
                (fun n ->
                  assume (n > 1);
                  (try ignore (call1cc_fac n); false
                   with Continuation_already_resumed -> true)))
    in
    [passing; failing]
  in
  let id_tests =
    let drop_k =
      QCheck.Test.make ~count:1000 ~name:"callcc id"
        QCheck.int
        (fun n -> Int.equal (prompt (fun () -> callcc (fun _ -> n))) n)
    in
    let invoke_k =
      QCheck.Test.make ~count:1000 ~name:"callcc id"
        QCheck.int
        (fun n -> Int.equal (prompt (fun () -> callcc (fun k -> throw k n))) n)
    in
    [drop_k; invoke_k]
  in
  id_tests @ factorial_tests

let ounit2_tests =
  let open OUnit2 in
  "test suite for callcc" >::: [
      "callcc_ex0" >:: test_callcc_ex0
    ; "call1cc_ex0" >:: test_call1cc_ex0
    ; "callcc_ex1" >:: test_callcc_ex1
    ; "call1cc_ex1" >:: test_call1cc_ex1
    ; "callcc_ex2" >:: test_callcc_ex2
    ; "call1cc_ex2" >:: test_call1cc_ex2
    ; "callcc_fac" >:: test_callcc_fac
    ; "call1cc_fac_1" >:: test_call1cc_fac_1
    ; "call1cc_fac_2" >:: test_call1cc_fac_2 ]
    @ List.map QCheck_ounit.to_ounit2_test qcheck_tests

let _ = OUnit2.run_test_tt_main ounit2_tests
