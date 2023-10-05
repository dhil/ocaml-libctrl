let c_ex0 () =
  let module C = Libctrl.Felleisen_C.Make(struct type t = int end) in
  let open C in
  prompt (fun () ->
      40 + (c (fun k ->
                throw k 2 + throw k 2)))

let test_c_ex0 _ = OUnit2.assert_equal 42 (c_ex0 ())

let c_ex1 () =
  let module C = Libctrl.Felleisen_C.Make(struct type t = int end) in
  let open C in
  prompt (fun () ->
      40 + (c (fun _ -> 2)))

let test_c_ex1 _ = OUnit2.assert_equal 2 (c_ex1 ())

let ounit2_tests =
  let open OUnit2 in
  "test suite for C" >::: [
      "c_ex0" >:: test_c_ex0
    ; "c_ex1" >:: test_c_ex1 ]

let _ = OUnit2.run_test_tt_main ounit2_tests

