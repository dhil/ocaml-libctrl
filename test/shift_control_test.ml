module IntDom = struct
  type t = int
end

let shift_ex0 () =
  let module SR = Libctrl.Shift_reset.Make(IntDom) in
  let open SR in
  reset (fun () ->
      let x = shift (fun k -> 10 + resume k 100) in
      let y = shift (fun _ -> 1) in
      x + y)

let shift_ex0_test _ = OUnit2.assert_equal 11 (shift_ex0 ())

let control_ex0 () =
  let module CP = Libctrl.Control_prompt.Make(IntDom) in
  let open CP in
  prompt (fun () ->
      let x = control (fun k -> 10 + resume k 100) in
      let y = control (fun _ -> 1) in
      x + y)

let control_ex0_test _ = OUnit2.assert_equal 1 (control_ex0 ())

let control_ex1 () =
  let module CP = Libctrl.Control_prompt.Make(IntDom) in
  let open CP in
  prompt (fun () ->
      let x = control (fun k -> 10 + (fun v -> prompt (fun () -> resume k v)) 100) in
      let y = control (fun _ -> 1) in
      x + y)

let control_ex1_test _ = OUnit2.assert_equal 11 (control_ex1 ())

module IntListDom = struct
  type t = int list
end

let shift0_ex0 () =
  let module SR = Libctrl.Shift_reset.Make(IntListDom) in
  let open SR in
  reset (fun () ->
      42 :: (reset (fun () -> shift0 (fun _ -> shift0 (fun _ -> [])))))

let shift0_ex0_test _ = OUnit2.assert_equal [] (shift0_ex0 ())

let shift0_ex1 () =
  let module SR = Libctrl.Shift_reset.Make(IntListDom) in
  let open SR in
  reset (fun () ->
      42 :: (reset (fun () -> (reset (fun () -> shift0 (fun _ -> shift0 (fun _ -> [])))))))

let shift0_ex1_test _ = OUnit2.assert_equal [42] (shift0_ex1 ())

let shift0_ex2 () =
  let module SR = Libctrl.Shift_reset.Make(IntListDom) in
  let open SR in
  let yield x =
    shift0 (fun k -> x :: resume k ())
  in
  reset
    (fun () ->
      yield 1;
      yield 2;
      yield 3;
      [])

let shift0_ex2_test _ = OUnit.assert_equal [1;2;3] (shift0_ex2 ())

let ounit2_tests =
  let open OUnit2 in
  "shift-reset_and_control-prompt_testsuite" >::: [
      "shift_ex0_test" >:: shift_ex0_test
    ; "control_ex0_test" >:: control_ex0_test
    ; "control_ex1_test" >:: control_ex1_test
    ; "shift0_ex0_test" >:: shift0_ex0_test
    ; "shift0_ex1_test" >:: shift0_ex1_test
    ; "shift0_ex2_test" >:: shift0_ex2_test]

let _ = OUnit2.run_test_tt_main ounit2_tests
