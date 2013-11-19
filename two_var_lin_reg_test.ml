(* @author holman
 *
 * Test for two variable linear regression solver
 *)

open OUnit

let test _ =
  assert_equal 0 0

let test_suite = "Two_var_lin_reg" >::: [
  "test" >:: test
]

(* Run tests *)
let _ = run_test_tt_main test_suite
