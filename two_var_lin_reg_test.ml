(* @author holman
 *
 * Test for two variable linear regression solver
 *)

open OUnit

(* The extra parameter is to make it return a function *)
let test_file file_name _ =
  let in_channel = Scanf.Scanning.open_in file_name in
  try
    (* Format of file is:
     * 1. Expected coefficients (a b1 b2)
     * 2 - eof. Data points (x1 x2 y)
     *)
    let analyze_coeffs a b1 b2 =
      let points = ref [] in
      let add_point x1 x2 y =
        let point = Two_var_lin_reg.Point.make x1 x2 y in
        points := !points @ [point]
      in
      while not (Scanf.Scanning.end_of_input in_channel) do
        Scanf.bscanf in_channel "%f %f %f\n" add_point
      done;
      let input_coeffs = Two_var_lin_reg.make_coeffs a b1 b2 in
      let generated_coeffs = Two_var_lin_reg.solve !points in
      assert_bool
        "Generated coeffs do not match input coeffs"
        (Two_var_lin_reg.basically_equal input_coeffs generated_coeffs)
    in
    Scanf.bscanf in_channel "%f %f %f\n" analyze_coeffs
  with e-> assert false

let test_suite =
  let test_files = [
    "test_01.txt"
  ] in
  let to_test file_name = file_name >:: (test_file file_name) in
  "Two_var_lin_reg" >::: (List.map to_test test_files)

(* Run tests *)
let _ = run_test_tt_main test_suite
