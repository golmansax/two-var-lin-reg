(* @author holman
 *
 *)

open Matrix

module Point = struct
  type t = {
    x1: float;
    x2: float;
    y: float;
  }

  let x1 point = point.x1
  let x2 point = point.x2
  let y point = point.y

  let make x1 x2 y = {
    x1 = x1;
    x2 = x2;
    y = y;
  }

  let print point = Printf.printf "Point -> x1: %f, x2: %f, y: %f\n"
    point.x1 point.x2 point.y
end

type t = {
  a: float;
  b1: float;
  b2: float;
}

let make_coeffs a b1 b2 = {
  a = a;
  b1 = b1;
  b2 = b2;
}

let print lin_reg_coeffs = Printf.printf "a: %.6f, b1: %.6f, b2: %.6f\n"
  lin_reg_coeffs.a lin_reg_coeffs.b1 lin_reg_coeffs.b2

let to_string lin_reg_coeffs = Printf.sprintf "%.6f\n%.6f\n%.6f"
  lin_reg_coeffs.a lin_reg_coeffs.b1 lin_reg_coeffs.b2

(* Returns true if the coeffs are within 1% apart *)
let basically_equal coeffs1 coeffs2 =
  let floats_basically_equal float1 float2 =
    ((float1 -. float2) /. float1) < 0.01
  in
  (floats_basically_equal coeffs1.a coeffs2.a) ||
  (floats_basically_equal coeffs1.b1 coeffs2.b1) ||
  (floats_basically_equal coeffs1.b2 coeffs2.b2)

let solve points =
  let num_points = ref 0.0 in
  let sum_x1 = ref 0.0 in
  let sum_x2 = ref 0.0 in
  let sum_y = ref 0.0 in
  let sum_x1_sq = ref 0.0 in
  let sum_x2_sq = ref 0.0 in
  let sum_x1_x2 = ref 0.0 in
  let sum_x1_y = ref 0.0 in
  let sum_x2_y = ref 0.0 in
  let add_to_ref ref value = ref := !ref +. value in
  let update_vars point =
    let x1 = Point.x1 point in
    let x2 = Point.x2 point in
    let y = Point.y point in
    add_to_ref num_points 1.0;
    add_to_ref sum_x1 x1;
    add_to_ref sum_x2 x2;
    add_to_ref sum_y y;
    add_to_ref sum_x1_sq (x1 ** 2.0);
    add_to_ref sum_x2_sq (x2 ** 2.0);
    add_to_ref sum_x1_x2 (x1 *. x2);
    add_to_ref sum_x1_y (x1 *. y);
    add_to_ref sum_x2_y (x2 *. y)
  in
  List.iter update_vars points;
  let init_for_matrix  = [
    [!sum_x2_sq; !sum_x1_x2; !sum_x2; !sum_x2_y];
    [!sum_x1_x2; !sum_x1_sq; !sum_x1; !sum_x1_y];
    [!sum_x2; !sum_x1; !num_points; !sum_y]
  ] in

  (* This is a helper function that initializes a Matrix from a float list list
   *)
  let init_matrix init_for_matrix =
    let num_rows = List.length init_for_matrix in
    let num_cols = List.length (List.hd init_for_matrix) in
    let matrix = EltMatrix.empty num_rows num_cols in
    let init_row row_num float_list =
      (* We need to first convert our floats to the Elt type that is used by
       * the matrix module
       *)
      let float_to_elt my_float =
        Elts.Elts.from_string (string_of_float my_float)
      in
      let elt_list = List.map float_to_elt float_list in
      EltMatrix.set_row matrix (row_num + 1) (Array.of_list elt_list)
    in
    List.iteri init_row init_for_matrix;
    matrix
  in

  let coeffs_matrix = EltMatrix.row_reduce (init_matrix init_for_matrix) in
  let get_coeff indices =
    (* Again, need to convert back to OCaml's float *)
    let elt = EltMatrix.get_elt coeffs_matrix indices in
    float_of_string (Elts.Elts.to_string elt)
  in
  let b2 = get_coeff (1, 4) in
  let b1 = get_coeff (2, 4) in
  let a = get_coeff (3, 4) in
  { a = a; b1 = b1; b2 = b2 }
