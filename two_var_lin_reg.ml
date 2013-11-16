(* @author holman
 *
 *)

module Two_var_lin_reg = struct
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

  let print lin_reg_coeffs = Printf.printf "a: %.2f, b1: %.2f, b2: %.2f\n"
    lin_reg_coeffs.a lin_reg_coeffs.b1 lin_reg_coeffs.b2

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
    let denom = !sum_x1_sq *. !sum_x2_sq -. !sum_x1_x2 ** 2.0 in
    let b1 = (!sum_x2_sq *. !sum_x1_y -. !sum_x1_x2 *. !sum_x2_y) /. denom in
    let b2 = (!sum_x1_sq *. !sum_x2_y -. !sum_x1_x2 *. !sum_x1_y) /. denom in
    let mean_from_sum sum = sum /. !num_points in
    let mean_x1 = mean_from_sum !sum_x1 in
    let mean_x2 = mean_from_sum !sum_x2 in
    Printf.printf "Means %f %f\n" mean_x1 mean_x2;
    let a = (mean_from_sum !sum_y) -. b1 *. mean_x1 -. b2 *. mean_x2 in
    { a = a; b1 = b1; b2 = b2 }
end
