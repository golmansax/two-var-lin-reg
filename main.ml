(* @author holman
 *
 * This is meant to be run on golmansax/gender-guess
 * TODO make this more modular
 *)

open Sqlite3
open Two_var_lin_reg

module Person = struct
  type t = {
    height: float;
    weight: float;
    gender: float;
  }

  let make height weight gender = {
    height = height;
    weight = weight;
    gender = gender;
  }

  let to_two_var_lin_reg_point person =
    Two_var_lin_reg.Point.make person.height person.weight person.gender
end

let eval_gender gender = match gender with
| "M" -> 1.0
| "F" -> -1.0
| _ -> assert false

let get_people () =
  let db = db_open ~mode:`READONLY "../gender-guess/db/development.sqlite3"
  in
  try
    let people = ref [] in
    let sql = Printf.sprintf "SELECT height, weight, gender FROM people" in
    let cb row headers =
      let extract_from_row idx = match row.(idx) with
      | Some str -> str
      | None -> assert false
      in
      let height = float_of_string (extract_from_row 0) in
      let weight = float_of_string (extract_from_row 1) in
      let gender_float = eval_gender (extract_from_row 2) in
      let person = Person.make height weight gender_float in
      people := !people @ [person]
    in
    match exec db sql ~cb:cb with
    | Rc.OK -> !people
    | _ -> assert false
  with xcp -> assert false

let () =
  let people = get_people () in
  let points = List.map Person.to_two_var_lin_reg_point people in
  List.iter Two_var_lin_reg.Point.print points;
  let lin_reg_coeffs = Two_var_lin_reg.solve points in
  Two_var_lin_reg.print lin_reg_coeffs
