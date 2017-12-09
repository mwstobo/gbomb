type t = String of string | Int of int

let to_string_opt f =
  match f with
  | String s -> Some s
  | _ -> None

let to_int_opt f =
  match f with
  | Int i -> Some i
  | _ -> None

let of_string s =
  match int_of_string_opt s with
  | Some i -> Int i
  | None -> String s
