type t = Yojson.Basic.json

let of_string s =
  let body_json = Yojson.Basic.from_string s in
  body_json |> Yojson.Basic.Util.member "results"


let rec get_fields_string field_names video =
  match field_names with
  | [] -> [None]
  | field :: rest ->
      let field_val =
        video |> Yojson.Basic.Util.member field |> Yojson.Basic.Util.to_string
      in
      Some field_val :: get_fields_string rest video
