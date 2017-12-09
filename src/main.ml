type action = Invalid | Download of string * string

let create_download opts =
  match opts with
  | [(Cli.Arg id); (Cli.Flag ("quality", qual))]
  | [(Cli.Flag ("quality", qual)); (Cli.Arg id)] ->
      Download (id, qual)
  | _ -> Invalid


let get_action opts =
  match opts with
  | (Cli.Arg "download") :: opts -> create_download opts
  | _ -> Invalid


let download_video api_key video_id quality =
  let video = Lwt_main.run (Giantbomb.Resource.get_video api_key video_id) in
  let high_url_field = Giantbomb.Resource.field quality video in
  match Giantbomb.Field.to_string_opt high_url_field with
  | Some s -> print_endline s
  | _ -> print_endline "Not found!"


let get_api_key =
  let api_key_opt = Sys.getenv_opt "GIANTBOMB_API_KEY" in
  match api_key_opt with
  | Some api_key -> Some api_key
  | None ->
    match Sys.getenv_opt "HOME" with
    | None -> None
    | Some home ->
        let filename = home ^ "/.giantbomb/api_key" in
        if Sys.file_exists filename then
          let ic = open_in filename in
          let api_key = input_line ic in
          close_in ic ; Some api_key
        else None


let () =
  let args = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  let opts = Cli.parse_options (Array.to_list args) in
  let api_key_opt = get_api_key in
  match api_key_opt with
  | None -> print_endline "No API key provided!"
  | Some api_key ->
      let action = get_action opts in
      match action with
      | Download (video_id, quality) -> download_video api_key video_id quality
      | _ -> print_endline "Invalid action!"
