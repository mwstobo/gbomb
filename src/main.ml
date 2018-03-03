type action = Invalid | Download of string * string | Videos of int

let ( >>= ) = Lwt.( >>= )

let create_download opts =
  match opts with
  | [(Cli.Arg id); (Cli.Param ("quality", qual))]
  | [(Cli.Param ("quality", qual)); (Cli.Arg id)] ->
      Download (id, qual)
  | [(Cli.Arg id)] -> Download (id, "high")
  | _ -> Invalid


let create_videos opts =
  match opts with
  | [(Cli.Param ("limit", limit_str))] -> (
    match int_of_string_opt limit_str with
    | Some limit -> Videos limit
    | None -> Invalid )
  | [] -> Videos 10
  | _ -> Invalid


let get_action opts =
  match opts with
  | (Cli.Arg "download") :: opts -> create_download opts
  | (Cli.Arg "videos") :: opts | (Cli.Arg "list") :: opts -> create_videos opts
  | _ -> Invalid


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


let rec save_stream st oc =
  Lwt_stream.get st
  >>= fun part_opt ->
  match part_opt with
  | None -> close_out oc ; Lwt.return (print_endline "Done!")
  | Some part -> output_string oc part ; save_stream st oc


let download_video api_key video_id quality =
  Giantbomb.video_get api_key video_id
  >>= fun video_result ->
  match video_result with
  | Error s -> Lwt.return (print_endline s)
  | Ok video ->
      let url_option =
        match quality with
        | "high" -> video.Giantbomb.Resources.high_url
        | "hd" -> video.Giantbomb.Resources.hd_url
        | _ -> video.Giantbomb.Resources.low_url
      in
      match url_option with
      | Some url ->
          let filename = video.Giantbomb.Resources.url in
          let authed_url = url ^ "?api_key=" ^ api_key in
          let ua = Cohttp.Header.user_agent in
          let headers = Cohttp.Header.init_with "User-Agent" ua in
          Cohttp_lwt_unix.Client.get ~headers (Uri.of_string authed_url)
          >>= fun (_, body) ->
          let oc = open_out filename in
          print_endline ("Starting download of " ^ filename) ;
          save_stream (Cohttp_lwt.Body.to_stream body) oc
      | None -> Lwt.return (print_endline "Specified quality not found!")


let rec print_videos video_opts =
  match video_opts with
  | [] -> ()
  | video :: rest ->
      let length = video.Giantbomb.Resources.length_seconds in
      let hours = length / 60 / 60 in
      let minutes = length / 60 mod 60 in
      let seconds = length mod 60 in
      Format.printf "%s: %s (%dh %dm %ds)\n" video.Giantbomb.Resources.guid
        video.Giantbomb.Resources.name hours minutes seconds ;
      print_videos rest


let list_videos api_key limit =
  Giantbomb.videos_get ~limit api_key
  >>= fun result ->
  match result with
  | Error s -> Lwt.return (print_endline s)
  | Ok videos -> Lwt.return (print_videos videos)


let () =
  let args = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  let opts = Cli.parse_options (Array.to_list args) in
  let api_key_opt = get_api_key in
  match api_key_opt with
  | None -> print_endline "No API key provided!"
  | Some api_key ->
      let action = get_action opts in
      match action with
      | Download (video_id, quality) ->
          Lwt_main.run (download_video api_key video_id quality)
      | Videos limit -> Lwt_main.run (list_videos api_key limit)
      | _ -> print_endline "Invalid action!"
