type t = Yojson.Basic.json

let field m video =
  video |> Yojson.Basic.Util.member m |> Yojson.Basic.Util.to_string
  |> Field.of_string


let of_string s =
  let body_json = Yojson.Basic.from_string s in
  body_json |> Yojson.Basic.Util.member "results"


let build_api_uri api_key resource =
  let api_base = "https://www.giantbomb.com/api" in
  let url_s =
    Printf.sprintf "%s%s/?api_key=%s&format=json" api_base resource api_key
  in
  Uri.of_string url_s


let make_request uri =
  let ( >>= ) = Lwt.( >>= ) in
  let ua = Cohttp.Header.user_agent in
  let headers = Cohttp.Header.init_with "User-Agent" ua in
  Cohttp_lwt_unix.Client.get ~headers uri
  >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body
  >>= fun body_string -> Lwt.return (of_string body_string)


let get_resource api_key resource =
  make_request (build_api_uri api_key resource)


let get_video api_key video_id =
  get_resource api_key (Printf.sprintf "/video/%s" video_id)
