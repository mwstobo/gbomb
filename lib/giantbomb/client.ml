let ( >>= ) = Lwt.( >>= )

let ua = Cohttp.Header.user_agent

let default_filters = Types.{limit=10}

let build_api_uri ?(filters= default_filters) api_key resource =
  let api_base = "https://www.giantbomb.com/api" in
  let authed_path = resource ^ "/?api_key=" ^ api_key ^ "&format=json" in
  let filtered_path = "&limit=" ^ string_of_int filters.Types.limit in
  Uri.of_string (api_base ^ authed_path ^ filtered_path)


let do_request uri =
  let headers = Cohttp.Header.init_with "User-Agent" ua in
  Cohttp_lwt_unix.Client.get ~headers uri
  >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body
  >>= fun body_string -> Lwt.return (Yojson.Safe.from_string body_string)


let return_result response json_deserializer =
  response
  >>= fun json ->
  let results = Yojson.Safe.Util.member "results" json in
  Lwt.return
    ( match json_deserializer results with
    | Result.Error s ->
        Types.Error ("failed to deserialize json with error: " ^ s)
    | Result.Ok deserialized -> Types.Ok deserialized )


let get_video api_key video_id =
  let resp = do_request (build_api_uri api_key ("/video/" ^ video_id)) in
  return_result resp Types.video_of_yojson


let get_videos ?(filters= default_filters) api_key =
  let resp = do_request (build_api_uri ~filters api_key "/videos") in
  return_result resp Types.videos_of_yojson
