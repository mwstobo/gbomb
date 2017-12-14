module Types = struct
  type video =
    { name: string
    ; guid: string
    ; filename: string
    ; low_url: string
    ; high_url: string
    ; hd_url: string }

  type filters = {limit: int}
end

module Client = struct
  let ( >>= ) = Lwt.( >>= )

  let ua = Cohttp.Header.user_agent

  let default_filters = Types.{limit= 10}

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
    >>= fun body_string -> Lwt.return (Yojson.Basic.from_string body_string)


  let build_video video_json =
    let name_json = Yojson.Basic.Util.member "name" video_json in
    let guid_json = Yojson.Basic.Util.member "guid" video_json in
    let filename_json = Yojson.Basic.Util.member "url" video_json in
    let low_url_json = Yojson.Basic.Util.member "low_url" video_json in
    let high_url_json = Yojson.Basic.Util.member "high_url" video_json in
    let hd_url_json = Yojson.Basic.Util.member "hd_url" video_json in
    let string_fields =
      Yojson.Basic.Util.filter_string
        [ name_json
        ; guid_json
        ; filename_json
        ; low_url_json
        ; high_url_json
        ; hd_url_json ]
    in
    match string_fields with
    | [name; guid; filename; low_url; high_url; hd_url] ->
        Some Types.{name; guid; filename; low_url; high_url; hd_url}
    | _ -> None


  let get_video api_key video_id =
    do_request (build_api_uri api_key ("/video/" ^ video_id))
    >>= fun json ->
    let video_json = Yojson.Basic.Util.member "results" json in
    Lwt.return (build_video video_json)


  let get_videos ?(filters= default_filters) api_key =
    do_request (build_api_uri ~filters api_key "/videos")
    >>= fun json ->
    let results = Yojson.Basic.Util.member "results" json in
    Lwt.return (Yojson.Basic.Util.convert_each build_video results)

end
