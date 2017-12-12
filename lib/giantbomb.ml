module Types = struct
  type video =
    { name: string
    ; filename: string
    ; low_url: string
    ; high_url: string
    ; hd_url: string }
end

module Client = struct
  let ( >>= ) = Lwt.( >>= )

  let ua = Cohttp.Header.user_agent

  let build_api_uri api_key resource =
    let api_base = "https://www.giantbomb.com/api" in
    let authed_path = resource ^ "/?api_key=" ^ api_key ^ "&format=json" in
    Uri.of_string (api_base ^ authed_path)


  let do_request uri =
    let headers = Cohttp.Header.init_with "User-Agent" ua in
    Cohttp_lwt_unix.Client.get ~headers uri
    >>= fun (_, body) ->
    Cohttp_lwt.Body.to_string body
    >>= fun body_string -> Lwt.return (Yojson.Basic.from_string body_string)


  let get_video api_key video_id =
    do_request (build_api_uri api_key ("/video/" ^ video_id))
    >>= fun json ->
    let results = Yojson.Basic.Util.member "results" json in
    let name_json = Yojson.Basic.Util.member "name" results in
    let filename_json = Yojson.Basic.Util.member "url" results in
    let low_url_json = Yojson.Basic.Util.member "low_url" results in
    let high_url_json = Yojson.Basic.Util.member "high_url" results in
    let hd_url_json = Yojson.Basic.Util.member "hd_url" results in
    let string_fields =
      Yojson.Basic.Util.filter_string
        [name_json; filename_json; low_url_json; high_url_json; hd_url_json]
    in
    match string_fields with
    | [name; filename; low_url; high_url; hd_url] ->
        Lwt.return (Some Types.{name; filename; low_url; high_url; hd_url})
    | _ -> Lwt.return None

end
