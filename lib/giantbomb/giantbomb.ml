module Video = struct
  type t =
    { id: int
    ; guid: string
    ; name: string
    ; url: string
    ; low_url: string option
    ; high_url: string option
    ; hd_url: string option
    ; length_seconds: int
    ; saved_time: string option }

  let of_json_obj json =
    let open Yojson.Basic.Util in
    { id= member "id" json |> to_int
    ; guid= member "guid" json |> to_string
    ; name= member "name" json |> to_string
    ; url= member "url" json |> to_string
    ; low_url= member "low_url" json |> to_string_option
    ; high_url= member "high_url" json |> to_string_option
    ; hd_url= member "hd_url" json |> to_string_option
    ; length_seconds= member "length_seconds" json |> to_int
    ; saved_time= member "saved_time" json |> to_string_option }

  let of_json json =
    let open Yojson.Basic.Util in
    let results = member "results" json in
    try Ok (of_json_obj results) with Type_error (s, _) -> Error s
end

module Videos = struct
  type t = Video.t list

  let of_json json =
    let open Yojson.Basic.Util in
    let accumulate_videos videos json = videos @ [Video.of_json_obj json] in
    let results = member "results" json |> to_list in
    try Ok (List.fold_left accumulate_videos [] results)
    with Type_error (s, _) -> Error s
end

module VideoShow = struct
  type t = {id: int; guid: string; title: string}

  let of_json_obj json =
    let open Yojson.Basic.Util in
    { id= member "id" json |> to_int
    ; guid= member "guid" json |> to_string
    ; title= member "title" json |> to_string }

  let of_json json =
    let open Yojson.Basic.Util in
    let results = member "results" json in
    try Ok (of_json_obj results) with Type_error (s, _) -> Error s
end

module VideoShows = struct
  type t = VideoShow.t list

  let of_json json =
    let open Yojson.Basic.Util in
    let accumulate_video_shows video_shows json =
      video_shows @ [VideoShow.of_json_obj json]
    in
    let results = member "results" json |> to_list in
    try Ok (List.fold_left accumulate_video_shows [] results)
    with Type_error (s, _) -> Error s
end

module SaveTime = struct
  type t = unit

  let of_json _json = Ok ()
end

module Response = struct
  type 'a t = Ok of 'a | JsonError of string | HttpError of int

  let return a = Ok a

  let _json_error e = JsonError e

  let http_error c = HttpError c

  let bind resp f =
    match resp with
    | JsonError e -> JsonError e
    | HttpError c -> HttpError c
    | Ok a -> f a

  let rbind f resp = bind resp f

  let map resp f = bind resp (fun a -> return (f a))

  let rmap f resp = map resp f
end

module Client = struct
  open Lwt.Infix

  type 'a request =
    | VideoRequest : string -> Video.t request
    | VideosRequest : int * int option -> Videos.t request
    | VideoShowRequest : string -> VideoShow.t request
    | VideoShowsRequest : int -> VideoShows.t request
    | SaveTimeRequest : int * int -> SaveTime.t request

  let build_url (type el) (request : el request) =
    match request with
    | VideoRequest guid -> Printf.sprintf "video/%s/" guid
    | VideosRequest _ -> "videos/"
    | VideoShowRequest guid -> Printf.sprintf "video_show/%s/" guid
    | VideoShowsRequest _ -> Printf.sprintf "video_shows/"
    | SaveTimeRequest _ -> "video/save-time/"

  let build_query (type el) (request : el request) =
    match request with
    | VideoRequest _ -> ""
    | VideosRequest (limit, None) -> Printf.sprintf "&limit=%d" limit
    | VideosRequest (limit, Some video_show_id) ->
        Printf.sprintf "&limit=%d&filter=video_show:%d" limit video_show_id
    | VideoShowRequest _ -> ""
    | VideoShowsRequest limit -> Printf.sprintf "&limit=%d" limit
    | SaveTimeRequest (video_id, time_to_save) ->
        Printf.sprintf "&video_id=%d&time_to_save=%d" video_id time_to_save

  let of_json (type el) (request : el request) :
      Yojson.Basic.json -> (el, string) result =
    match request with
    | VideoRequest _ -> Video.of_json
    | VideosRequest _ -> Videos.of_json
    | VideoShowRequest _ -> VideoShow.of_json
    | VideoShowsRequest _ -> VideoShows.of_json
    | SaveTimeRequest _ -> SaveTime.of_json

  let base_url = "https://www.giantbomb.com/api"

  let base_query api_key = Printf.sprintf "?api_key=%s&format=json" api_key

  let user_agent = "giantbomb-ocaml"

  let default_headers = Cohttp.Header.init_with "User-Agent" user_agent

  let format_response resp =
    resp
    >>= fun (info, body) ->
    let code = info |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
    if Cohttp.Code.is_success code then
      body |> Cohttp_lwt.Body.to_string >|= Response.return
    else Lwt.return (Response.http_error code)

  let get api_key request =
    let url = Printf.sprintf "%s/%s" base_url (build_url request) in
    let query = base_query api_key ^ build_query request in
    Uri.of_string (url ^ query)
    |> Cohttp_lwt_unix.Client.get ~headers:default_headers
    |> format_response
    >|= Response.rmap Yojson.Basic.from_string
    >|= Response.rmap (of_json request)
    >|= Response.rbind (fun resource ->
            match resource with Ok r -> Ok r | Error e -> JsonError e )
end
