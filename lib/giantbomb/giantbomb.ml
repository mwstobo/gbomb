module type Resource = sig
  type t

  type params

  val build_url : params -> string

  val build_query : params -> string

  val of_json : Yojson.Basic.json -> (t, string) result
end

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

  type params = string

  let build_params guid = guid

  let build_url guid = Printf.sprintf "video/%s/" guid

  let build_query _ = ""

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

  type params = int * int option

  let build_params limit video_show_id = (limit, video_show_id)

  let build_url _ = "videos/"

  let build_query = function
    | limit, None -> Printf.sprintf "&limit=%d" limit
    | limit, Some video_show_id ->
        Printf.sprintf "&limit=%d&filter=video_show:%d" limit video_show_id

  let of_json json =
    let open Yojson.Basic.Util in
    let accumulate_videos videos json = videos @ [Video.of_json_obj json] in
    let results = member "results" json |> to_list in
    try Ok (List.fold_left accumulate_videos [] results)
    with Type_error (s, _) -> Error s
end

module VideoShow = struct
  type t = {id: int; guid: string; title: string}

  type params = string

  let build_params guid = guid

  let build_url guid = Printf.sprintf "video_show/%s/" guid

  let build_query _ = ""

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

  type params = int

  let build_params limit = limit

  let build_url _ = Printf.sprintf "video_shows"

  let build_query limit = Printf.sprintf "&limit=%d" limit

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

  type params = int * int

  let build_params video_id time_to_save = (video_id, time_to_save)

  let build_url _ = "video/save-time/"

  let build_query (video_id, time_to_save) =
    Printf.sprintf "&video_id=%d&time_to_save=%d" video_id time_to_save

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

module Client (R : Resource) = struct
  open Lwt.Infix

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

  let get api_key params =
    let url = Printf.sprintf "%s/%s" base_url (R.build_url params) in
    let query = base_query api_key ^ R.build_query params in
    Uri.of_string (url ^ query)
    |> Cohttp_lwt_unix.Client.get ~headers:default_headers
    |> format_response
    >|= Response.rmap Yojson.Basic.from_string
    >|= Response.rmap R.of_json
    >|= Response.rbind (fun resource ->
            match resource with Ok r -> Ok r | Error e -> JsonError e )
end

module VideoClient = Client (Video)
module VideosClient = Client (Videos)
module VideoShowClient = Client (VideoShow)
module VideoShowsClient = Client (VideoShows)
module SaveTimeClient = Client (SaveTime)
