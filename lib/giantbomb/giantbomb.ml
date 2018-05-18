(*${*)
open Lwt.Infix
let sprintf = Printf.sprintf
(*$}*)

module Api = struct
  module Default = struct
    let limit = 10
  end

  type key = string

  type 'a response = Ok of 'a | JsonError of string | HttpError of int

  let base = "https://www.giantbomb.com/api"

  let user_agent = "gbomb-ocaml"

  let default_headers = Cohttp.Header.init_with "User-Agent" user_agent

  let send uri = Cohttp_lwt_unix.Client.get ~headers:default_headers uri

  let response_of_yojson_result yjr =
    match yjr with
    | Result.Ok deserialized -> Ok deserialized
    | Result.Error s -> JsonError s

  let response_of_http_error code =
    HttpError code

  let format_response deserializer resp =
    resp >>= fun (info, body) ->
    let code = info |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
    if Cohttp.Code.is_success code then
      body |> Cohttp_lwt.Body.to_string
      >|= Yojson.Safe.from_string
      >|= Yojson.Safe.Util.member "results"
      >|= deserializer
      >|= response_of_yojson_result
    else Lwt.return (response_of_http_error code)
end

module Path = struct
  (*${*)
  (*$= with_resource
    ("a.test/videos")  (with_resource "videos" None "a.test")
    ("a.test/video/1") (with_resource "video" (Some "1") "a.test")
  *)
  let with_resource resource_name resource_id path =
    match resource_id with
    | Some id -> sprintf "%s/%s/%s" path resource_name id
    | None -> sprintf "%s/%s" path resource_name

  (*$= with_query
    ("a.test/b")          (with_query [] "a.test/b")
    ("a.test/b/?c=d")     (with_query [("c", "d")] "a.test/b")
    ("a.test/b/?c=d&e=f") (with_query [("c", "d"); ("e", "f")] "a.test/b")
  *)
  let with_query query_params path =
    let qjoin (k, v) = sprintf "%s=%s" k v in
    let qstring = List.map qjoin query_params |> String.concat "&" in
    match qstring with
    | "" -> sprintf "%s" path
    | _ -> sprintf "%s/?%s" path qstring
  (*$}*)
end

module Resources = struct
  type video =
    { guid: string
    ; name: string
    ; url: string
    ; low_url: string option
    ; high_url: string option
    ; hd_url: string option
    ; length_seconds: int }
  [@@deriving yojson {strict=false}]

  type videos = video list [@@deriving yojson {strict=false}]
end

let video_get api_key video_id =
  Api.base
  |> Path.with_resource "video" (Some video_id)
  |> Path.with_query [("api_key", api_key); ("format", "json")]
  |> Uri.of_string
  |> Api.send
  |> Api.format_response Resources.video_of_yojson

let videos_get ?(limit=Api.Default.limit) api_key =
  Api.base
  |> Path.with_resource "videos" None
  |> Path.with_query
    [ ("api_key", api_key)
    ; ("format", "json")
    ; ("limit", string_of_int limit) ]
  |> Uri.of_string
  |> Api.send
  |> Api.format_response Resources.videos_of_yojson
