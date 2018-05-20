open Lwt.Infix
let sprintf = Printf.sprintf

module type Fetchable = sig
  type key
  type fields
  type filters
  val resource_url : key -> string
  val resources_url : string
  val params_of_filters : filters -> (string * string) list
  val deserializer : Yojson.Safe.json -> (fields, string) Result.result
  val list_deserializer : Yojson.Safe.json -> (fields list, string) Result.result
end

module Video = struct
  type key = string

  type fields = {
    id: int;
    guid: string;
    name: string;
    url: string;
    low_url: string option;
    high_url: string option;
    hd_url: string option;
    length_seconds: int;
  } [@@deriving yojson {strict=false}]

  type fields_list = fields list [@@deriving yojson {strict=false}]

  type filters = {
    limit: int option;
    video_show: int option;
  } [@@deriving fields]

  let resource_url guid = "videos/" ^ guid
  let resources_url = "videos"

  let params_of_filters filters =
    let acc_option to_s = fun acc field ->
      match Fieldslib.Field.get field filters with
      | None -> acc
      | Some value -> (Fieldslib.Field.name field, to_s value) :: acc
    in
    Fields_of_filters.fold
      ~init: []
      ~limit: (acc_option string_of_int)
      ~video_show: (acc_option string_of_int)

  let deserializer = fields_of_yojson
  let list_deserializer = fields_list_of_yojson
end

module type Api = sig
  type 'a response
  val base : string
  val query_string_of_list : (string * string) list -> string
  val send : Uri.t -> (Cohttp_lwt.Response.t * Cohttp_lwt.Body.t) Lwt.t
  val format_response :
    (Yojson.Safe.json -> ('a, string) Result.result)
    -> (Cohttp_lwt.Response.t * Cohttp_lwt.Body.t) Lwt.t
    -> 'a response Lwt.t
end

module GiantbombApi = struct
  type 'a response = Ok of 'a | JsonError of string | HttpError of int

  let base = "https://www.giantbomb.com/api"
  let user_agent = "giantbomb-ocaml"
  let default_headers = Cohttp.Header.init_with "User-Agent" user_agent

  let send uri = Cohttp_lwt_unix.Client.get ~headers:default_headers uri

  let query_string_of_list params =
    let qjoin (k, v) = sprintf "%s=%s" k v in
    let qstring = List.map qjoin params |> String.concat "&" in
    match qstring with
    | "" -> sprintf ""
    | _ -> sprintf "?%s" qstring

  let response_of_yojson_result yjr =
    match yjr with
    | Result.Ok deserialized -> Ok deserialized
    | Result.Error s -> JsonError s

  let response_of_http_error code = HttpError code

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

module Client (A: Api)(F: Fetchable) = struct
  type 'a response = 'a A.response

  let get api_key key =
    let base_params = [("api_key", api_key); ("format", "json")] in
    let query_string = A.query_string_of_list base_params in
    sprintf "%s/%s/%s" A.base (F.resource_url key) query_string
    |> Uri.of_string
    |> A.send
    |> A.format_response F.deserializer

  let get_many filters api_key =
    let base_params = [("api_key", api_key); ("format", "json")] in
    let resource_params = F.params_of_filters filters in
    let query_string = A.query_string_of_list (base_params @ resource_params) in
    sprintf "%s/%s/%s" A.base F.resources_url query_string
    |> Uri.of_string
    |> A.send
    |> A.format_response F.list_deserializer
end

module VideoClient = Client(GiantbombApi)(Video)
