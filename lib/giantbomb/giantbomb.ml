open Lwt.Infix
let sprintf = Printf.sprintf

module type Fetchable = sig
  type key
  type fields
  type filters
  val resource_url : key -> string
  val resources_url : string
  val string_of_filters : filters -> string
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
    video_show: int option;
  } [@@deriving fields]

  let resource_url guid = "videos/" ^ guid
  let resources_url = "videos"

  let string_of_filters filters =
    let acc_option to_s = fun acc field ->
      match Fieldslib.Field.get field filters with
      | None -> acc
      | Some value ->
          (sprintf "%s:%s" (Fieldslib.Field.name field) (to_s value)) :: acc
    in
    String.concat
      ","
      (Fields_of_filters.fold
        ~init: []
        ~video_show: (acc_option string_of_int))

  let deserializer = fields_of_yojson
  let list_deserializer = fields_list_of_yojson
end

module VideoShow = struct
  type key = string

  type fields = {
    id: int;
    guid: string;
    title: string;
  } [@@deriving yojson {strict=false}]

  type fields_list = fields list [@@deriving yojson {strict=false}]

  type filters = None

  let resource_url guid = "video_shows/" ^ guid
  let resources_url = "video_shows"

  let string_of_filters filters = ""

  let deserializer = fields_of_yojson
  let list_deserializer = fields_list_of_yojson
end

module type Queryable = sig
  type 'a response
  type filters
  val base : string
  val params_of_filters : filters -> (string * string) list
  val query_string_of_list : (string * string) list -> string
  val send : Uri.t -> (Cohttp_lwt.Response.t * Cohttp_lwt.Body.t) Lwt.t
  val format_response :
    (Yojson.Safe.json -> ('a, string) Result.result)
    -> (Cohttp_lwt.Response.t * Cohttp_lwt.Body.t) Lwt.t
    -> 'a response Lwt.t
end

module Api = struct
  type 'a response = Ok of 'a | JsonError of string | HttpError of int

  let base = "https://www.giantbomb.com/api"
  let user_agent = "giantbomb-ocaml"
  let default_headers = Cohttp.Header.init_with "User-Agent" user_agent

  type filters = {
    limit: int;
  } [@@deriving fields]

  let params_of_filters filters =
    let acc to_s = fun acc field ->
      let field_name = Fieldslib.Field.name field in
      let field_value = Fieldslib.Field.get field filters in
      (field_name, (to_s field_value)) :: acc
    in
    Fields_of_filters.fold
      ~init: []
      ~limit: (acc string_of_int)

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

module Client (Q: Queryable)(F: Fetchable) = struct
  let get api_key key =
    let base_params = [("api_key", api_key); ("format", "json")] in
    let query_string = Q.query_string_of_list base_params in
    sprintf "%s/%s/%s" Q.base (F.resource_url key) query_string
    |> Uri.of_string
    |> Q.send
    |> Q.format_response F.deserializer

  let get_many query_filters fetch_filters api_key =
    let base_params = [("api_key", api_key); ("format", "json")] in
    let query_params = Q.params_of_filters query_filters in
    let fetch_param = ("filter", F.string_of_filters fetch_filters) in
    let query_string =
      Q.query_string_of_list (fetch_param :: base_params @ query_params)
    in
    sprintf "%s/%s/%s" Q.base F.resources_url query_string
    |> Uri.of_string
    |> Q.send
    |> Q.format_response F.list_deserializer
end

module VideoClient = Client(Api)(Video)
module VideoShowClient = Client(Api)(VideoShow)
