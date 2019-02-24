module Api : sig
  type 'a response = Ok of 'a | JsonError of string | HttpError of int

  type filters = {
    limit: int;
  }
end

module Video : sig
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
    saved_time: string option;
  }

  type filters = {
    video_show: int option;
  }
end

module VideoShow : sig
  type key = string

  type fields = {
    id: int;
    guid: string;
    title: string;
  }

  type filters = None
end

module VideoClient : sig
  val get : string -> Video.key -> Video.fields Api.response Lwt.t

  val get_many : Api.filters -> Video.filters -> string -> Video.fields list Api.response Lwt.t
end

module VideoShowClient : sig
  val get : string -> VideoShow.key -> VideoShow.fields Api.response Lwt.t

  val get_many : Api.filters -> VideoShow.filters -> string -> VideoShow.fields list Api.response Lwt.t
end
