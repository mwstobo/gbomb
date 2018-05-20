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
  }

  type filters = {
    limit: int option;
    video_show: int option;
  }
end

module VideoApi : sig
  type 'a response = Ok of 'a | JsonError of string | HttpError of int

  val get : string -> Video.key -> Video.fields response Lwt.t

  val get_many : Video.filters -> string -> Video.fields list response Lwt.t
end
