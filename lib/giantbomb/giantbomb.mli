module Video : sig
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

  type params

  val build_params : string -> params
end

module Videos : sig
  type t = Video.t list

  type params

  val build_params : int -> int option -> params
end

module VideoShow : sig
  type t = {id: int; guid: string; title: string}

  type params

  val build_params : string -> params
end

module VideoShows : sig
  type t = VideoShow.t list

  type params

  val build_params : int -> params
end

module SaveTime : sig
  type t = unit

  type params

  val build_params : int -> int -> params
end

module Response : sig
  type 'a t = Ok of 'a | JsonError of string | HttpError of int
end

module VideoClient : sig
  val get : string -> Video.params -> Video.t Response.t Lwt.t
end

module VideosClient : sig
  val get : string -> Videos.params -> Videos.t Response.t Lwt.t
end

module VideoShowClient : sig
  val get : string -> VideoShow.params -> VideoShow.t Response.t Lwt.t
end

module VideoShowsClient : sig
  val get : string -> VideoShows.params -> VideoShows.t Response.t Lwt.t
end

module SaveTimeClient : sig
  val get : string -> SaveTime.params -> SaveTime.t Response.t Lwt.t
end
