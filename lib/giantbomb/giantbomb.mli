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
end

module Videos : sig
  type t = Video.t list
end

module VideoShow : sig
  type t = {id: int; guid: string; title: string}
end

module VideoShows : sig
  type t = VideoShow.t list
end

module SaveTime : sig
  type t = unit
end

module Response : sig
  type 'a t = Ok of 'a | JsonError of string | HttpError of int
end

module Client : sig
  type 'a request =
    | VideoRequest : string -> Video.t request
    | VideosRequest : int * int option -> Videos.t request
    | VideoShowRequest : string -> VideoShow.t request
    | VideoShowsRequest : int -> VideoShows.t request
    | SaveTimeRequest : int * int -> SaveTime.t request

  val get : string -> 'a request -> 'a Response.t Lwt.t
end
