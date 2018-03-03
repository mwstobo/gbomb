module Api : sig
  type key = string

  type 'a response = Ok of 'a | Error of string
end

module Resources : sig
  type video =
    { guid: string
    ; name: string
    ; url: string
    ; low_url: string option
    ; high_url: string option
    ; hd_url: string option
    ; length_seconds: int }

  type videos = video list
end

val video_get : Api.key -> string -> Resources.video Api.response Lwt.t

val videos_get : ?limit : int -> Api.key -> Resources.videos Api.response Lwt.t
