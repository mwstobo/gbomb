module Types : sig
  type video =
    { name: string
    ; guid: string
    ; filename: string
    ; low_url: string
    ; high_url: string
    ; hd_url: string
    ; length: int }

  type filters = {limit: int}
end

module Client : sig
  val get_video : string -> string -> Types.video option Lwt.t

  val get_videos :
    ?filters:Types.filters -> string -> Types.video option list Lwt.t
end
