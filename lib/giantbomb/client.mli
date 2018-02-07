val get_video : string -> string -> Types.video option Lwt.t

val get_videos :
  ?filters:Types.filters -> string -> Types.video option list Lwt.t
