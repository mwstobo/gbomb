val get_video : string -> string -> Types.video_result Lwt.t

val get_videos : ?filters:Types.filters -> string -> Types.videos_result Lwt.t
