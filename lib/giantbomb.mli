module Types : sig
  type video =
    { name: string
    ; filename: string
    ; low_url: string
    ; high_url: string
    ; hd_url: string }
end

module Client : sig
  val get_video : string -> string -> Types.video option Lwt.t
end
