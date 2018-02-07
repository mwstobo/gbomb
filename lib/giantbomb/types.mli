type video =
  { name: string
  ; guid: string
  ; filename: string
  ; low_url: string option
  ; high_url: string option
  ; hd_url: string option
  ; length: int }

type filters = {limit: int}
