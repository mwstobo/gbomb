type 'a result = Ok of 'a | Error of string

type 'a api_response =
  { error: string
  ; limit: int
  ; offset: int
  ; number_of_page_results: int
  ; number_of_total_results: int
  ; status_code: int
  ; results: 'a }
  [@@deriving yojson {strict= false}]

type video =
  { name: string
  ; guid: string
  ; filename: string [@key "url"]
  ; low_url: string option
  ; high_url: string option
  ; hd_url: string option
  ; length_seconds: int }
  [@@deriving yojson {strict= false}]

type video_result = video result

type videos = video list [@@deriving yojson {strict= false}]

type videos_result = videos result

type filters = {limit: int}
