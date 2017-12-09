type cli_options = Arg of string | Flag of string * string

let rec parse_options options =
  match options with
  | [] -> []
  | [opt] -> [Arg opt]
  | opt :: second :: options ->
      let first_char = opt.[0] in
      let second_char = opt.[1] in
      if first_char == '-' then
        let flag =
          if second_char == '-' then
            Flag (String.sub opt 2 (String.length opt - 2), second)
          else Flag (String.sub opt 1 (String.length opt - 1), second)
        in
        flag :: parse_options options
      else Arg opt :: parse_options (second :: options)
