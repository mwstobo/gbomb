(** Represents different kinds of command line options.
    Arg   - option that doesn't start with a -  (eg. git _status_)
    Flag  - option that has no associated value (eg. git _--help_)
    Param - option that has an associated value (eg. git log _-n 5_)
*)
type cli_options = Arg of string | Flag of string | Param of string * string

(** Represents different possible strings that could be passed as options.
    Dash - strings that start with a -
    Just - strings that do not start with a -
 *)
type option_strings = Dash of string | Just of string

exception InvalidOpts of string

(*$T get_string_type
  get_string_type "-t"    = Dash("t")
  get_string_type "--foo" = Dash("foo")
  get_string_type "foo"   = Just("foo")
  get_string_type "1"     = Just("1")

  try ignore (get_string_type "");   false with (InvalidOpts _) -> true
  try ignore (get_string_type "-");  false with (InvalidOpts _) -> true
  try ignore (get_string_type "--"); false with (InvalidOpts _) -> true
*)
let get_string_type opt =
  if opt = "" || opt = "-" || opt = "--" then
    raise (InvalidOpts ("invalid opt" ^ opt))
  else if String.length opt >= 2 && opt.[0] = '-' then
    let start = if opt.[1] = '-' then 2 else 1 in
    Dash (String.sub opt start (String.length opt - start))
  else Just opt


(*$= parse_options
  ([ Flag("t") ])                   (parse_options [ "-t" ])
  ([ Flag("foo") ])                 (parse_options [ "--foo" ])
  ([ Param("d", "1") ])             (parse_options [ "-d"; "1" ])
  ([ Param("bar", "1") ])           (parse_options [ "--bar" ; "1" ])
  ([ Flag("f"); Flag("b") ])        (parse_options [ "-f" ; "-b" ])
  ([ Arg("foo") ])                  (parse_options [ "foo" ])
  ([ Param("d", "1"); Arg("foo") ]) (parse_options [ "-d"; "1"; "foo" ])
*)
(*$T parse_options
  try ignore (parse_options [ "" ]);         false with (InvalidOpts _) -> true
  try ignore (parse_options [ "-" ]);        false with (InvalidOpts _) -> true
  try ignore (parse_options [ "-"; "foo" ]); false with (InvalidOpts _) -> true
  try ignore (parse_options [ "-d"; "-" ]);  false with (InvalidOpts _) -> true
*)
let rec parse_options options =
  match options with
  | [] -> []
  | [opt] -> (
    match get_string_type opt with
    | Just arg -> [Arg arg]
    | Dash flag -> [Flag flag] )
  | opt :: second :: options ->
    match get_string_type opt with
    | Just arg -> Arg arg :: parse_options (second :: options)
    | Dash dash ->
      match get_string_type second with
      | Just param_val -> Param (dash, param_val) :: parse_options options
      | _ -> Flag dash :: parse_options (second :: options)
