(** Lexer for .obuild files

    Tokenizes the line-based, indentation-sensitive format.
*)

(** Token location *)
type loc = {
  line: int;
  col: int;
}

(** Token types *)
type token =
  | KEY_VALUE of string * string    (* key: value or key = value *)
  | BLOCK of string * string list   (* blockname arg1 arg2 ... *)
  | BLANK                           (* empty or comment line *)
  | EOF

(** A token with its location and indentation level *)
type located_token = {
  tok: token;
  loc: loc;
  indent: int;
}

(** Lexer error *)
exception Lexer_error of loc * string

(** Check if a character is whitespace (space or tab) *)
let is_whitespace c = c = ' ' || c = '\t'

(** Count leading whitespace and return (indent_level, rest_of_string) *)
let count_indent s =
  let len = String.length s in
  let rec loop i =
    if i >= len then (i, "")
    else if is_whitespace s.[i] then loop (i + 1)
    else (i, String.sub s i (len - i))
  in
  loop 0

(** Strip trailing whitespace *)
let strip_trailing s =
  let len = String.length s in
  let rec loop i =
    if i <= 0 then ""
    else if is_whitespace s.[i - 1] then loop (i - 1)
    else String.sub s 0 i
  in
  loop len

(** Strip leading and trailing whitespace *)
let strip s =
  let _, rest = count_indent s in
  strip_trailing rest

(** Check if line is blank or a comment *)
let is_blank_or_comment s =
  let s = strip s in
  s = "" || (String.length s > 0 && s.[0] = '#')

(** Find separator (: or =) and split into key/value *)
let find_key_value s =
  let len = String.length s in
  let rec loop i =
    if i >= len then None
    else match s.[i] with
      | ':' | '=' ->
        let key = strip (String.sub s 0 i) in
        let value = strip (String.sub s (i + 1) (len - i - 1)) in
        Some (key, value)
      | _ -> loop (i + 1)
  in
  loop 0

(** Split string into words *)
let split_words s =
  let len = String.length s in
  let rec skip_ws i =
    if i >= len then i
    else if is_whitespace s.[i] then skip_ws (i + 1)
    else i
  in
  let rec read_word i acc =
    if i >= len then (i, acc)
    else if is_whitespace s.[i] then (i, acc)
    else read_word (i + 1) (acc ^ String.make 1 s.[i])
  in
  let rec loop i words =
    let i = skip_ws i in
    if i >= len then List.rev words
    else
      let (i', word) = read_word i "" in
      loop i' (word :: words)
  in
  loop 0 []

(** Tokenize a single line *)
let tokenize_line line_num line =
  let (indent, content) = count_indent line in
  let loc = { line = line_num; col = indent + 1 } in
  if is_blank_or_comment line then
    { tok = BLANK; loc; indent }
  else
    match find_key_value content with
    | Some (key, value) ->
      { tok = KEY_VALUE (key, value); loc; indent }
    | None ->
      (* No separator - must be a block header *)
      let words = split_words content in
      match words with
      | [] -> { tok = BLANK; loc; indent }
      | keyword :: args ->
        { tok = BLOCK (keyword, args); loc; indent }

(** Tokenize entire input string *)
let tokenize input =
  let lines = String.split_on_char '\n' input in
  let rec loop line_num acc = function
    | [] ->
      let loc = { line = line_num; col = 1 } in
      List.rev ({ tok = EOF; loc; indent = 0 } :: acc)
    | line :: rest ->
      let token = tokenize_line line_num line in
      (* Skip blank lines in token stream *)
      let acc' = if token.tok = BLANK then acc else token :: acc in
      loop (line_num + 1) acc' rest
  in
  loop 1 [] lines

(** Tokenize from a file *)
let tokenize_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  tokenize s

(** Pretty-print a token for debugging *)
let token_to_string = function
  | KEY_VALUE (k, v) -> Printf.sprintf "KEY_VALUE(%s, %s)" k v
  | BLOCK (name, args) -> Printf.sprintf "BLOCK(%s, [%s])" name (String.concat "; " args)
  | BLANK -> "BLANK"
  | EOF -> "EOF"

let located_token_to_string t =
  Printf.sprintf "%d:%d indent=%d %s"
    t.loc.line t.loc.col t.indent (token_to_string t.tok)
