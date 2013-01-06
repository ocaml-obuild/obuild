open Ext
open Filepath

let read_file_with f filename = 
    let lines = ref [] in
    let chan = open_in filename in
    try
        while true; do
            let z = f (input_line chan) in
            match z with
            | None    -> ()
            | Some z' -> lines := z' :: !lines
        done; []
    with End_of_file ->
        close_in chan;
        List.rev !lines

let toKV line =
    match string_split ~limit:2 ':' line with
    | [k]   -> (string_stripSpaces k, None)
    | [k;v] -> (string_stripSpaces k, Some (string_stripSpaces v))
    | _     -> assert false

let toKVeq line =
    match string_split ~limit:2 '=' line with
    | [k]   -> (string_stripSpaces k, None)
    | [k;v] -> (string_stripSpaces k, Some (string_stripSpaces v))
    | _     -> assert false

let parseCSV value = List.map string_stripSpaces (string_split ',' value)

let to_include_path_options paths =
    List.concat $ List.map (fun p -> [ "-I"; fp_to_string p ]) paths

let showList sep f l = String.concat sep (List.map f l)

(* hacky way to detect windows *)
let is_windows = Filename.dir_sep <> "/"

let to_exe_name name =
    fn (if is_windows then name ^ ".exe" else name)
