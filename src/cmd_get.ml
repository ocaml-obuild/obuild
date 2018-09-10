open Printf
open Obuild

let mainGet = function
  | [] ->
    eprintf "missing field for 'obuild get'\n";
    exit 1
  | [field] ->
    let proj_file = App_utils.project_read () in

    (* TODO: hardcoded just for now to get basic fields.
     * - add option for quoting
     * - optional formating options for multi values (one per line, csv)
     * - access more complicated fields lib/sublib modules/dependencies, etc
     * *)
    let value =
      match field with
      | "name" -> proj_file.Project.name;
      | "version" -> proj_file.Project.version;
      | "license" -> proj_file.Project.license;
      | f -> eprintf "error: unknown field %s\n" f; exit 1 in
    printf "%s\n" value
  | _ :: _ ->
    eprintf "too many fields for 'obuild get', only one is supported\n";
    exit 1

let () =
  let cmd = {
    Cmd.name = "get";
    args = [];
    fn = mainGet;
    short_desc = "XXX";
    long_desc = "\
XXX
";
  } in
  Cmd.register_cmd cmd
