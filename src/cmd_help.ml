open Printf

let mainHelp = function
  | [] ->
    eprintf "missing command for 'obuild help'\n";
    exit 1
  | [command] ->
    let cmd = Cmd.require_cmd command in
    let usage_msg = sprintf "%s - %s\n\n%s\n\nOptions:"
                      command cmd.Cmd.short_desc cmd.Cmd.long_desc in
    print_string (Arg.usage_string (Arg.align cmd.Cmd.args) usage_msg)
  | _ :: _ ->
    eprintf "too many commands for 'obuild help', only one is supported\n";
    exit 1

let () =
  let cmd = {
    Cmd.name = "help";
    args = [];
    fn = mainHelp;
    short_desc = "Help about commands";
    long_desc = "\
XXX
";
  } in
  Cmd.register_cmd cmd
