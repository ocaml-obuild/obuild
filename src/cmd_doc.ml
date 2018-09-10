open Printf

let mainDoc argv =
    let proj_file = App_utils.project_read () in
    Doc.run proj_file;
    App_utils.unimplemented ()

let () =
    let cmd = {
        Cmd.name = "doc";
        args = [];
        fn = mainDoc;
        short_desc = "Generate documentation";
        long_desc = "\
XXX
";
    } in
    Cmd.register_cmd cmd
