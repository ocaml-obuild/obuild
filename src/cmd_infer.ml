open Printf

let mainInfer argv =
    if argv = []
    then (printf "no modules to infer\n"; exit 0);

    App_utils.unimplemented ()

let () =
    let cmd = {
        Cmd.name = "infer";
        args = [];
        fn = mainInfer;
        short_desc = "XXX";
        long_desc = "\
XXX
";
    } in
    Cmd.register_cmd cmd
