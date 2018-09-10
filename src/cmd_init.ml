open Ext.Filepath
open Obuild

let mainInit _ =
    let project = Init.run () in
    let name = fn (project.Project.name) <.> "obuild" in
    Project.write (in_current_dir name) project

let () =
    let cmd = {
        Cmd.name = "init";
        args = [];
        fn = mainInit;
        short_desc = "XXX";
        long_desc = "\
XXX
";
    } in
    Cmd.register_cmd cmd
