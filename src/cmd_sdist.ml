open Obuild

let isSnapshot = ref false

let args = [
    ("--snapshot", Arg.Set isSnapshot, " build a snapshot of the project");
]

let mainSdist argv =
    Dist.check_exn (fun () -> ());

    let proj_file = App_utils.project_read () in
    Sdist.run proj_file !isSnapshot;
    ()

let () =
    let cmd = {
        Cmd.name = "sdist";
        args = args;
        fn = mainSdist;
        short_desc = "Create a source distribution file (.tar.gz)";
        long_desc = "\
Generate a source distribution file .tar.gz that contains
all the necessary bits to distribute to someone else
and being able to build and install the package.";
    } in
    Cmd.register_cmd cmd
