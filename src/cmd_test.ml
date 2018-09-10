open Printf
open Ext.Filepath
open Ext
open Obuild.Types
open Obuild.Helper
open Obuild

let showTest = ref false

let args = [
    ("--output", Arg.Set showTest, " show test outputs");
]

let mainTest argv =
    let setup = App_utils.read_setup () in
    let proj_file = App_utils.project_read () in
    let _ = Configure.check proj_file false setup in
    if not (Gconf.get_target_option "build-tests") then (
        eprintf "error: building tests are disabled, re-configure with --enable-tests\n";
        exit 1
    );
    let testTargets = List.map Project.Test.to_target proj_file.Project.tests in
    if testTargets <> []
        then (
            let results =
                List.map (fun test ->
                    let testTarget = Project.Test.to_target test in
                    let outputName = Utils.to_exe_name Normal Native (Target.get_target_dest_name testTarget) in
                    let dir = Dist.get_build_exn (Dist.Target testTarget.Target.target_name) in
                    let exePath = dir </> outputName in
                    if not (Filesystem.exists exePath) then (
                        eprintf "error: %s doesn't appears built, make sure 'obuild build' is run first\n" (Target.get_target_name testTarget);
                        exit 1
                    );
                    (match Process.run [ fp_to_string exePath ] with
                    | Process.Success (out,_,_) ->
                        if !showTest then print_warnings out;
                        (test.Project.Test.name, true)
                    | Process.Failure err ->
                        print_warnings err;
                        (test.Project.Test.name, false)
                    )
                ) proj_file.Project.tests
                in
            (* this is just a mockup. expect results displayed in javascript and 3d at some point *)
            let failed = List.filter (fun (_,x) -> false = x) results in
            let successes = List.filter (fun (_,x) -> true = x) results in
            let total = List.length failed + List.length successes in
            printf "%sSUCCESS%s: %d/%d\n" (color_green()) (color_white()) (List.length successes) total;
            printf "%sFAILED%s : %d/%d\n" (color_red()) (color_white()) (List.length failed) total;
            List.iter (fun (n,_) -> printf "  %s\n" n) failed;
            if failed <> [] then exit 1

        ) else
            printf "warning: no tests defined: not doing anything.\n"

let () =
    let cmd = {
        Cmd.name = "test";
        args = args;
        fn = mainTest;
        short_desc = "Run the tests";
        long_desc = "\
XXX
";
    } in
    Cmd.register_cmd cmd
