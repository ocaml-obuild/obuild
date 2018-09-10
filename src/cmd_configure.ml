open Ext.Fugue
open Obuild.Helper
open Obuild.Gconf
open Obuild

let user_flags = ref []
let user_opts = ref []

let configure argv =
  FindlibConf.load ();
  let proj_file = Project.read gconf.strict in
  verbose Report "Configuring %s-%s...\n" proj_file.Project.name proj_file.Project.version;
  Configure.run proj_file !user_flags !user_opts;
  (* check build deps of everything buildables *)
  ()

let () =
  let user_set_flags s =
    let tweak = if string_startswith "-" s then Configure.ClearFlag (string_drop 1 s) else Configure.SetFlag s
    in
    user_flags := tweak :: !user_flags
  in
  let set_target_options field value () =
    let opt_name = if (List.mem field ["examples"; "benchs"; "tests"]) then ("build-" ^ field) else field in
    user_opts := (opt_name,value) :: !user_opts
  in
  let enable_disable_opt opt_name doc = [
    ("--enable-" ^ opt_name, Arg.Unit (set_target_options opt_name true), " enable " ^ doc);
    ("--disable-" ^ opt_name, Arg.Unit (set_target_options opt_name false), " disable " ^ doc)
  ] in
  let opts = [
    ("--flag", Arg.String user_set_flags, "FLAG enable or disable a project's flag");
    ("--executable-as-obj", Arg.Unit (set_target_options "executable-as-obj" true), " output executable as obj file");
    ("--annot", Arg.Unit (set_target_options "annot" true), " generate .annot files");
    ("-g", Arg.Unit (fun () ->
        (set_target_options "library-debugging" true)();
        (set_target_options "executable-debugging" true)();
    ), " compilation with debugging");
    ("-pg", Arg.Unit (fun () ->
        (set_target_options "library-profiling" true)();
        (set_target_options "executable-profiling" true)();
    ), " compilation with profiling")
  ] in
  let args =
    enable_disable_opt "library-bytecode" "library compilation as bytecode"
    @ enable_disable_opt "library-native" "library compilation as native"
    @ enable_disable_opt "library-plugin" "library compilation as native plugin"
    @ enable_disable_opt "executable-bytecode" "executable compilation as bytecode"
    @ enable_disable_opt "executable-native" "executable compilation as native"
    @ enable_disable_opt "library-profiling" "library profiling"
    @ enable_disable_opt "library-debugging" "library debugging"
    @ enable_disable_opt "executable-profiling" "executable profiling"
    @ enable_disable_opt "executable-debugging" "executable debugging"
    @ enable_disable_opt "examples" "building examples"
    @ enable_disable_opt "benchs" "building benchs"
    @ enable_disable_opt "tests" "building tests"
    @ opts in

  let cmd = {
    Cmd.name = "configure";
    args = args;
    fn = configure;
    short_desc = "Prepare to build the package";
    long_desc = "\
Configure verify that the environment is able to compile the project
and this is where the user can tell obuild options to build

System settings and user settings are cached, to provide faster
access for building task.";
  } in
  Cmd.register_cmd cmd
