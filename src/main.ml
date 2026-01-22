open Printf
open Fugue
open Filepath
open Types
open Helper
open Gconf
open Lib
open Cli

(* ===== Helper Functions ===== *)

let read_setup () =
  FindlibConf.load ();
  let setup = Dist.read_setup () in
  Configure.set_opts setup;
  setup

let project_read () =
  try Project_read.read ()
  with exn ->
    verbose Verbose "exception during project read: %s\n" (Printexc.to_string exn);
    raise exn

(* ===== Configure Command ===== *)

let cmd_configure =
  let cmd =
    Cli.command "configure" ~doc:"Prepare to build the package"
      ~description:
        "Configures the build system and detects dependencies. Sets up compilation flags and \
         options."
      ~run:(fun ctx ->
        (* Collect user flags and options from context *)
        let user_flags = ref [] in
        let user_opts = ref [] in

        (* Process flag options *)
        let flags_str = Cli.get_strings ctx "flag" in
        List.iter
          (fun s ->
            let tweak =
              if string_startswith "-" s then
                Configure.ClearFlag (string_drop 1 s)
              else
                Configure.SetFlag s
            in
            user_flags := tweak :: !user_flags)
          flags_str;

        (* Process all boolean options *)
        let add_opt name value = user_opts := (name, value) :: !user_opts in
        let add_bool_opt cli_name opt_name =
          match Cli.get_bool_opt ctx cli_name with
          | Some value -> add_opt opt_name value
          | None -> ()
        in

        add_bool_opt "library-bytecode" "library-bytecode";
        add_bool_opt "library-native" "library-native";
        add_bool_opt "library-plugin" "library-plugin";
        add_bool_opt "executable-bytecode" "executable-bytecode";
        add_bool_opt "executable-native" "executable-native";
        add_bool_opt "library-profiling" "library-profiling";
        add_bool_opt "library-debugging" "library-debugging";
        add_bool_opt "executable-profiling" "executable-profiling";
        add_bool_opt "executable-debugging" "executable-debugging";
        add_bool_opt "examples" "build-examples";
        add_bool_opt "benchs" "build-benchs";
        add_bool_opt "tests" "build-tests";

        (* Handle shorthand flags *)
        if Cli.get_flag ctx "executable-as-obj" then add_opt "executable-as-obj" true;
        if Cli.get_flag ctx "annot" then add_opt "annot" true;
        if Cli.get_flag ctx "g" then (
          add_opt "library-debugging" true;
          add_opt "executable-debugging" true);
        if Cli.get_flag ctx "pg" then (
          add_opt "library-profiling" true;
          add_opt "executable-profiling" true);

        (* Run configuration *)
        FindlibConf.load ();
        let proj_file = Project_read.read () in
        verbose Report "Configuring %s-%s...\n" proj_file.Project.name proj_file.Project.version;
        Configure.run proj_file !user_flags !user_opts)
      ()
  in
  cmd |> Cli.help_flag
  |> Cli.option_strings "flag" ~doc:"Enable or disable a project flag (can be repeated)"
  |> Cli.option_bool "library-bytecode" ~doc:"Compile libraries as bytecode"
  |> Cli.option_bool "library-native" ~doc:"Compile libraries as native"
  |> Cli.option_bool "library-plugin" ~doc:"Compile libraries as native plugin"
  |> Cli.option_bool "executable-bytecode" ~doc:"Compile executables as bytecode"
  |> Cli.option_bool "executable-native" ~doc:"Compile executables as native"
  |> Cli.option_bool "library-profiling" ~doc:"Enable library profiling"
  |> Cli.option_bool "library-debugging" ~doc:"Enable library debugging"
  |> Cli.option_bool "executable-profiling" ~doc:"Enable executable profiling"
  |> Cli.option_bool "executable-debugging" ~doc:"Enable executable debugging"
  |> Cli.option_bool "examples" ~doc:"Build examples"
  |> Cli.option_bool "benchs" ~doc:"Build benchmarks"
  |> Cli.option_bool "tests" ~doc:"Build tests"
  |> Cli.flag "executable-as-obj" ~doc:"Output executable as obj file"
  |> Cli.flag "annot" ~doc:"Generate .annot files"
  |> Cli.flag "g" ~short:'g' ~doc:"Compilation with debugging"
  |> Cli.flag "pg" ~doc:"Compilation with profiling"

(* ===== Build Command ===== *)

let cmd_build =
  let cmd =
    Cli.command "build" ~doc:"Make this package ready for installation"
      ~description:"Compiles all or specified targets in the project using parallel compilation."
      ~run:(fun ctx ->
        (* Set parallel jobs *)
        gconf.parallel_jobs <- Cli.get_int ctx "jobs" ~default:gconf.parallel_jobs;

        (* Set other options *)
        if Cli.get_flag ctx "dot" then gconf.dump_dot <- true;
        if Cli.get_flag ctx "noocamlmklib" then gconf.ocamlmklib <- false;

        (* Get target names *)
        let targets = Cli.get_positionals ctx in

        (* Build *)
        Dist.exist ();
        let setup = read_setup () in
        let proj_file = project_read () in
        let flags = Configure.check proj_file true setup in
        let project = Analyze.prepare proj_file flags in
        let bstate = Prepare.init project in

        let dag =
          match targets with
          | [] -> project.Analyze.project_targets_dag
          | _ ->
              let target_names = List.map Target.Name.of_string targets in
              Dag.subset project.Analyze.project_targets_dag target_names
        in
        Build.build_dag bstate proj_file dag)
      ()
  in
  cmd |> Cli.help_flag
  |> Cli.option_int "jobs" ~short:'j' ~placeholder:"N" ~default:gconf.parallel_jobs
       ~doc:"Maximum number of jobs in parallel"
  |> Cli.flag "dot" ~doc:"Dump dependencies dot files during build"
  |> Cli.flag "noocamlmklib" ~doc:"Do not use ocamlmklib when linking C code"
  |> Cli.positionals "targets" ~placeholder:"TARGET..." ~doc:"Optional list of targets to build"

(* ===== Clean Command ===== *)

let cmd_clean =
  let cmd =
    Cli.command "clean" ~doc:"Clean up after a build"
      ~description:"Removes all build artifacts and the dist directory."
      ~run:(fun _ctx ->
        if Filesystem.exists (Dist.get_path ()) then (
          Filesystem.remove_dir (Dist.get_path ());
          Dist.remove_dead_links ()))
      ()
  in
  cmd |> Cli.help_flag

(* ===== Sdist Command ===== *)

let cmd_sdist =
  let cmd =
    Cli.command "sdist" ~doc:"Generate a source distribution file (.tar.gz)"
      ~description:"Creates a tarball of the source code for distribution."
      ~run:(fun ctx ->
        let is_snapshot = Cli.get_flag ctx "snapshot" in
        Dist.check_exn (fun () -> ());
        let proj_file = project_read () in
        Sdist.run proj_file is_snapshot)
      ()
  in
  cmd |> Cli.help_flag |> Cli.flag "snapshot" ~doc:"Build a snapshot of the project"

(* ===== Install Command ===== *)

let cmd_install =
  let cmd =
    Cli.command "install" ~doc:"Install this package"
      ~description:"Installs compiled libraries and executables to the system."
      ~run:(fun ctx ->
        let dest_dir_str = Cli.get_string_opt ctx "destdir" in
        let opam_install = Cli.get_flag ctx "opam" in

        Dist.exist ();
        let setup = read_setup () in
        let proj_file = project_read () in
        let flags = Configure.check proj_file false setup in

        let dest_dir =
          match dest_dir_str with
          | Some d -> fp d
          | None -> (
              match FindlibConf.get_destdir () with
              | None -> failwith "no destdir specified, and no findlib default found"
              | Some p -> p)
        in

        Install.install_libs proj_file dest_dir opam_install;
        if opam_install then Install.opam_install_file proj_file flags)
      ()
  in
  cmd |> Cli.help_flag
  |> Cli.option_string "destdir" ~placeholder:"DIR" ~doc:"Override destination where to install"
  |> Cli.flag "opam" ~doc:"Only create the .install file for opam"

(* ===== Test Command ===== *)

let cmd_test =
  let cmd =
    Cli.command "test" ~doc:"Run the tests"
      ~description:"Executes all test targets and reports results."
      ~run:(fun ctx ->
        let show_test = Cli.get_flag ctx "output" in

        let setup = read_setup () in
        let proj_file = project_read () in
        let _ = Configure.check proj_file false setup in

        if not (Gconf.get_target_option "build-tests") then (
          eprintf "error: building tests are disabled, re-configure with --enable-tests\n";
          exit 1);

        let test_targets = List.map Project.Test.to_target proj_file.Project.tests in
        if test_targets <> [] then (
          let results =
            List.map
              (fun test ->
                let test_target = Project.Test.to_target test in
                let output_name =
                  Utils.to_exe_name Normal Native (Target.get_target_dest_name test_target)
                in
                let dir = Dist.get_build_exn (Dist.Target test_target.Target.target_name) in
                let exe_path = dir </> output_name in

                if not (Filesystem.exists exe_path) then (
                  eprintf "error: %s doesn't appear built, make sure 'obuild build' is run first\n"
                    (Target.get_target_name test_target);
                  exit 1);

                match Process.run [ fp_to_string exe_path ] with
                | Process.Success (out, _, _) ->
                    if show_test then print_warnings out;
                    (test.Project.Test.name, true)
                | Process.Failure err ->
                    print_warnings err;
                    (test.Project.Test.name, false))
              proj_file.Project.tests
          in

          let failed = List.filter (fun (_, x) -> not x) results in
          let successes = List.filter (fun (_, x) -> x) results in
          let total = List.length failed + List.length successes in

          printf "%sSUCCESS%s: %d/%d\n" (color_green ()) (color_white ()) (List.length successes)
            total;
          printf "%sFAILED%s : %d/%d\n" (color_red ()) (color_white ()) (List.length failed) total;
          List.iter (fun (n, _) -> printf "  %s\n" n) failed;

          if failed <> [] then exit 1)
        else
          printf "warning: no tests defined: not doing anything.\n")
      ()
  in
  cmd |> Cli.help_flag |> Cli.flag "output" ~doc:"Show test outputs"

(* ===== Init Command ===== *)

let cmd_init =
  let cmd =
    Cli.command "init" ~doc:"Initialize a new project"
      ~description:"Creates a new obuild project file interactively."
      ~run:(fun _ctx ->
        let project = Init.run () in
        let name = fn project.Project.name <.> "obuild" in
        Project.write (in_current_dir name) project)
      ()
  in
  cmd |> Cli.help_flag

(* ===== Doc Command ===== *)

let cmd_doc =
  let cmd =
    Cli.command "doc" ~doc:"Generate documentation"
      ~description:"Generates OCamldoc documentation for the project."
      ~run:(fun _ctx ->
        let proj_file = project_read () in
        Doc.run proj_file;
        eprintf "sorry, you've reached an unimplemented part! please be patient or send a patch.\n";
        exit 1)
      ()
  in
  cmd |> Cli.help_flag

(* ===== Get Command ===== *)

let cmd_get =
  let cmd =
    Cli.command "get" ~doc:"Get project metadata field"
      ~description:"Retrieve specific fields from the project file (name, version, license)."
      ~run:(fun ctx ->
        let proj_file = project_read () in
        let positionals = Cli.get_positionals ctx in

        match positionals with
        | [] ->
            eprintf "usage: obuild get <field>\n\n";
            exit 1
        | [ field ] -> (
            match field with
            | "name" -> printf "%s\n" proj_file.Project.name
            | "version" -> printf "%s\n" proj_file.Project.version
            | "license" -> printf "%s\n" proj_file.Project.license
            | _ ->
                eprintf "error: unknown field %s\n" field;
                exit 1)
        | _ ->
            eprintf "usage: obuild get <field>\n";
            exit 1)
      ()
  in
  cmd |> Cli.help_flag
  |> Cli.positionals "field" ~placeholder:"FIELD" ~doc:"Field to retrieve (name, version, license)"

(* ===== Infer Command ===== *)

let cmd_infer =
  let cmd =
    Cli.command "infer" ~doc:"Infer module dependencies (unimplemented)"
      ~description:"Analyzes source files to infer module dependencies."
      ~run:(fun ctx ->
        let modules = Cli.get_positionals ctx in
        if modules = [] then (
          printf "no modules to infer\n";
          exit 0);
        eprintf "sorry, you've reached an unimplemented part! please be patient or send a patch.\n";
        exit 1)
      ()
  in
  cmd |> Cli.help_flag |> Cli.positionals "modules" ~placeholder:"MODULE..." ~doc:"Modules to infer"

(* ===== Completion Command ===== *)

(* ===== Generate Command (with subcommands) ===== *)

(* We'll fill in the app reference after obuild_app is defined *)
let completion_app_ref = ref None

(* Subcommand: generate completion *)
let cmd_generate_completion =
  let cmd =
    Cli.command "completion" ~doc:"Generate shell completion scripts"
      ~description:"Generates completion scripts for bash, zsh, or fish shells."
      ~run:(fun ctx ->
        let shell = Cli.get_positionals ctx in
        let app =
          match !completion_app_ref with
          | Some a -> a
          | None -> failwith "Internal error: app not initialized"
        in
        match shell with
        | [] ->
            eprintf "Usage: obuild generate completion <shell>\n\n";
            eprintf "Supported shells: bash, zsh, fish\n";
            exit 1
        | [ "bash" ] -> printf "%s\n" (Cli.generate_bash_completion app)
        | [ "zsh" ] -> printf "%s\n" (Cli.generate_zsh_completion app)
        | [ "fish" ] -> printf "%s\n" (Cli.generate_fish_completion app)
        | [ shell ] ->
            eprintf "Unsupported shell: %s\n" shell;
            eprintf "Supported shells: bash, zsh, fish\n";
            exit 1
        | _ ->
            eprintf "Usage: obuild generate completion <shell>\n";
            exit 1)
      ()
  in
  cmd |> Cli.help_flag
  |> Cli.positionals "shell" ~placeholder:"SHELL"
       ~doc:"Shell to generate completion for (bash, zsh, fish)"

(* Subcommand: generate merlin *)
let cmd_generate_merlin =
  let cmd =
    Cli.command "merlin" ~doc:"Generate .merlin file for IDE support"
      ~description:"Generates a .merlin configuration file from the project definition."
      ~run:(fun ctx ->
        Dist.exist ();
        let proj_file = project_read () in

        (* Generate .merlin file *)
        let merlin_content = Buffer.create 256 in

        (* Add source directories for libraries *)
        List.iter
          (fun lib ->
            List.iter
              (fun src_dir ->
                Buffer.add_string merlin_content (Printf.sprintf "S %s\n" (fp_to_string src_dir)))
              lib.Project.Library.target.Target.target_obits.Target.target_srcdir;
            Buffer.add_string merlin_content
              (Printf.sprintf "B %s\n"
                 (fp_to_string
                    (current_dir </> fn "dist" </> fn "build"
                    </> fn ("lib-" ^ Libname.to_string lib.Project.Library.name)))))
          proj_file.Project.libs;

        (* Add source directories for executables *)
        List.iter
          (fun exe ->
            List.iter
              (fun src_dir ->
                Buffer.add_string merlin_content (Printf.sprintf "S %s\n" (fp_to_string src_dir)))
              exe.Project.Executable.target.Target.target_obits.Target.target_srcdir;
            Buffer.add_string merlin_content
              (Printf.sprintf "B %s\n"
                 (fp_to_string
                    (current_dir </> fn "dist" </> fn "build" </> fn exe.Project.Executable.name))))
          proj_file.Project.exes;

        (* Collect all package dependencies *)
        let all_deps = ref [] in
        List.iter
          (fun lib ->
            all_deps :=
              lib.Project.Library.target.Target.target_obits.Target.target_builddeps @ !all_deps)
          proj_file.Project.libs;
        List.iter
          (fun exe ->
            all_deps :=
              exe.Project.Executable.target.Target.target_obits.Target.target_builddeps @ !all_deps)
          proj_file.Project.exes;
        List.iter
          (fun test ->
            all_deps :=
              test.Project.Test.target.Target.target_obits.Target.target_builddeps @ !all_deps)
          proj_file.Project.tests;

        let unique_deps = List.sort_uniq compare !all_deps in
        List.iter
          (fun (libname, _) ->
            Buffer.add_string merlin_content (Printf.sprintf "PKG %s\n" (Libname.to_string libname)))
          unique_deps;

        (* Write .merlin file *)
        let oc = open_out ".merlin" in
        output_string oc (Buffer.contents merlin_content);
        close_out oc;

        printf "Generated .merlin file\n")
      ()
  in
  cmd |> Cli.help_flag

(* Subcommand: generate opam *)
let cmd_generate_opam =
  let cmd =
    Cli.command "opam" ~doc:"Generate .opam file from project definition"
      ~description:"Generates an OPAM package file from the .obuild project definition."
      ~run:(fun ctx ->
        let proj_file = project_read () in

        (* Generate .opam file *)
        let opam_content = Buffer.create 512 in

        Buffer.add_string opam_content (Printf.sprintf "opam-version: \"2.0\"\n");
        Buffer.add_string opam_content (Printf.sprintf "name: \"%s\"\n" proj_file.Project.name);
        Buffer.add_string opam_content
          (Printf.sprintf "version: \"%s\"\n" proj_file.Project.version);

        (* Synopsis and description *)
        if proj_file.Project.synopsis <> "" then
          Buffer.add_string opam_content
            (Printf.sprintf "synopsis: \"%s\"\n" proj_file.Project.synopsis);

        if proj_file.Project.description <> "" then (
          Buffer.add_string opam_content "description: \"\"\"\n";
          Buffer.add_string opam_content proj_file.Project.description;
          Buffer.add_string opam_content "\n\"\"\"\n");

        (* Authors *)
        if proj_file.Project.authors <> [] then (
          Buffer.add_string opam_content "authors: [\n";
          List.iter
            (fun author -> Buffer.add_string opam_content (Printf.sprintf "  \"%s\"\n" author))
            proj_file.Project.authors;
          Buffer.add_string opam_content "]\n");

        (* License *)
        if proj_file.Project.license <> "" then
          Buffer.add_string opam_content
            (Printf.sprintf "license: \"%s\"\n" proj_file.Project.license);

        (* Homepage *)
        if proj_file.Project.homepage <> "" then
          Buffer.add_string opam_content
            (Printf.sprintf "homepage: \"%s\"\n" proj_file.Project.homepage);

        (* Collect all dependencies from all target types *)
        let all_deps = ref [] in
        List.iter
          (fun lib ->
            all_deps :=
              lib.Project.Library.target.Target.target_obits.Target.target_builddeps @ !all_deps)
          proj_file.Project.libs;
        List.iter
          (fun exe ->
            all_deps :=
              exe.Project.Executable.target.Target.target_obits.Target.target_builddeps @ !all_deps)
          proj_file.Project.exes;
        List.iter
          (fun test ->
            all_deps :=
              test.Project.Test.target.Target.target_obits.Target.target_builddeps @ !all_deps)
          proj_file.Project.tests;
        List.iter
          (fun bench ->
            all_deps :=
              bench.Project.Bench.target.Target.target_obits.Target.target_builddeps @ !all_deps)
          proj_file.Project.benchs;
        List.iter
          (fun example ->
            all_deps :=
              example.Project.Example.target.Target.target_obits.Target.target_builddeps @ !all_deps)
          proj_file.Project.examples;

        let unique_deps = List.sort_uniq compare !all_deps in
        if unique_deps <> [] then (
          Buffer.add_string opam_content "depends: [\n";
          Buffer.add_string opam_content "  \"ocaml\"\n";
          Buffer.add_string opam_content "  \"obuild\" {build}\n";
          List.iter
            (fun (libname, _) ->
              Buffer.add_string opam_content
                (Printf.sprintf "  \"%s\"\n" (Libname.to_string libname)))
            unique_deps;
          Buffer.add_string opam_content "]\n");

        (* Build instructions *)
        Buffer.add_string opam_content "build: [\n";
        Buffer.add_string opam_content "  [\"obuild\" \"configure\"]\n";
        Buffer.add_string opam_content "  [\"obuild\" \"build\"]\n";
        Buffer.add_string opam_content "]\n";

        (* Install instructions *)
        Buffer.add_string opam_content "install: [\n";
        Buffer.add_string opam_content "  [\"obuild\" \"install\"]\n";
        Buffer.add_string opam_content "]\n";

        (* Write .opam file *)
        let filename = proj_file.Project.name ^ ".opam" in
        let oc = open_out filename in
        output_string oc (Buffer.contents opam_content);
        close_out oc;

        printf "Generated %s\n" filename)
      ()
  in
  cmd |> Cli.help_flag

(* Main generate command with subcommands *)
let cmd_generate =
  Cli.command_with_subcommands "generate"
    ~doc:"Generate configuration files (merlin, opam, completions)"
    ~description:"Generate various configuration and helper files from the project definition."
    ~commands:[ cmd_generate_merlin; cmd_generate_opam; cmd_generate_completion ]

(* ===== Global Args Handler ===== *)

let process_global_args ctx =
  (* Process global flags *)
  if Cli.get_flag ctx "verbose" then gconf.verbosity <- Verbose;
  if Cli.get_flag ctx "quiet" then gconf.verbosity <- Silent;
  if Cli.get_flag ctx "debug" then gconf.verbosity <- Debug;
  if Cli.get_flag ctx "debug+" then gconf.verbosity <- DebugPlus;
  if Cli.get_flag ctx "color" then gconf.color <- true;

  (* Process global options *)
  (match Cli.get_string_opt ctx "findlib-conf" with
  | Some p -> Gconf.set_env "findlib-path" p
  | None -> ());
  (match Cli.get_string_opt ctx "ocamlopt" with
  | Some p -> Gconf.set_env "ocamlopt" p
  | None -> ());
  (match Cli.get_string_opt ctx "ocamldep" with
  | Some p -> Gconf.set_env "ocamldep" p
  | None -> ());
  (match Cli.get_string_opt ctx "ocamlc" with
  | Some p -> Gconf.set_env "ocamlc" p
  | None -> ());
  (match Cli.get_string_opt ctx "cc" with
  | Some p -> Gconf.set_env "cc" p
  | None -> ());
  (match Cli.get_string_opt ctx "ar" with
  | Some p -> Gconf.set_env "ar" p
  | None -> ());
  (match Cli.get_string_opt ctx "pkg-config" with
  | Some p -> Gconf.set_env "pkgconfig" p
  | None -> ());
  match Cli.get_string_opt ctx "ranlib" with
  | Some p -> Gconf.set_env "ranlib" p
  | None -> ()

(* ===== Application Definition ===== *)

let obuild_app =
  Cli.app "obuild" ~version:Path_generated.project_version
    ~doc:"Simple, declarative build system for OCaml"
    ~description:"obuild is a parallel, incremental build system for OCaml projects."
    ~global_args:
      [
        Cli.help_flag;
        Cli.version_flag;
        Cli.verbose_flag;
        Cli.quiet_flag;
        (fun cmd -> Cli.flag "debug" ~short:'d' ~doc:"Enable debug output" cmd);
        (fun cmd -> Cli.flag "debug+" ~doc:"Enable debug output with commands" cmd);
        (fun cmd -> Cli.flag "color" ~doc:"Enable colored output" cmd);
        (fun cmd ->
          Cli.option_string "findlib-conf" ~placeholder:"PATH" ~doc:"Path to findlib configuration"
            cmd);
        (fun cmd ->
          Cli.option_string "ocamlopt" ~placeholder:"PATH" ~doc:"Path to ocamlopt compiler" cmd);
        (fun cmd ->
          Cli.option_string "ocamldep" ~placeholder:"PATH" ~doc:"Path to ocamldep tool" cmd);
        (fun cmd ->
          Cli.option_string "ocamlc" ~placeholder:"PATH" ~doc:"Path to ocamlc compiler" cmd);
        (fun cmd -> Cli.option_string "cc" ~placeholder:"PATH" ~doc:"Path to C compiler" cmd);
        (fun cmd -> Cli.option_string "ar" ~placeholder:"PATH" ~doc:"Path to ar archiver" cmd);
        (fun cmd ->
          Cli.option_string "pkg-config" ~placeholder:"PATH" ~doc:"Path to pkg-config tool" cmd);
        (fun cmd -> Cli.option_string "ranlib" ~placeholder:"PATH" ~doc:"Path to ranlib tool" cmd);
      ]
    ~on_global_args:process_global_args
    ~commands:
      [
        cmd_configure;
        cmd_build;
        cmd_clean;
        cmd_sdist;
        cmd_install;
        cmd_test;
        cmd_init;
        cmd_doc;
        cmd_get;
        cmd_infer;
        cmd_generate;
      ]
    ()

(* Initialize the completion app reference *)
let () = completion_app_ref := Some obuild_app

(* ===== Main Entry Point ===== *)

let () =
  try
    (* Load config from default locations (~/.obuildrc and ./.obuildrc) *)
    let config = Cli.load_config () in
    Cli.run_with_config ~config obuild_app
  with
  | Init.ProjectAlreadyExists ->
      eprintf
        "error: found another project file in this directory. cannot run init in an already \
         existing project\n";
      exit 12
  | Init.AbortedByUser ->
      eprintf "init aborted. nothing written\n";
      exit 0
  | exn -> Exception.show exn
