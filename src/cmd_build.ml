open Obuild.Gconf
open Obuild

let args = [
  ("-j", Arg.Int (fun i -> gconf.parallel_jobs <- i), "N maximum number of jobs in parallel");
  ("--jobs", Arg.Int (fun i -> gconf.parallel_jobs <- i), "N maximum number of jobs in parallel");
  ("--dot", Arg.Unit (fun () -> gconf.dump_dot <- true), " dump dependencies dot files during build");
  ("--noocamlmklib", Arg.Unit (fun () -> gconf.ocamlmklib <- false), " do not use ocamlmklib when linking C code");
]

let mainBuild argv =
  Dist.exist ();
  let setup = App_utils.read_setup () in
  let proj_file = App_utils.project_read () in
  let flags = Configure.check proj_file true setup in
  let project = Analyze.prepare proj_file flags in
  let bstate = Prepare.init project in

  let dag = match argv with
    | [] -> project.Analyze.project_targets_dag
    | _  ->
      let targets = List.map Target.Name.of_string argv in
      Dag.subset project.Analyze.project_targets_dag targets
  in
  Build.build_dag bstate proj_file dag

let () =
  let cmd = {
    Cmd.name = "build";
    args = args;
    fn = mainBuild;
    short_desc = "Build every buildable bits";
    long_desc = "\
Build all your different targets (library, executable,
tests, benchmarks, example) that are marked as buildable.";
  } in
  Cmd.register_cmd cmd
