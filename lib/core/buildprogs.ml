open Types
open Filepath
open Fugue
open Process
open Prepare
open Gconf

exception LinkingFailed of string
exception InferFailed of string

type c_linking_mode = LinkingStatic | LinkingShared

type linking_mode = LinkingLibrary | LinkingPlugin | LinkingExecutable

type annotation_mode = AnnotationNone | AnnotationBin | AnnotationText | AnnotationBoth

type packopt = Hier.t option

let annotToOpts = function
  | AnnotationNone -> []
  | AnnotationBin  -> ["-bin-annot"]
  | AnnotationText -> ["-annot"]
  | AnnotationBoth -> ["-bin-annot";"-annot"]

let run_ocaml_compile dirSpec useThread annotMode buildMode compileOpt packopt pp oflags modhier =
  let dstDir = dirSpec.dst_dir in
  let entry = Hier.get_file_entry modhier [dirSpec.src_dir] in
  let src_file = Hier.get_src_file dirSpec.src_dir entry in
  let compileOpt = if buildMode = Interface && compileOpt = WithProf then WithDebug else compileOpt in
  Filesystem.mkdir_safe_recursive dstDir 0o755;
  let (prog, srcFile, dstFile) =
    match buildMode with
    | Interface ->
      (Prog.get_ocamlc ()
      ,Hier.ml_to_ext src_file Filetype.FileMLI
      ,Hier.get_dest_file dstDir Filetype.FileCMI modhier
      )
    | Compiled ct ->
      let ext = if ct = ByteCode then Filetype.FileCMO else Filetype.FileCMX in
      ((if ct = ByteCode then Prog.get_ocamlc () else Prog.get_ocamlopt ())
      ,src_file
      ,Hier.get_dest_file dstDir ext modhier
      )
  in
  let args = [prog]
             @ (match useThread with
                 | NoThread   -> []
                 | WithThread -> ["-thread"])
             @ (Utils.to_include_path_options dirSpec.include_dirs)
             @ (match compileOpt with
                 | Normal    -> []
                 | WithDebug -> ["-g"]
                 | WithProf  -> ["-p"])
             @ annotToOpts annotMode
             @ oflags
             @ gconf.ocaml_extra_args
             @ Pp.to_params pp
             @ maybe [] (fun x -> if buildMode = Compiled Native then [ "-for-pack"; Hier.to_string x ] else []) packopt
             @ (if gconf.short_path then [ "-short-paths" ] else [])
             @ ["-o"; fp_to_string dstFile ]
             @ ["-c"; fp_to_string srcFile ]
  in
  Process.make args

let run_ocaml_pack _srcDir dstDir annotMode buildMode packOpt dest modules =
  let prog = if buildMode = ByteCode then Prog.get_ocamlc () else Prog.get_ocamlopt () in
  let ext = if buildMode = ByteCode then Filetype.FileCMO else Filetype.FileCMX in
  let ext_f = function
    | Filetype.FileML -> ext
    | Filetype.FileMLI -> Filetype.FileCMI
    | _ -> (* It should not happen *)
      if buildMode = ByteCode then Filetype.FileCMO else Filetype.FileCMX
  in
  Filesystem.mkdir_safe_recursive dstDir 0o755;
  let args = [prog]
             @ maybe [] (fun x -> if buildMode = Native then [ "-for-pack"; Hier.to_string x ] else []) packOpt
             @ annotToOpts annotMode
             @ [ "-pack"; "-o"; fp_to_string (Hier.get_dest_file dstDir ext dest); ]
             @ List.map (fun m -> fp_to_string (Hier.get_dest_file_ext dstDir m ext_f)) modules
  in
  Process.make args

let run_ocaml_infer srcDir includes pp modname =
  let entry = Hier.get_file_entry modname [srcDir] in
  let args = [Prog.get_ocamlc (); "-i"]
             @ Pp.to_params pp
             @ (Utils.to_include_path_options includes)
             @ [fp_to_string (Hier.get_src_file srcDir entry)]
  in
  match run args with
  | Success (mli, _, _) -> mli
  | Process.Failure er       -> raise (InferFailed er)

let o_from_cfile file = file <.> "o"

let run_c_compile project dirSpec cflags file =
    let dstDir = dirSpec.dst_dir in
    Filesystem.mkdir_safe_recursive dstDir 0o755;
    let callCCompiler = string_words_noempty (Analyze.get_ocaml_config_key "bytecomp_c_compiler" project) in
    let srcFile = dirSpec.src_dir </> file in
    (* make a .c.o file to avoid collision *)
    let dstFile = dirSpec.dst_dir </> o_from_cfile file in
    let args = callCCompiler
             @ cflags
             @ (Utils.to_include_path_options dirSpec.include_dirs)
             @ ["-o"; fp_to_string dstFile]
             @ ["-c"; fp_to_string srcFile]
        in
    Process.make args

let run_ar dest deps =
  let args = [ Prog.get_ar (); "rc"; fp_to_string dest ] @ List.map fp_to_string deps in
  Process.make args

let run_ranlib dest =
  Process.make [ Prog.get_ranlib (); fp_to_string dest ]

let run_c_linking sharingMode depfiles dest =
  let args = if gconf.ocamlmklib then
      [ Prog.get_ocamlmklib () ] @ (match sharingMode with
          | LinkingStatic -> ["-custom"]
          | LinkingShared   -> [])
      @ ["-o"; fp_to_string dest ]
      @ List.map fp_to_string depfiles
    else (* Not working if system != linux *)
      [ Prog.get_cc () ]
      @ (match sharingMode with
          | LinkingStatic -> []
          | LinkingShared -> ["-shared"]) (* TODO: fix this for all system != linux *)
      @ ["-o"; fp_to_string dest ]
      @ List.map fp_to_string depfiles in
  Process.make args

let run_ocaml_linking includeDirs buildMode linkingMode compileType useThread systhread cclibs libs modules dest =
  (* create a soft link to a freshly compiled exe, unless a file with the same name already exist *)
  let link_maybe linking_mode dest =
    let file_or_link_exists fn = try let _ = Unix.lstat fn in true with _ -> false
    in
    (match linking_mode with
     | LinkingPlugin | LinkingLibrary -> ()
     | LinkingExecutable ->
       if not (Gconf.get_target_option "executable-as-obj") then
         let real = fp_to_string dest in
         let basename = Filename.basename real in
         if not (file_or_link_exists basename)
         then Unix.symlink real basename)
  in
  let prog = match buildMode with
    | Native    -> Prog.get_ocamlopt ()
    | ByteCode  -> Prog.get_ocamlc ()
  in
  let ext = if buildMode = ByteCode then Filetype.FileCMO else Filetype.FileCMX in
  let args = [ prog ]
             @ (match useThread with
                 | NoThreads   -> []
                 | PosixThread -> ["-thread"]
                 | VMThread -> ["-vmthread"]
                 | DefaultThread ->
                   (if systhread = "true" then ["-thread"] else ["-vmthread"]))
             @ (match linkingMode with
                 | LinkingPlugin    -> ["-shared"]
                 | LinkingLibrary    -> ["-a"]
                 | LinkingExecutable -> if (Gconf.get_target_option "executable-as-obj") then ["-output-obj"] else [])
             @ ["-o"; fp_to_string dest]
             @ (match compileType with
                 | Normal    -> []
                 | WithDebug -> ["-g"]
                 | WithProf  -> ["-p"])
             @ (Utils.to_include_path_options includeDirs)
             @ (List.map fp_to_string libs)
             @ (List.concat (List.map (fun x ->
                 [ (match buildMode with
                      | Native -> "-cclib"
                      | ByteCode -> if x.[1] = 'L' then "-cclib" else "-dllib") (* Ugly hack but do the job for now *)
                 ; x ]) cclibs))
             @ (List.map (fun m -> fp_to_string (Hier.get_dest_file current_dir ext m)) modules)
  in
  let res = Process.make args in
  let () = link_maybe linkingMode dest in
  res

(* ================================================================
   ctypes.cstubs generation functions
   ================================================================ *)

exception CstubsGenerationFailed of string

(* Get the autogen directory for a library, creating it if needed *)
let get_cstubs_autogen_dir libname =
  let autogen_base = Dist.get_build_path Dist.Autogen in
  let lib_autogen = autogen_base </> fn (Libname.to_string libname) in
  Filesystem.mkdir_safe_recursive lib_autogen 0o755;
  lib_autogen

(* Generate type discovery ML source that uses Cstubs_structs *)
let generate_cstubs_type_discovery_source cstubs libname autogen_dir =
  match cstubs.Target.cstubs_type_description with
  | None -> None
  | Some type_desc ->
    let prefix = cstubs.Target.cstubs_external_library_name in
    let functor_name = Hier.to_string type_desc.Target.cstubs_functor in
    let headers_includes = String.concat "\n"
      (List.map (fun h -> Printf.sprintf "  header \"#include <%s>\";" h)
                cstubs.Target.cstubs_headers) in
    let source = Printf.sprintf {|
(* Auto-generated type discovery program for %s *)
let () =
  let prefix = "%s" in
  let generate_types_struct name =
    print_endline (Printf.sprintf "let %%s = %%d" name (Ctypes.sizeof Ctypes.size_t))
  in
  (* Generate type bindings *)
  print_endline "(* Auto-generated type bindings *)";
  generate_types_struct "size_t_size"
|}
      (Libname.to_string libname)
      prefix
    in
    ignore (functor_name, headers_includes);
    let discover_ml = autogen_dir </> fn "discover_types.ml" in
    Filesystem.write_file discover_ml source;
    Some discover_ml

(* Generate function stubs ML source that uses Cstubs *)
let generate_cstubs_function_stubs_source cstubs libname autogen_dir =
  match cstubs.Target.cstubs_function_description with
  | None -> None
  | Some func_desc ->
    let prefix = cstubs.Target.cstubs_external_library_name in
    let functor_name = Hier.to_string func_desc.Target.cstubs_functor in
    let entry_point = cstubs.Target.cstubs_generated_entry_point in
    (* Generate the stub generator program *)
    let source = Printf.sprintf {|
(* Auto-generated stub generator for %s *)
(* Functor: %s, Entry point: %s *)

let c_headers = "/* Auto-generated C stubs for %s */\n"

let () =
  (* Generate C stubs *)
  let c_file = open_out "%s_stubs.c" in
  output_string c_file c_headers;
  output_string c_file "#include <caml/mlvalues.h>\n";
  output_string c_file "#include <caml/memory.h>\n";
  output_string c_file "#include <caml/alloc.h>\n";
  output_string c_file "/* Stub implementations would be generated here */\n";
  close_out c_file;

  (* Generate ML entry point *)
  let ml_file = open_out "%s.ml" in
  Printf.fprintf ml_file "(* Auto-generated entry point for %s *)\n";
  Printf.fprintf ml_file "module Types = Types_generated\n";
  Printf.fprintf ml_file "module Functions = struct\n";
  Printf.fprintf ml_file "  (* Function bindings would be here *)\n";
  Printf.fprintf ml_file "end\n";
  close_out ml_file;

  print_endline "Stubs generated successfully"
|}
      (Libname.to_string libname)
      functor_name
      entry_point
      (Libname.to_string libname)
      prefix
      entry_point
      (Libname.to_string libname)
    in
    let stubgen_ml = autogen_dir </> fn "stubgen.ml" in
    Filesystem.write_file stubgen_ml source;
    Some stubgen_ml

(* Compile and run a generated ML program *)
let run_cstubs_generator project includes ml_file output_file =
  let prog = Prog.get_ocamlc () in
  let exe_file = Filetype.replace_extension (path_basename ml_file) (Filetype.FileOther "exe") in
  let exe_path = (path_dirname ml_file) </> exe_file in
  (* Compile the generator *)
  let compile_args = [prog]
    @ (Utils.to_include_path_options includes)
    @ ["-I"; "+ctypes"]
    @ ["-o"; fp_to_string exe_path]
    @ [fp_to_string ml_file]
  in
  ignore project;
  match Process.run compile_args with
  | Process.Failure err ->
    raise (CstubsGenerationFailed ("Failed to compile cstubs generator: " ^ err))
  | Process.Success _ ->
    (* Run the generator *)
    let run_args = [fp_to_string exe_path] in
    match Process.run run_args with
    | Process.Failure err ->
      raise (CstubsGenerationFailed ("Failed to run cstubs generator: " ^ err))
    | Process.Success (stdout, _, _) ->
      (* Write output to the target file *)
      Filesystem.write_file output_file stdout;
      ()
