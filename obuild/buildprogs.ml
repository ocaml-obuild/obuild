open Types
open Ext
open Ext.Filepath
open Ext.Fugue
open Helper
open Process
open Printf
open Filetype
open Analyze
open Target
open Prepare
open Gconf
open Pp

exception LinkingFailed of string
exception InferFailed of string

type c_linking_mode = LinkingStatic | LinkingShared

type linking_mode = LinkingLibrary | LinkingPlugin | LinkingExecutable

type annotation_mode = AnnotationNone | AnnotationBin | AnnotationText | AnnotationBoth

type packopt = Hier.t option

let annotToOpts annotMode =
    match annotMode with
    | AnnotationNone -> []
    | AnnotationBin  -> ["-bin-annot"]
    | AnnotationText -> ["-annot"]
    | AnnotationBoth -> ["-bin-annot";"-annot"]

let runOcamlCompile dirSpec useThread annotMode buildMode compileOpt packopt pp oflags modhier =
    let dstDir = dirSpec.dst_dir in
    let compileOpt = if buildMode = Interface && compileOpt = WithProf then WithDebug else compileOpt in
    Filesystem.mkdirSafeRecursive dstDir 0o755;
    let (prog, srcFile, dstFile) =
        match buildMode with
        | Interface ->
            (Prog.getOcamlC ()
            ,Hier.to_interface modhier dirSpec.src_dir
            ,Hier.to_cmi dstDir modhier
            )
        | Compiled ct ->
            ((if ct = ByteCode then Prog.getOcamlC () else Prog.getOcamlOpt ())
            ,Hier.to_filename modhier dirSpec.src_dir
            ,Hier.to_cmc ct dstDir modhier
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
             @ pp_to_params pp
             @ maybe [] (fun x -> if buildMode = Compiled Native then [ "-for-pack"; Hier.to_string x ] else []) packopt
             @ (if gconf.short_path then [ "-short-paths" ] else [])

             @ ["-o"; fp_to_string dstFile ]
             @ ["-c"; fp_to_string srcFile ]
        in
    Process.create args

let runOcamlPack srcDir dstDir annotMode buildMode packOpt dest modules =
    let prog = if buildMode = ByteCode then Prog.getOcamlC () else Prog.getOcamlOpt () in
    Filesystem.mkdirSafeRecursive dstDir 0o755;
    let args = [prog]
             @ maybe [] (fun x -> if buildMode = Native then [ "-for-pack"; Hier.to_string x ] else []) packOpt
             @ annotToOpts annotMode
             @ [ "-pack"; "-o"; fp_to_string (Hier.to_cmc buildMode dstDir dest); ]
             @ List.map (fun m -> fp_to_string (Hier.to_cmc buildMode srcDir m)) modules
        in
    Process.create args

let runOcamlInfer srcDir includes pp modname =
    let args = [Prog.getOcamlC (); "-i"]
             @ pp_to_params pp
             @ (Utils.to_include_path_options includes)
             @ [fp_to_string (Hier.to_filename srcDir modname)]
        in
    match run args with
    | Success (mli, _, _) -> mli
    | Failure er       -> raise (InferFailed er)

let o_from_cfile file = file <.> "o"

let runCCompile project dirSpec cflags file =
    let dstDir = dirSpec.dst_dir in
    Filesystem.mkdirSafeRecursive dstDir 0o755;
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
    Process.create args

let runAr dest deps =
  let args = [ Prog.getAR (); "rc"; fp_to_string dest ] @ List.map fp_to_string deps in
  Process.create args

let runRanlib dest =
  Process.create [ Prog.getRanlib (); fp_to_string dest ]

let runCLinking sharingMode depfiles dest =
  let args = if gconf.ocamlmklib then
      [ Prog.getOcamlMklib () ] @ (match sharingMode with
          | LinkingStatic -> ["-custom"]
          | LinkingShared   -> [])
      @ ["-o"; fp_to_string dest ]
      @ List.map fp_to_string depfiles
    else (* Not working if system != linux *)
      [ Prog.getCC () ]
      @ (match sharingMode with
          | LinkingStatic -> []
          | LinkingShared -> ["-shared"]) (* TODO: fix this for all system != linux *)
      @ ["-o"; fp_to_string dest ]
      @ List.map fp_to_string depfiles in
  Process.create args

let runOcamlLinking includeDirs buildMode linkingMode compileType useThread cclibs libs modules dest =
  (* create a soft link to a freshly compiled exe, unless a file already exist with
     the soft link name we were planning to use *)
  let createSoftLink linkingMode dest =
    (match linkingMode with
     | LinkingPlugin     -> ()
     | LinkingLibrary    -> ()
     | LinkingExecutable ->
       if Gconf.get_target_option "executable-as-obj" then ()
       else
         let real = fp_to_string dest in
         let basename = Filename.basename real in
         if Sys.file_exists basename then () (* FBR: should we log that? *)
         else
           let (_: Process.t) = Process.create ["ln"; "-s"; real; basename] in
           ())
  in
  let prog = match buildMode with
    | Native    -> Prog.getOcamlOpt ()
    | ByteCode  -> Prog.getOcamlC ()
  in
  let args = [ prog ]
             @ (match useThread with
                 | NoThread   -> []
                 | WithThread -> ["-thread"])
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
             @ (List.map fp_to_string $ List.map (Hier.to_cmc buildMode currentDir) modules)
  in
  let res = Process.create args in
  let () = createSoftLink linkingMode dest in
  res
