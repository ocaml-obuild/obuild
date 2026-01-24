open Fugue
open Filepath
open Types
open Helper
open Printf
open Analyze
open Target
open Prepare
open Gconf
open Buildprogs

(* Helper: get include paths for ctypes from dependencies *)
let get_ctypes_includes bstate =
  let stdlib = fp (Analyze.get_ocaml_config_key "standard_library" bstate.bstate_config) in
  (* Find ctypes package paths *)
  try
    (* Get integers library path *)
    let (integers_path, integers_pkg) = Metacache.get "integers" in
    let integers_dir = Meta.get_include_dir stdlib (integers_path, integers_pkg) in

    (* Get str library path *)
    let (str_path, str_pkg) = Metacache.get "str" in
    let str_dir = Meta.get_include_dir stdlib (str_path, str_pkg) in

    let (path, pkg) = Metacache.get "ctypes" in
    let ctypes_dir = Meta.get_include_dir stdlib (path, pkg) in
    let ctypes_stubs_lib = Libname.of_string "ctypes.stubs" in
    let (stubs_path, stubs_root_pkg) = Metacache.get "ctypes" in
    let stubs_pkg = Meta.Pkg.find ctypes_stubs_lib.Libname.subnames stubs_root_pkg in
    let stubs_dir = Meta.get_include_dir stdlib (stubs_path, stubs_pkg) in
    [integers_dir; str_dir; ctypes_dir; stubs_dir]
  with _ -> []

(* Helper: get ctypes library files for linking *)
let get_ctypes_libs bstate =
  let stdlib = fp (Analyze.get_ocaml_config_key "standard_library" bstate.bstate_config) in
  try
    (* Get integers library (required by ctypes) *)
    let (integers_path, integers_pkg) = Metacache.get "integers" in
    let integers_dir = Meta.get_include_dir stdlib (integers_path, integers_pkg) in
    let integers_cma = integers_dir </> fn "integers.cma" in

    (* Get str library (required by ctypes.stubs) *)
    let (str_path, str_pkg) = Metacache.get "str" in
    let str_dir = Meta.get_include_dir stdlib (str_path, str_pkg) in
    let str_cma = str_dir </> fn "str.cma" in

    (* Get ctypes library *)
    let (path, pkg) = Metacache.get "ctypes" in
    let ctypes_dir = Meta.get_include_dir stdlib (path, pkg) in
    let ctypes_cma = ctypes_dir </> fn "ctypes.cma" in

    (* Get ctypes.stubs library *)
    let ctypes_stubs_lib = Libname.of_string "ctypes.stubs" in
    let (stubs_path, stubs_root_pkg) = Metacache.get "ctypes" in
    let stubs_pkg = Meta.Pkg.find ctypes_stubs_lib.Libname.subnames stubs_root_pkg in
    let stubs_dir = Meta.get_include_dir stdlib (stubs_path, stubs_pkg) in
    let stubs_cma = stubs_dir </> fn "ctypes_stubs.cma" in

    [integers_cma; str_cma; ctypes_cma; stubs_cma]
  with _ -> []

let get_nb_step dag =
  let nb_step = Dag.length dag in
  let nb_step_len = String.length (string_of_int nb_step) in
  (nb_step, nb_step_len)

(* Generate ctypes.cstubs type discovery - produces types_generated.ml *)
let generate_cstubs_types task_index task lib bstate task_context dag =
  let (cstate, target) = Hashtbl.find task_context task in
  let (nb_step, nb_step_len) = get_nb_step dag in
  verbose Report "[%*d of %d] Generating cstubs types for %s\n%!" nb_step_len task_index nb_step
    (Libname.to_string lib);
  match target.target_cstubs with
  | None ->
    verbose Report "  No cstubs configuration found\n%!";
    Scheduler.FinishTask task
  | Some cstubs ->
    let autogen_dir = Dist.get_build_exn Dist.Autogen </> fn (Libname.to_string lib) in
    Filesystem.mkdir_safe autogen_dir 0o755;
    let generated_types_name = cstubs.cstubs_generated_types in
    (* Write generated files to autogen directory - no placeholders needed *)
    let target_file = autogen_dir </> fn (Compat.string_uncapitalize generated_types_name ^ ".ml") in

    (* Check if we have type description *)
    match cstubs.cstubs_type_description with
    | None ->
      (* No type description - generate empty types module *)
      let content = Printf.sprintf
        "(* Auto-generated type bindings for %s *)\n\
         (* No type description specified *)\n"
        (Libname.to_string lib)
      in
      Filesystem.write_file target_file content;
      verbose Report "  Generated %s (no types)\n%!" (fp_to_string target_file);
      Scheduler.FinishTask task
    | Some type_desc ->
      (* Get the bindings module name *)
      let bindings_hier = type_desc.Target.cstubs_functor in
      let bindings_parts = Hier.to_string bindings_hier in
      (* Split "Bindings.Types" into module "Bindings" and functor "Types" *)
      let parts = String_utils.split '.' bindings_parts in
      let (bindings_module, types_functor) = match parts with
        | [m; f] -> (m, f)
        | [m] -> (m, "Types")
        | _ -> (List.hd parts, "Types")
      in

      (* Get paths *)
      let ctypes_includes = get_ctypes_includes bstate in
      let build_dir = cstate.compilation_builddir_ml Normal in
      let src_dirs = target.target_obits.target_srcdir in

      (* Generate discover.ml that uses Cstubs_structs.write_c to discover type layouts.
         This properly handles structs defined in the user's Types functor. *)
      let discover_ml = autogen_dir </> fn "discover.ml" in
      let headers_str = String.concat "; "
        (List.map (fun h -> Printf.sprintf "\"#include <%s>\\n\"" h) cstubs.cstubs_headers) in
      let headers_list = if headers_str = "" then "[]" else Printf.sprintf "[%s]" headers_str in
      let discover_content = Printf.sprintf
        "(* Auto-generated type discovery for %s using Cstubs_structs *)\n\
         let () =\n\
        \  let headers = String.concat \"\" %s in\n\
        \  let fmt = Format.std_formatter in\n\
        \  Format.fprintf fmt \"#include <stddef.h>@.\";\n\
        \  Format.fprintf fmt \"#include <stdint.h>@.\";\n\
        \  Format.fprintf fmt \"%%s\" headers;\n\
        \  Cstubs_structs.write_c fmt (module %s.%s)\n"
        (Libname.to_string lib)
        headers_list
        bindings_module
        types_functor
      in
      Filesystem.write_file discover_ml discover_content;
      verbose Report "  Generated %s\n%!" (fp_to_string discover_ml);

      (* Compile discover.ml - needs ctypes.stubs and the bindings module *)
      let discover_exe = autogen_dir </> fn "discover.byte" in
      let ocamlc = Prog.get_ocamlc () in
      let ctypes_libs = get_ctypes_libs bstate in
      let bindings_cmo = build_dir </> fn (Compat.string_uncapitalize bindings_module ^ ".cmo") in
      let include_args = List.concat [
        Utils.to_include_path_options ctypes_includes;
        Utils.to_include_path_options [build_dir];
        Utils.to_include_path_options src_dirs;
        Utils.to_include_path_options [autogen_dir];
      ] in
      let lib_args = List.map fp_to_string ctypes_libs in
      (* Check if bindings.cmo exists *)
      let bindings_exists = Filesystem.exists bindings_cmo in
      (* Link with ctypes libs and the user's bindings module *)
      let compile_args = [ocamlc] @ include_args @ lib_args @
        (if bindings_exists then [fp_to_string bindings_cmo] else []) @
        ["-o"; fp_to_string discover_exe; fp_to_string discover_ml] in

      verbose Report "  Compiling type discovery program...\n%!";
      (match Process.run compile_args with
      | Process.Failure err ->
        verbose Report "  Warning: Failed to compile discover.ml: %s\n%!" err;
        verbose Report "  Falling back to static type sizes\n%!";
        (* Fallback: generate static content *)
        let content = Printf.sprintf
          "(* Auto-generated type bindings for %s *)\n\
           (* Generated statically - type discovery compilation failed *)\n\
           \n\
           let size_t_size = %d\n"
          (Libname.to_string lib)
          (Sys.word_size / 8)
        in
        Filesystem.write_file target_file content;
        Scheduler.FinishTask task
      | Process.Success _ ->
        (* Run discover to generate C code *)
        verbose Report "  Running type discovery program...\n%!";
        (match Process.run [fp_to_string discover_exe] with
        | Process.Failure err ->
          verbose Report "  Warning: Failed to run discover: %s\n%!" err;
          let content = Printf.sprintf
            "(* Auto-generated type bindings for %s *)\n\
             let size_t_size = %d\n"
            (Libname.to_string lib)
            (Sys.word_size / 8)
          in
          Filesystem.write_file target_file content;
          Scheduler.FinishTask task
        | Process.Success (c_code, _, _) ->
          (* Write the C program *)
          let discover_c = autogen_dir </> fn "discover.c" in
          Filesystem.write_file discover_c c_code;
          verbose Report "  Generated %s\n%!" (fp_to_string discover_c);

          (* Compile the C program - include ctypes directory for ctypes_cstubs_internals.h *)
          let discover_c_exe = autogen_dir </> fn "discover_c" in
          let cc = Prog.get_cc () in
          let ocaml_include = Analyze.get_ocaml_config_key "standard_library" bstate.bstate_config in
          (* Get ctypes include paths for ctypes_cstubs_internals.h *)
          let ctypes_c_includes = List.concat (List.map
            (fun p -> ["-I"; fp_to_string p])
            ctypes_includes) in
          let compile_c_args = [cc; "-I"; ocaml_include] @ ctypes_c_includes @
            ["-o"; fp_to_string discover_c_exe; fp_to_string discover_c] in

          verbose Report "  Compiling C type discovery...\n%!";
          (match Process.run compile_c_args with
          | Process.Failure err ->
            verbose Report "  Warning: Failed to compile discover.c: %s\n%!" err;
            let content = Printf.sprintf
              "(* Auto-generated type bindings for %s *)\n\
               let size_t_size = %d\n"
              (Libname.to_string lib)
              (Sys.word_size / 8)
            in
            Filesystem.write_file target_file content;
            Scheduler.FinishTask task
          | Process.Success _ ->
            (* Run the C program to get types *)
            verbose Report "  Running C type discovery...\n%!";
            (match Process.run [fp_to_string discover_c_exe] with
            | Process.Failure err ->
              verbose Report "  Warning: Failed to run C discover: %s\n%!" err;
              let content = Printf.sprintf
                "(* Auto-generated type bindings for %s *)\n\
                 let size_t_size = %d\n"
                (Libname.to_string lib)
                (Sys.word_size / 8)
              in
              Filesystem.write_file target_file content;
              Scheduler.FinishTask task
            | Process.Success (ml_code, _, _) ->
              Filesystem.write_file target_file ml_code;
              verbose Report "  Generated %s\n%!" (fp_to_string target_file);
              ignore (bindings_module, types_functor);
              Scheduler.FinishTask task
            )
          )
        )
      )

(* Generate ctypes.cstubs function stubs - produces C.ml and stubs.c *)
let generate_cstubs_functions task_index task lib bstate task_context dag =
  let (cstate, target) = Hashtbl.find task_context task in
  let (nb_step, nb_step_len) = get_nb_step dag in
  verbose Report "[%*d of %d] Generating cstubs functions for %s\n%!" nb_step_len task_index nb_step
    (Libname.to_string lib);
  match target.target_cstubs with
  | None ->
    verbose Report "  No cstubs configuration found\n%!";
    Scheduler.FinishTask task
  | Some cstubs ->
    let autogen_dir = Dist.get_build_exn Dist.Autogen </> fn (Libname.to_string lib) in
    Filesystem.mkdir_safe autogen_dir 0o755;
    let entry_point_name = cstubs.cstubs_generated_entry_point in
    let c_lib_name = cstubs.cstubs_external_library_name in
    let generated_types = cstubs.cstubs_generated_types in

    match cstubs.cstubs_function_description with
    | None ->
      (* No function description - generate minimal entry point *)
      let entry_file = autogen_dir </> fn (Compat.string_uncapitalize entry_point_name ^ ".ml") in
      let entry_content = Printf.sprintf
        "(* Auto-generated entry point for %s *)\n\
         module Types = %s\n\
         module Functions = struct end\n"
        (Libname.to_string lib)
        generated_types
      in
      Filesystem.write_file entry_file entry_content;
      (* Generate empty C stubs *)
      let c_stubs_file = autogen_dir </> fn (c_lib_name ^ "_stubs.c") in
      Filesystem.write_file c_stubs_file
        "/* Auto-generated C stubs - no functions bound */\n\
         #include <caml/mlvalues.h>\n";
      verbose Report "  Generated %s (no functions)\n%!" (fp_to_string entry_file);
      Scheduler.FinishTask task
    | Some func_desc ->
      let bindings_hier = func_desc.Target.cstubs_functor in
      let bindings_parts = Hier.to_string bindings_hier in
      let parts = String_utils.split '.' bindings_parts in
      let (bindings_module, functions_functor) = match parts with
        | [m; f] -> (m, f)
        | [m] -> (m, "Functions")
        | _ -> (List.hd parts, "Functions")
      in

      (* Get types functor name from type description *)
      let types_functor = match cstubs.cstubs_type_description with
        | Some type_desc ->
          let type_parts = String_utils.split '.' (Hier.to_string type_desc.Target.cstubs_functor) in
          (match type_parts with
           | [_; f] -> f
           | [_] -> "Types"
           | _ -> "Types")
        | None -> "Types"
      in

      (* Get paths *)
      let ctypes_includes = get_ctypes_includes bstate in
      let ctypes_libs = get_ctypes_libs bstate in
      let build_dir = cstate.compilation_builddir_ml Normal in
      let src_dirs = target.target_obits.target_srcdir in
      (* Write all generated ML files to autogen directory - no placeholders needed *)
      let ml_output_dir = autogen_dir in
      ignore src_dirs;

      (* Path to compiled bindings module *)
      let bindings_cmo = build_dir </> fn (Compat.string_uncapitalize bindings_module ^ ".cmo") in

      (* Generate stubgen.ml *)
      let stubgen_ml = autogen_dir </> fn "stubgen.ml" in
      let prefix = c_lib_name in
      let entry_file_name = Compat.string_uncapitalize entry_point_name ^ ".ml" in
      let stubs_file_name = c_lib_name ^ "_stubs.c" in

      (* Generate a module name for the generated FOREIGN implementation *)
      let generated_foreign_name = c_lib_name ^ "_generated" in
      let generated_foreign_file = Compat.string_uncapitalize generated_foreign_name ^ ".ml" in

      (* Convert concurrency policy to Cstubs module value string *)
      let concurrency_str = match cstubs.cstubs_concurrency with
        | Target.Cstubs_sequential -> "Cstubs.sequential"
        | Target.Cstubs_unlocked -> "Cstubs.unlocked"
        | Target.Cstubs_lwt_jobs -> "Cstubs.lwt_jobs"
        | Target.Cstubs_lwt_preemptive -> "Cstubs.lwt_preemptive"
      in

      (* Convert errno policy to Cstubs module value string *)
      let errno_str = match cstubs.cstubs_errno with
        | Target.Cstubs_ignore_errno -> "Cstubs.ignore_errno"
        | Target.Cstubs_return_errno -> "Cstubs.return_errno"
      in

      let stubgen_content = Printf.sprintf
        "(* Auto-generated stub generator for %s *)\n\
         let prefix = \"%s\"\n\
         let autogen_dir = \"%s\"\n\
         let ml_output_dir = \"%s\"\n\
         \n\
         let () =\n\
        \  (* Generate C stubs to autogen directory *)\n\
        \  let c_file = open_out (Filename.concat autogen_dir \"%s\") in\n\
        \  let c_fmt = Format.formatter_of_out_channel c_file in\n\
        \  Format.fprintf c_fmt \"/* Auto-generated by ctypes.cstubs */\\n\";\n\
        \  Format.fprintf c_fmt \"#include <caml/mlvalues.h>\\n\";\n\
        \  Format.fprintf c_fmt \"#include <caml/memory.h>\\n\";\n\
        \  Format.fprintf c_fmt \"#include <caml/alloc.h>\\n\";\n\
        \  Format.fprintf c_fmt \"#include <caml/custom.h>\\n\";\n\
        \  Format.fprintf c_fmt \"#include <caml/callback.h>\\n\";\n\
        \  Format.fprintf c_fmt \"#include <caml/fail.h>\\n\";\n\
        \  Format.fprintf c_fmt \"#include <string.h>\\n\";\n\
        \  Format.fprintf c_fmt \"\\n\";\n\
        \  Cstubs.write_c c_fmt ~concurrency:%s ~errno:%s ~prefix (module %s.%s);\n\
        \  close_out c_file;\n\
         \n\
        \  (* Generate FOREIGN implementation module to source directory *)\n\
        \  let foreign_file = open_out (Filename.concat ml_output_dir \"%s\") in\n\
        \  let foreign_fmt = Format.formatter_of_out_channel foreign_file in\n\
        \  Format.fprintf foreign_fmt \"(* Auto-generated FOREIGN implementation for %s *)\\n\";\n\
        \  Cstubs.write_ml foreign_fmt ~concurrency:%s ~errno:%s ~prefix (module %s.%s);\n\
        \  close_out foreign_file;\n\
         \n\
        \  (* Generate entry point that applies user's functor to generated module *)\n\
        \  let entry_file = open_out (Filename.concat ml_output_dir \"%s\") in\n\
        \  let entry_fmt = Format.formatter_of_out_channel entry_file in\n\
        \  Format.fprintf entry_fmt \"(* Auto-generated entry point for %s *)\\n\";\n\
        \  Format.fprintf entry_fmt \"(* Apply user's Types functor to generated TYPE implementation for struct layouts *)\\n\";\n\
        \  Format.fprintf entry_fmt \"module Types = %s.%s(%s)\\n\\n\";\n\
        \  Format.fprintf entry_fmt \"(* Apply user's Functions functor to generated FOREIGN implementation *)\\n\";\n\
        \  Format.fprintf entry_fmt \"module C_Functions = %s.%s(%s)\\n\";\n\
        \  close_out entry_file;\n\
         \n\
        \  print_endline \"Stub generation complete\"\n"
        (Libname.to_string lib)
        prefix
        (fp_to_string autogen_dir)
        (fp_to_string ml_output_dir)
        stubs_file_name
        concurrency_str errno_str
        bindings_module functions_functor
        generated_foreign_file
        (Libname.to_string lib)
        concurrency_str errno_str
        bindings_module functions_functor
        entry_file_name
        (Libname.to_string lib)
        bindings_module types_functor generated_types
        bindings_module functions_functor
        (Compat.string_capitalize generated_foreign_name)
      in
      Filesystem.write_file stubgen_ml stubgen_content;
      verbose Report "  Generated %s\n%!" (fp_to_string stubgen_ml);

      (* Compile stubgen.ml *)
      let stubgen_exe = autogen_dir </> fn "stubgen.byte" in
      let ocamlc = Prog.get_ocamlc () in
      let include_args = List.concat [
        Utils.to_include_path_options ctypes_includes;
        Utils.to_include_path_options [build_dir];
        Utils.to_include_path_options [autogen_dir];
        Utils.to_include_path_options src_dirs;
      ] in
      let lib_args = List.map fp_to_string ctypes_libs in

      (* Check if bindings.cmo exists *)
      let bindings_exists = Filesystem.exists bindings_cmo in
      verbose Report "  Bindings module at %s: %s\n%!"
        (fp_to_string bindings_cmo) (if bindings_exists then "found" else "not found");

      let compile_args = [ocamlc] @ include_args @ lib_args @
        (if bindings_exists then [fp_to_string bindings_cmo] else []) @
        ["-o"; fp_to_string stubgen_exe; fp_to_string stubgen_ml] in

      verbose Report "  Compiling stub generator...\n%!";
      (match Process.run compile_args with
      | Process.Failure err ->
        verbose Report "  Warning: Failed to compile stubgen.ml: %s\n%!" err;
        verbose Report "  Falling back to placeholder stubs\n%!";
        (* Fallback: generate placeholder content *)
        let entry_file = autogen_dir </> fn (Compat.string_uncapitalize entry_point_name ^ ".ml") in
        let entry_content = Printf.sprintf
          "(* Auto-generated entry point for %s *)\n\
           (* Stub generation failed - placeholder *)\n\
           \n\
           module Types = %s\n\
           \n\
           module Functions = struct\n\
          \  (* Function stubs would be here *)\n\
           end\n"
          (Libname.to_string lib)
          generated_types
        in
        Filesystem.write_file entry_file entry_content;
        let c_stubs_file = autogen_dir </> fn (c_lib_name ^ "_stubs.c") in
        Filesystem.write_file c_stubs_file
          "/* Auto-generated C stubs - placeholder */\n\
           #include <caml/mlvalues.h>\n\
           #include <caml/memory.h>\n\
           #include <caml/alloc.h>\n";
        verbose Report "  Generated placeholder stubs\n%!";
        Scheduler.FinishTask task
      | Process.Success _ ->
        (* Run stubgen *)
        verbose Report "  Running stub generator...\n%!";
        (match Process.run [fp_to_string stubgen_exe] with
        | Process.Failure err ->
          verbose Report "  Warning: Failed to run stubgen: %s\n%!" err;
          (* Fallback *)
          let entry_file = autogen_dir </> fn (Compat.string_uncapitalize entry_point_name ^ ".ml") in
          let entry_content = Printf.sprintf
            "(* Auto-generated entry point for %s *)\n\
             module Types = %s\n\
             module Functions = struct end\n"
            (Libname.to_string lib)
            generated_types
          in
          Filesystem.write_file entry_file entry_content;
          let c_stubs_file = autogen_dir </> fn (c_lib_name ^ "_stubs.c") in
          Filesystem.write_file c_stubs_file
            "/* Auto-generated C stubs */\n\
             #include <caml/mlvalues.h>\n";
          Scheduler.FinishTask task
        | Process.Success (output, _, _) ->
          verbose Report "  %s\n%!" output;
          verbose Report "  Stubs generated successfully\n%!";
          Scheduler.FinishTask task
        )
      )

(* Compile generated ctypes.cstubs C code *)
let compile_cstubs_c task_index task lib bstate task_context dag =
  let (cstate, target) = Hashtbl.find task_context task in
  let (nb_step, nb_step_len) = get_nb_step dag in
  verbose Report "[%*d of %d] Compiling cstubs C for %s\n%!" nb_step_len task_index nb_step
    (Libname.to_string lib);
  match target.target_cstubs with
  | None ->
    verbose Report "  No cstubs configuration found\n%!";
    Scheduler.FinishTask task
  | Some cstubs ->
    let autogen_dir = Dist.get_build_exn Dist.Autogen </> fn (Libname.to_string lib) in
    let c_lib_name = cstubs.cstubs_external_library_name in
    let c_stubs_file = fn (c_lib_name ^ "_stubs.c") in
    (* Add ctypes include paths for C headers like ctypes_cstubs_internals.h *)
    let ctypes_c_includes = get_ctypes_includes bstate in
    let c_dir_spec = {
      include_dirs = cstate.compilation_c_include_paths @ ctypes_c_includes;
      dst_dir      = autogen_dir;
      src_dir      = autogen_dir
    } in
    let dest = (Filetype.FileO, c_dir_spec.dst_dir </> o_from_cfile c_stubs_file) in
    verbose Report "  Compiling %s -> %s\n%!" (fn_to_string c_stubs_file) (fp_to_string (snd dest));
    (* Use the C compiler to compile the stubs *)
    Scheduler.AddProcess (task, run_c_compile bstate.bstate_config c_dir_spec [] c_stubs_file)
