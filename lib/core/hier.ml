open Fugue
open Filepath
open Compat

exception EmptyModuleHierarchy

(* module name resolves to different source files in different targets *)
exception ModuleCollision of (string * filepath * filepath)

type t = Modname.t list

(* first filepath is the source path, second is the actual path *)
type file_entry =
  | FileEntry of (filepath * filepath) (* root_path, full_path *)
  | GeneratedFileEntry of (filepath * filepath * filename)
    (* root_path, full_path, generated_path *)
  | DirectoryEntry of (filepath * filepath)
    (* root_path, full_path *)
  | AliasedFileEntry of (filepath * filepath)
(* root_path, full_path — like FileEntry, but destination artifact names are
   derived from the hier leaf instead of the source file basename, so a source
   like util.ml can compile to the unit Mylib__Util (module-alias wrapping) *)

let file_entry_to_string = function
  | FileEntry (p, f) -> Printf.sprintf "FileEntry %s %s" (fp_to_string p) (fp_to_string f)
  | DirectoryEntry (p, f) -> Printf.sprintf "DirectoryEntry %s %s" (fp_to_string p) (fp_to_string f)
  | GeneratedFileEntry (p, f, n) ->
      Printf.sprintf "GeneratedFileEntry %s %s %s" (fp_to_string p) (fp_to_string f)
        (fn_to_string n)
  | AliasedFileEntry (p, f) ->
      Printf.sprintf "AliasedFileEntry %s %s" (fp_to_string p) (fp_to_string f)

let hiers : (t, file_entry) Hashtbl.t = Hashtbl.create 128

(* Global registry of generated module names (from generate blocks across all targets) *)
let generated_modules : (string, unit) Hashtbl.t = Hashtbl.create 16

let register_generated_module name =
  Hashtbl.replace generated_modules name ()

let is_generated_module name =
  Hashtbl.mem generated_modules name

let clear () =
  Hashtbl.clear hiers;
  Hashtbl.clear generated_modules

let root = function
  | x :: _ -> x
  | [] -> raise EmptyModuleHierarchy

let parent x =
  match x with
  | [] -> failwith "internal error: hier.parent called on empty hierarchy"
  | [ _ ] -> None
  | l -> Some (list_init l)

let leaf = list_last
let make l = if l = [] then raise EmptyModuleHierarchy else l
let lvl x = List.length x - 1
let to_string x = String.concat "." (List.map Modname.to_string x)

let of_string x =
  let l = String_utils.split '.' x in
  make (List.map Modname.of_string l)

let ml_to_ext path ext =
  let f = path_basename path in
  let d = path_dirname path in
  d </> (chop_extension f <.> Filetype.to_string ext)

let of_modname x = [ x ]
let to_node x = x

let to_dirpath x =
  match x with
  | [] | [_] -> current_dir
  | _ -> fp (String.concat Filename.dir_sep (List.map Modname.to_dir $ list_init x))

let append x m = x @ [ m ]

let add_prefix prefix_path hier =
  match hier with
  | [] | [_] -> prefix_path
  | _ ->
    let to_fp = fp (String.concat Filename.dir_sep (List.map Modname.to_dir $ list_init hier)) in
    if path_length prefix_path = 0 then
      to_fp
    else
      let rec loop path hier_list =
        match hier_list with
        | [] -> path <//> to_fp
        | x :: xs ->
            if path_basename path = fn (Modname.to_dir x) then
              if path_length prefix_path = 1 then
                to_fp (* prefix_path is fully included in hier *)
              else
                loop (path_dirname path) xs
            else
              path <//> to_fp
      in
      loop prefix_path (List.tl (List.rev hier))

let check_file path filename ext =
  if ext <> Filetype.FileOther "" then
    Filesystem.exists (path </> (fn filename <.> Filetype.to_string ext))
  else
    Filesystem.exists (path </> fn filename)

let check_modname path modname ext =
  if check_file path modname ext then
    Some modname
  else
    let name = string_uncapitalize modname in
    if check_file path name ext then
      Some name
    else
      None

let get_filepath root_path hier ext : file_entry option =
  match SafeHashtbl.find_opt hiers hier with
  | Some entry ->
      (* the module already resolved through another root; if this root also
         provides a source for it, the module name is ambiguous: the cached
         entry would silently shadow the other source *)
      let entry_root =
        match entry with
        | FileEntry (r, _) | GeneratedFileEntry (r, _, _) | DirectoryEntry (r, _)
        | AliasedFileEntry (r, _) -> r
      in
      if entry_root <> root_path then begin
        let path = add_prefix root_path hier in
        match check_modname path (Modname.to_string (leaf hier)) ext with
        | Some name ->
            let conflicting =
              if ext <> Filetype.FileOther "" then
                path </> (fn name <.> Filetype.to_string ext)
              else
                path </> fn name
            in
            let existing =
              match entry with
              | FileEntry (_, f) | DirectoryEntry (_, f) | GeneratedFileEntry (_, f, _)
              | AliasedFileEntry (_, f) -> f
            in
            if conflicting <> existing then
              raise (ModuleCollision (to_string hier, existing, conflicting))
        | None -> ()
      end;
      Some entry
  | None -> (
      let path = add_prefix root_path hier in
      let modname = Modname.to_string (leaf hier) in
      let res = check_modname path modname ext in
      match res with
      | None -> None
      | Some name ->
          let entry =
            if ext <> Filetype.FileOther "" then
              FileEntry (root_path, path </> (fn name <.> Filetype.to_string ext))
            else
              DirectoryEntry (root_path, path </> fn name)
          in
          Hashtbl.add hiers hier entry;
          Some entry)

let to_filename hier prefix_path = get_filepath prefix_path hier Filetype.FileML
let to_directory hier prefix_path = get_filepath prefix_path hier (Filetype.FileOther "")

let to_generators hier prefix_path =
  match SafeHashtbl.find_opt hiers hier with
  | Some entry -> Some entry
  | None -> (
      try
        Some
          (list_find_map
             (fun gen ->
               let path = add_prefix prefix_path hier in
               let modname_t = leaf hier in
               let modname_t = gen.Generators.modname modname_t in
               let modname_str = Modname.to_string modname_t in
               let ext = Filetype.FileOther gen.Generators.suffix in
               let res = check_modname path modname_str ext in
               match res with
               | None -> None
               | Some name ->
                   let filename = fn name <.> Filetype.to_string ext in
                   let fullname = path </> filename in
                   let generated_file =
                     gen.Generators.generated_files filename (Modname.to_string (leaf hier))
                   in
                   Hashtbl.add hiers hier
                     (GeneratedFileEntry (prefix_path, fullname, generated_file));
                   Some (GeneratedFileEntry (prefix_path, fullname, generated_file)))
             (Generators.get_all ()))
      with Not_found -> None)

let get_src_file dst_dir = function
  | FileEntry (_, f) -> f
  | GeneratedFileEntry (_, _, fn) -> dst_dir </> fn
  | DirectoryEntry (_, f) -> f
  | AliasedFileEntry (_, f) -> f

(* destination basename for an aliased entry: the (uncapitalized) hier leaf,
   so the -o file name makes the compiler use the mangled unit name *)
let aliased_dest_name hier = fn (string_uncapitalize (Modname.to_string (leaf hier)))

let get_dest_file dst_dir ext hier =
  let entry =
    match SafeHashtbl.find_opt hiers hier with
    | Some e -> e
    | None -> raise Not_found
  in
  match entry with
  | FileEntry (_, f) ->
      let filename = path_basename f in
      let path = add_prefix dst_dir hier in
      path </> (chop_extension filename <.> Filetype.to_string ext)
  | GeneratedFileEntry (_, _, filename) ->
      let path = add_prefix dst_dir hier in
      path </> (chop_extension filename <.> Filetype.to_string ext)
  | DirectoryEntry (_, f) ->
      let filename = path_basename f in
      let path = add_prefix dst_dir hier in
      path </> (filename <.> Filetype.to_string ext)
  | AliasedFileEntry (_, _) ->
      let path = add_prefix dst_dir hier in
      path </> (aliased_dest_name hier <.> Filetype.to_string ext)

let get_dest_file_ext dst_dir hier ext_f =
  let entry =
    match SafeHashtbl.find_opt hiers hier with
    | Some e -> e
    | None -> raise Not_found
  in
  match entry with
  | FileEntry (_, f) ->
      let filename = path_basename f in
      let filetype = Filetype.of_filepath f in
      let path = add_prefix dst_dir hier in
      path </> (chop_extension filename <.> Filetype.to_string (ext_f filetype))
  | GeneratedFileEntry (_, _, filename) ->
      let path = add_prefix dst_dir hier in
      let filetype = Filetype.of_filename filename in
      path </> (chop_extension filename <.> Filetype.to_string (ext_f filetype))
  | DirectoryEntry (_, f) ->
      let filename = path_basename f in
      let path = add_prefix dst_dir hier in
      let filetype = Filetype.of_filepath f in
      path </> (filename <.> Filetype.to_string (ext_f filetype))
  | AliasedFileEntry (_, f) ->
      let path = add_prefix dst_dir hier in
      let filetype = Filetype.of_filepath f in
      path </> (aliased_dest_name hier <.> Filetype.to_string (ext_f filetype))

let to_interface hier prefix_path = get_filepath prefix_path hier Filetype.FileMLI
let get_file_entry_maybe hier = SafeHashtbl.find_opt hiers hier

let get_file_entry hier paths =
  match SafeHashtbl.find_opt hiers hier with
  | Some entry ->
      (* the module already resolved elsewhere (usually another target); if one
         of this target's search paths also provides a source for it, the name
         is ambiguous and the cached entry would silently shadow it *)
      let entry_root, entry_file =
        match entry with
        | FileEntry (r, f) | GeneratedFileEntry (r, f, _) | DirectoryEntry (r, f)
        | AliasedFileEntry (r, f) -> (r, f)
      in
      List.iter
        (fun path ->
          if path <> entry_root then
            let prefixed = add_prefix path hier in
            match check_modname prefixed (Modname.to_string (leaf hier)) Filetype.FileML with
            | Some name ->
                let conflicting = prefixed </> (fn name <.> Filetype.to_string Filetype.FileML) in
                if conflicting <> entry_file then
                  raise (ModuleCollision (to_string hier, entry_file, conflicting))
            | None -> ())
        paths;
      entry
  | None ->
      list_find_map
        (fun path ->
          try
            Some
              (list_find_map
                 (fun lookup -> lookup hier path)
                 [ to_filename; to_directory; to_generators; to_interface ])
          with Not_found -> None)
        paths

(* Side-effect-free existence probe: check whether a source (ml, mli, directory
   or generator input) provides this module in one of the given paths, WITHOUT
   registering anything in the global hiers table.  Used by project validation,
   which must not pollute the registry: for pack: true libraries the same bare
   module name may legitimately exist in several targets. *)
let source_exists hier paths =
  match SafeHashtbl.find_opt hiers hier with
  | Some _ -> true
  | None ->
      let name = Modname.to_string (leaf hier) in
      List.exists
        (fun path ->
          let prefixed = add_prefix path hier in
          check_modname prefixed name Filetype.FileML <> None
          || check_modname prefixed name Filetype.FileMLI <> None
          || check_modname prefixed name (Filetype.FileOther "") <> None
          || List.exists
               (fun gen ->
                 let gname = Modname.to_string (gen.Generators.modname (leaf hier)) in
                 check_modname prefixed gname (Filetype.FileOther gen.Generators.suffix) <> None)
               (Generators.get_all ()))
        paths

(* Register a synthetic file entry for modules that will be generated during build
   (e.g., cstubs-generated modules, generate-block modules). This allows get_dest_file
   to work for these modules even before the source file exists.
   This function REPLACES any existing entry because during dependency analysis
   a directory or other entry might have been cached before we knew it was synthetic. *)
let register_synthetic_entry hier root_path full_path =
  Hashtbl.replace hiers hier (FileEntry (root_path, full_path))

(* Register a generated file entry for modules produced by generators (e.g., atdgen).
   This allows modules like Ollama_t (from ollama.atd) to be discovered.
   - hier: the module hierarchy (e.g., Ollama_t)
   - root_path: the source directory containing the generator input
   - src_path: full path to the source file (e.g., lib/ollama.atd)
   - output_file: the generated output filename (e.g., ollama_t.ml) *)
let register_generated_entry hier root_path src_path output_file =
  Hashtbl.replace hiers hier (GeneratedFileEntry (root_path, src_path, output_file))

(* Register a directory entry for virtual pack modules (pack: true libraries).
   The full path's basename determines the pack's artifact names (<name>.cmi/.cmo/.cmx)
   and does not need to exist on disk. *)
let register_directory_entry hier root_path full_path =
  Hashtbl.replace hiers hier (DirectoryEntry (root_path, full_path))

(* Register an aliased file entry: the source compiles under the unit name of
   the hier leaf (e.g. util.ml as Mylib__Util) for module-alias wrapping. *)
let register_aliased_entry hier root_path full_path =
  Hashtbl.replace hiers hier (AliasedFileEntry (root_path, full_path))

let of_filename filename =
  let name = Filename.chop_extension (fn_to_string filename) in
  let m =
    try Modname.wrap (string_capitalize name) with
    | Modname.EmptyModuleName -> raise (Modname.ModuleFilenameNotValid (fn_to_string filename))
    | Invalid_argument _ -> raise (Modname.ModuleFilenameNotValid (fn_to_string filename))
  in
  make [ m ]
