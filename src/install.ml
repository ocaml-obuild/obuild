open Obuild
open Ext.Fugue
open Ext.Filepath
open Printf
open Modname
open Project
open Types
open Meta
open Target

let opam_install_file projFile =
    let installPath = Dist.getDistPath () </> (fn projFile.name <.> "install") in
    Utils.generateFile installPath (fun add ->
        let cat_of_target target =
            match target.target_name with
            | ExeName _     -> Some "bin"
            | LibName _     -> Some "lib"
            | TestName _    -> None
            | BenchName _   -> None
            | ExampleName _ -> None
            in

        let allTargets = Project.get_all_buildable_targets projFile in
        List.iter (fun target ->
            match cat_of_target target with
            | None -> ()
            | Some cat ->
                add (sprintf "%s: [\n" cat);
                (* FIXME *)
                List.iter (fun file ->
                    add (sprintf "  %s\n" file)
                ) [];
                add ("\n");
        ) allTargets
    )

let lib_to_meta projFile lib =
    let requires_of_lib lib =
        let deps = lib.lib_target.target_obits.target_builddeps in
        [ (None, List.map (fun d -> fst d) deps) ]
        in
    let set_meta_field_from_lib pkg lib =
        { pkg with
              package_requires    = requires_of_lib lib
            ; package_description = if lib.lib_description <> "" then lib.lib_description else projFile.description
            ; package_archives    =
                [ ([Pred_Byte]  , fn_to_string (cmca_of_lib ByteCode Normal lib.lib_name))
                ; ([Pred_Native], fn_to_string (cmca_of_lib Native Normal lib.lib_name))
                ]
        }
        in

    let subPkgs =
        List.map (fun sub ->
            let npkg = newPkg (list_last (lib_name_to_string_nodes sub.lib_name)) in
            set_meta_field_from_lib npkg sub
        ) lib.lib_subs
        in

    let pkg = set_meta_field_from_lib (newPkg "") lib in
    { pkg with
          package_version          = projFile.version
        ; package_subs             = subPkgs
    }

let write_lib_meta projFile lib =
    let dir = Dist.getBuildDest (Dist.Target lib.lib_target.target_name) in
    let metadirPath = dir </> fn "META" in
    let pkg = lib_to_meta projFile lib in
    Meta.write metadirPath pkg


