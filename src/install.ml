open Ext
open Printf
open Filepath
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

    (* TODO hardcoded tests *)
    let fileCma = "a.cma" in
    let fileCmxa = "a.cmxa" in

    let subPkgs =
        List.map (fun sub ->
            newPkg (list_last (lib_name_to_string_nodes sub.lib_name))
        ) lib.lib_subs
        in

    let pkg = newPkg "" in
    { pkg with
          package_version          = projFile.version
        ; package_description      = if lib.lib_description <> "" then lib.lib_description else projFile.description
        ; package_requires         = []
        ; package_archives         =
            [ ([Pred_Byte]  , fileCma)
            ; ([Pred_Native], fileCmxa)
            ]
        ; package_subs             = subPkgs
    }

let write_lib_meta projFile lib =
    let dir = Dist.getBuildDest (Dist.Target lib.lib_target.target_name) in
    let metadirPath = dir </> fn "META" in
    let pkg = lib_to_meta projFile lib in
    Meta.write metadirPath pkg


