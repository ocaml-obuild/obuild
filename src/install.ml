open Project
open Meta

let lib_to_meta projFile lib =
    let metadirPath = dir </> fn "META" in

    let subPkgs = List.map (fun sub ->
        let p = newPackage sub.sublib_name in
        { p with package_archives =
             [
             ]
        }
    ) lib.lib_subs in

    (* TODO hardcoded *)
    let fileCma = "a.cma" in
    let fileCmxa = "a.cmxa" in

    let pkg = newPackage "" in
    { pkg with
          package_version          = projFile.version
        ; package_description      = projFile.description
        ; package_requires         = []
        ; package_archives         =
            [ (["byte"]  , fileCma)
            ; (["native"], fileCmxa)
            ]
        ; package_subs             = subPkgs
    }
