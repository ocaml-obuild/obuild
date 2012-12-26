open Ext
open Types

exception ConfigChanged of string

let getDigestKV () =
    let digest = Conf.projectDigest () in
    [ ("obuild-digest", digest) ]

let run generalConf projFile =
    Dist.checkOrCreate ();
    let digestKV = getDigestKV () in
    let ocamlCfg = Prog.getOcamlConfig true in
    (* write setup file *)
    Filesystem.removeDirContent Dist.distPath;
    Dist.write_setup (digestKV @ ocamlCfg);

    let autogenDir = Dist.createBuildDest Dist.Autogen in
    (*generateMlFile (autogenDir </> "path_generated.ml")*)
    ()

let check generalConf =
    Dist.checkOrFail ();

    let comparekvs reason setup l =
        List.iter (fun (k,v) ->
            try let v' = List.assoc k setup in
                if v' <> v then raise Not_found
            with Not_found -> raise (ConfigChanged reason)
        ) l
        in
    let setup = Dist.read_setup () in
    let ocamlCfg = Prog.getOcamlConfig true in
    let digestKV = getDigestKV () in

    comparekvs "ocaml config" setup ocamlCfg;
    comparekvs "digest" setup digestKV;
    ()
