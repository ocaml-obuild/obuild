open Ext
open Helper
open Types

exception ConfigChanged of string

let getDigestKV () =
    let digest = Project.projectDigest () in
    [ ("obuild-digest", digest) ]

let run gconf projFile =
    let cache = Prepare.prepare gconf projFile in

    Dist.checkOrCreate ();
    let digestKV = getDigestKV () in
    (* write setup file *)
    Filesystem.removeDirContent Dist.distPath;
    Dist.write_setup (digestKV @ hashtbl_toList cache.Prepare.data_ocamlcfg);

    let autogenDir = Dist.createBuildDest Dist.Autogen in
    (*generateMlFile (autogenDir </> "path_generated.ml")*)

    ()

let check gconf =
    Dist.checkOrFail ();

    let comparekvs reason setup l =
        List.iter (fun (k,v) ->
            try let v' = List.assoc k setup in
                if v' <> v then raise Not_found
            with Not_found -> raise (ConfigChanged reason)
        ) l
        in
    let setup = Dist.read_setup () in
    let ocamlCfg = Prog.getOcamlConfig gconf true in
    let digestKV = getDigestKV () in

    comparekvs "ocaml config" setup ocamlCfg;
    comparekvs "digest" setup digestKV;

    gconf.conf_setup <- setup;
    ()
