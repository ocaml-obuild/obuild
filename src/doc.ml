open Ext
open Gconf

exception DocumentationBuildingFailed of string

let runOcamldoc pp =
    let args = [ Prog.getOcamlDoc (); "-html" ]
             @ (maybe [] (fun s -> ["-pp"; s ]) pp)
             @ []
        in
    match Process.run_with_outputs args with
    | Process.Failure er      -> raise (DocumentationBuildingFailed er)
    | Process.Success (_,_) -> ()

let run projFile =
    ()
