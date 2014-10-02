open Ext.Fugue
open Obuild

exception DocumentationBuildingFailed of string

let runOcamldoc pp =
    let args = [ Prog.getOcamlDoc (); "-html" ]
             @ (maybe [] (fun s -> ["-pp"; s ]) pp)
             @ []
        in
    match Process.run args with
    | Process.Failure er      -> raise (DocumentationBuildingFailed er)
    | Process.Success (_,_,_) -> ()

let run projFile =
    ()
