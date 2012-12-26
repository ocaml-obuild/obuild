open Types

exception DistNotADirectory
exception DistDoesntExist

let distPath = "dist"
let checkDist f =
    if Sys.file_exists distPath
        then (if Sys.is_directory distPath
                then ()
                else raise DistNotADirectory
        ) else
            f ()

let checkDistOrFail () = checkDist (fun () -> raise DistDoesntExist)
let checkDistOrCreate () = checkDist (fun () -> Unix.mkdir distPath 0o755)

let getGeneralConfig () =
    { conf_verbosity = Report 
    }
