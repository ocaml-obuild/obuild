exception UnexpectedFileType

let removeDir path =
    let rec rmdir_recursive path =
        let dirhandle = Unix.opendir path in
        (try
            while true do
                let ent = Unix.readdir dirhandle in
                if String.length ent > 0 && ent.[0] <> '.'
                    then
                        let fent = path ^ Filename.dir_sep ^ ent in
                        match (Unix.lstat ent).Unix.st_kind with
                        | Unix.S_DIR -> rmdir_recursive ent
                        | Unix.S_REG -> Unix.unlink ent
                        | _          -> raise UnexpectedFileType
            done;
        with End_of_file ->
            ()
        );
        Unix.closedir dirhandle;
        Unix.rmdir path 
        in
    rmdir_recursive path
