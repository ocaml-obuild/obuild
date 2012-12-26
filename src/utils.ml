let rec split ?limit:(limit=(-1)) c s =
    let i = try String.index s c with Not_found -> -1 in
    let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
    if i = -1 || nlimit = 0 then
        [ s ]
    else
        let a = String.sub s 0 i
        and b = String.sub s (i + 1) (String.length s - i - 1) in
        a :: (split ~limit: nlimit c b)

let startswith prefix x =
    let x_l = String.length x and prefix_l = String.length prefix in
    prefix_l <= x_l && String.sub x 0 prefix_l  = prefix

let stripPredicate p str =
    let len = String.length str in
    let s = ref 0 in
    let e = ref (String.length str) in
    while !s < len && p str.[!s] do
        s := !s + 1
    done;
    let start = !s in
    while !e > start && p str.[!e-1] do
        e := !e - 1
    done;
    String.sub str start (!e - start)

let splitAt pos s =
    let len = String.length s in
    if pos > len
    then invalid_arg "splitAt"
    else (String.sub s 0 pos, String.sub s pos (len - pos))

let take n s =
    let len = String.length s in
    if n > len
    then invalid_arg "take"
    else String.sub s 0 n

let drop n s =
    let len = String.length s in
    if n > len
    then invalid_arg "drop"
    else String.sub s n (len - n)

let read_file_with f filename = 
    let lines = ref [] in
    let chan = open_in filename in
    try
        while true; do
            let z = f (input_line chan) in
            match z with
            | None    -> ()
            | Some z' -> lines := z' :: !lines
        done; []
    with End_of_file ->
        close_in chan;
        List.rev !lines
