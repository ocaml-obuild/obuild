let finally fct clean_f =
    let result = try fct (); with exn -> clean_f (); raise exn in
    clean_f ();
    result

let maybe d f v = match v with None -> d | Some x -> f x
let may f v = maybe None (fun x -> Some (f x)) v
let default d v = maybe d (fun x -> x) v
let maybe_unit f v = maybe () f v

let ($) f a = f a

let id = (fun x -> x)

let (</>) a b =
    let len = String.length a in
    if len > 0
        then (let dsep = Filename.dir_sep in
                if String.make 1 (a.[len-1]) = dsep
                    then a ^ b
                    else a ^ dsep ^ b)
        else b

let string_index_pred p s =
    let len = String.length s in
    let i = ref 0 in
    while !i < len && not (p s.[!i]) do
        i := !i + 1
    done;
    if !i == len
        then (raise Not_found)
        else !i

let rec string_split ?limit:(limit=(-1)) c s =
    let i = try String.index s c with Not_found -> -1 in
    let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
    if i = -1 || nlimit = 0 then
        [ s ]
    else
        let a = String.sub s 0 i
        and b = String.sub s (i + 1) (String.length s - i - 1) in
        a :: (string_split ~limit: nlimit c b)

let rec string_split_pred ?limit:(limit=(-1)) p s =
    let i = try string_index_pred p s with Not_found -> -1 in
    let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
    if i = -1 || nlimit = 0 then
        [ s ]
    else
        let a = String.sub s 0 i
        and b = String.sub s (i + 1) (String.length s - i - 1) in
        a :: (string_split_pred ~limit: nlimit p b)

let string_startswith prefix x =
    let x_l = String.length x and prefix_l = String.length prefix in
    prefix_l <= x_l && String.sub x 0 prefix_l  = prefix

let string_stripPredicate p str =
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

let string_stripSpaces = string_stripPredicate (fun c -> c = ' ' || c = '\t' || c = '\n')

let string_splitAt pos s =
    let len = String.length s in
    if pos > len
    then invalid_arg "splitAt"
    else (String.sub s 0 pos, String.sub s pos (len - pos))

let string_take n s =
    let len = String.length s in
    if n > len
    then invalid_arg "take"
    else String.sub s 0 n

let string_drop n s =
    let len = String.length s in
    if n > len
    then invalid_arg "drop"
    else String.sub s n (len - n)

let string_words s = List.filter (fun x -> x <> "") (string_split ' ' s)

let list_iteri f list =
    let rec loop i l =
        match l with
        | []    -> ()
        | x::xs -> f i x; loop (i+1) xs
        in
    loop 1 list

let hashtbl_map f h =
    let newh = Hashtbl.create (Hashtbl.length h) in
    Hashtbl.iter (fun k v -> Hashtbl.add newh k (f v)) h;
    newh

let hashtbl_modify_one f k h =
    let v = Hashtbl.find h k in
    Hashtbl.replace h k (f v)

let hashtbl_modify_all f h =
    let keys = Hashtbl.fold (fun k _ acc -> k :: acc) h [] in
    List.iter (fun k ->
        let v = Hashtbl.find h k in
        Hashtbl.replace h k (f v)
    ) keys

let hashtbl_fromList l =
    let h = Hashtbl.create (List.length l) in
    List.iter (fun (k,v) -> Hashtbl.add h k v) l;
    h

let hashtbl_toList h = Hashtbl.fold (fun k v l -> (k,v)::l) h []

let first f (a,b) = (f a, b)
let second f (a,b) = (a, f b)

exception ConversionIntFailed of string * string

let user_int_of_string loc s =
    try int_of_string s with _ -> raise (ConversionIntFailed (loc,s))

