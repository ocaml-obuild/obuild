let finally fct clean_f =
    let result = try fct (); with exn -> clean_f (); raise exn in
    clean_f ();
    result

let maybe d f v = match v with None -> d | Some x -> f x
let may f v = maybe None (fun x -> Some (f x)) v
let default d v = maybe d (fun x -> x) v
let maybe_unit f v = maybe () f v
let const v = (fun _ -> v)

let rec maybes_to_list l =
    match l with
    | []             -> []
    | None :: xs     -> maybes_to_list xs
    | (Some x) :: xs -> x :: maybes_to_list xs

type ('a,'b) either = Left of 'a | Right of 'b

let ($) f a = f a

let id = (fun x -> x)

let char_is_alphanum c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

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

let string_endswith suffix x = Filename.check_suffix x suffix

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
    then invalid_arg "String.take"
    else String.sub s 0 n

let string_drop n s =
    let len = String.length s in
    if n > len
    then invalid_arg "String.drop"
    else String.sub s n (len - n)

let string_init n s =
    let len = String.length s in
    if n > len
    then invalid_arg "String.init"
    else String.sub s 0 (len - n)

let string_all p s =
    let len = String.length s in
    let rec loop i =
        if i = len then true else (if not (p s.[i]) then false else loop (i+1))
        in
    loop 0

let string_lines s = string_split '\n' s
let string_words s = string_split_pred (fun c -> c = ' ' || c = '\n' || c = '\t') s

let no_empty emptyVal = List.filter (fun x -> x <> emptyVal)
let string_words_noempty s = no_empty "" (string_words s)
let string_lines_noempty s = no_empty "" (string_lines s)

let list_singleton = fun x -> [x]

let rec list_init l =
    match l with
    | []    -> failwith "init empty list"
    | [_]   -> []
    | x::xs -> x :: list_init xs

let rec list_last l =
    match l with
    | []    -> failwith "last is empty"
    | [x]   -> x 
    | _::xs -> list_last xs

let list_remove e list = List.filter (fun x -> x <> e) list

let list_iteri f list =
    let rec loop i l =
        match l with
        | []    -> ()
        | x::xs -> f i x; loop (i+1) xs
        in
    loop 1 list

let list_eq_noorder (l1: 'a list) (l2: 'a list) : bool =
    List.for_all (fun z -> List.mem z l2) l1

let list_filter_map (f: 'a -> 'b option) (l: 'a list) : 'b list =
    let rec loop (z: 'a list) : 'b list =
        match z with
        | []    -> []
        | x::xs -> match f x with
                   | None   -> loop xs
                   | Some y -> y :: loop xs
        in
    loop l

let list_mem_many needles haystack =
    let rec loop l =
        match l with
        | []    -> false
        | x::xs -> if List.mem x needles then true else loop xs
        in
    loop haystack

let rec list_uniq l =
    match l with
    | []    -> []
    | x::xs -> if List.mem x xs
                   then list_uniq xs
                   else x :: list_uniq xs

let rec list_findmap p l =
    match l with
    | []    -> raise Not_found
    | x::xs -> match p x with
               | Some z -> z
               | None   -> list_findmap p xs

let hashtbl_map f h =
    let newh = Hashtbl.create (Hashtbl.length h) in
    Hashtbl.iter (fun k v -> Hashtbl.add newh k (f v)) h;
    newh

let hashtbl_keys h = Hashtbl.fold (fun k _ l -> k :: l) h []

let hashtbl_modify_one f k h =
    let v = Hashtbl.find h k in
    Hashtbl.replace h k (f v)

let hashtbl_modify_all f h =
    let keys = hashtbl_keys h in
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
exception ConversionBoolFailed of string * string

let user_int_of_string loc s =
    try int_of_string s with _ -> raise (ConversionIntFailed (loc,s))

let user_bool_of_string loc s =
    try bool_of_string s with _ -> raise (ConversionBoolFailed (loc,s))

module StringSet = struct
  include Set.Make(struct
    type t = string
    let compare = Pervasives.compare
  end)
  let to_list t = fold (fun elt l -> elt::l) t []
end 
