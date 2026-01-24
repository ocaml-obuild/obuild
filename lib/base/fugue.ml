let finally fct clean_f =
  let result =
    try fct ()
    with exn ->
      clean_f ();
      raise exn
  in
  clean_f ();
  result

let maybe d f v =
  match v with
  | None -> d
  | Some x -> f x

let default d v = maybe d (fun x -> x) v
let maybe_unit f v = maybe () f v
let const v _ = v

let rec maybes_to_list l =
  match l with
  | [] -> []
  | None :: xs -> maybes_to_list xs
  | Some x :: xs -> x :: maybes_to_list xs

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

let ( $ ) f a = f a
let id x = x
let char_is_alphanum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
let no_empty emptyVal = List.filter (fun x -> x <> emptyVal)

let rec list_init l =
  match l with
  | [] -> failwith "init empty list"
  | [ _ ] -> []
  | x :: xs -> x :: list_init xs

let rec list_last l =
  match l with
  | [] -> failwith "last is empty"
  | [ x ] -> x
  | _ :: xs -> list_last xs

let list_remove e list = List.filter (fun x -> x <> e) list

let list_iteri f list =
  let rec loop i l =
    match l with
    | [] -> ()
    | x :: xs ->
        f i x;
        loop (i + 1) xs
  in
  loop 1 list

let list_eq_noorder (l1 : 'a list) (l2 : 'a list) : bool = List.for_all (fun z -> List.mem z l2) l1

let list_filter_map (f : 'a -> 'b option) (l : 'a list) : 'b list =
  (* Use safe implementation from Compat *)
  Compat.SafeList.filter_map f l

let rec list_uniq l =
  match l with
  | [] -> []
  | x :: xs ->
      if List.mem x xs then
        list_uniq xs
      else
        x :: list_uniq xs

let list_find_map p l =
  (* Use safe implementation from Compat, convert option to exception *)
  match Compat.SafeList.find_map p l with
  | Some z -> z
  | None -> raise Not_found

let hashtbl_map f h =
  let newh = Hashtbl.create (Hashtbl.length h) in
  Hashtbl.iter (fun k v -> Hashtbl.add newh k (f v)) h;
  newh

let hashtbl_keys h = Hashtbl.fold (fun k _ l -> k :: l) h []

let hashtbl_modify_all f h =
  let keys = hashtbl_keys h in
  List.iter
    (fun k ->
      let v = Hashtbl.find h k in
      Hashtbl.replace h k (f v))
    keys

let hashtbl_from_list l =
  let h = Hashtbl.create (List.length l) in
  List.iter (fun (k, v) -> Hashtbl.add h k v) l;
  h

let hashtbl_to_list h = Hashtbl.fold (fun k v l -> (k, v) :: l) h []
let first f (a, b) = (f a, b)
let second f (a, b) = (a, f b)

exception ConversionIntFailed of string * string
exception ConversionBoolFailed of string * string

let user_int_of_string loc s = try int_of_string s with _ -> raise (ConversionIntFailed (loc, s))

let user_bool_of_string loc s =
  try bool_of_string s with _ -> raise (ConversionBoolFailed (loc, s))

module StringSet = struct
  include Set.Make (struct
    type t = string

    let compare = compare
  end)

  let to_list t = fold (fun elt l -> elt :: l) t []
end
