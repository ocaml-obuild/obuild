
(* Result type definition - common across versions *)
type ('a, 'b) result = Ok of 'a | Error of 'b

module Result = struct
  type ('a, 'b) t = ('a, 'b) result

  let ok x = Ok x
  let error e = Error e

  let is_ok = function Ok _ -> true | Error _ -> false
  let is_error = function Ok _ -> false | Error _ -> true

  let map f = function
    | Ok x -> Ok (f x)
    | Error e -> Error e

  let map_error f = function
    | Ok x -> Ok x
    | Error e -> Error (f e)

  let bind r f = match r with
    | Ok x -> f x
    | Error e -> Error e

  let value r ~default = match r with
    | Ok x -> x
    | Error _ -> default

  let get_ok = function
    | Ok x -> x
    | Error _ -> invalid_arg "Result.get_ok"

  let get_error = function
    | Ok _ -> invalid_arg "Result.get_error"
    | Error e -> e
end

(* Option helpers *)
module Option = struct
  let map f = function
    | Some x -> Some (f x)
    | None -> None

  let bind o f = match o with
    | Some x -> f x
    | None -> None

  let value o ~default = match o with
    | Some x -> x
    | None -> default

  let get = function
    | Some x -> x
    | None -> invalid_arg "Option.get"

  let is_some = function
    | Some _ -> true
    | None -> false

  let is_none = function
    | Some _ -> false
    | None -> true
end

(* SafeList - List module with exception-safe operations *)
module SafeList = struct
  include List

  let find_opt pred lst =
    try Some (List.find pred lst)
    with Not_found -> None

  let assoc_opt key lst =
    try Some (List.assoc key lst)
    with Not_found -> None

  let nth_opt lst n =
    try Some (List.nth lst n)
    with Failure _ | Invalid_argument _ -> None

  let filter_map f lst =
    let rec aux acc = function
      | [] -> List.rev acc
      | x :: xs ->
          match f x with
          | Some y -> aux (y :: acc) xs
          | None -> aux acc xs
    in
    aux [] lst

  let find_map f lst =
    let rec aux = function
      | [] -> None
      | x :: xs ->
          match f x with
          | Some _ as result -> result
          | None -> aux xs
    in
    aux lst
end

(* SafeHashtbl - Hashtbl module with exception-safe operations *)
module SafeHashtbl = struct
  include Hashtbl

  let find_opt tbl key =
    try Some (Hashtbl.find tbl key)
    with Not_found -> None

  let update tbl key f =
    match find_opt tbl key with
    | Some v -> Hashtbl.replace tbl key (f (Some v))
    | None -> Hashtbl.replace tbl key (f None)

  let find_default tbl key default =
    try Hashtbl.find tbl key
    with Not_found -> default

  let add_or_update tbl key ~default ~update =
    match find_opt tbl key with
    | Some v -> Hashtbl.replace tbl key (update v)
    | None -> Hashtbl.add tbl key default
end

(* SafeString - String module with exception-safe operations *)
module SafeString = struct
  include String

  let index_opt str ch =
    try Some (String.index str ch)
    with Not_found -> None

  let rindex_opt str ch =
    try Some (String.rindex str ch)
    with Not_found -> None

  let index_from_opt str pos ch =
    try Some (String.index_from str pos ch)
    with Not_found | Invalid_argument _ -> None

  let rindex_from_opt str pos ch =
    try Some (String.rindex_from str pos ch)
    with Not_found | Invalid_argument _ -> None

  let sub_safe str start len =
    try Some (String.sub str start len)
    with Invalid_argument _ -> None

  let get_opt str idx =
    try Some (String.get str idx)
    with Invalid_argument _ -> None
end
