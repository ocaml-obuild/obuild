open Fugue

 (** [index_pred p s] returns the index of the first character in [s] satisfying predicate [p].
    @raise Not_found if no character satisfies the predicate *)
let index_pred p s =
  let len = String.length s in
  let i = ref 0 in
  while !i < len && not (p s.[!i]) do
    i := !i + 1
  done;
  if !i == len then
    raise Not_found
  else
    !i

(** [strip_predicate p s] removes leading and trailing characters satisfying predicate [p] from [s]
*)
let rec split ?(limit = -1) c s =
  let i = try String.index s c with Not_found -> -1 in
  let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
  if i = -1 || nlimit = 0 then
    [ s ]
  else
    let a = String.sub s 0 i and b = String.sub s (i + 1) (String.length s - i - 1) in
    a :: split ~limit:nlimit c b

let rec split_pred ?(limit = -1) p s =
  let i = try index_pred p s with Not_found -> -1 in
  let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
  if i = -1 || nlimit = 0 then
    [ s ]
  else
    let a = String.sub s 0 i and b = String.sub s (i + 1) (String.length s - i - 1) in
    a :: split_pred ~limit:nlimit p b

let startswith prefix x =
  let x_l = String.length x and prefix_l = String.length prefix in
  prefix_l <= x_l && String.sub x 0 prefix_l = prefix

let endswith suffix x = Filename.check_suffix x suffix

let strip_predicate p str =
  let len = String.length str in
  let s = ref 0 in
  let e = ref (String.length str) in
  while !s < len && p str.[!s] do
    s := !s + 1
  done;
  let start = !s in
  while !e > start && p str.[!e - 1] do
    e := !e - 1
  done;
  String.sub str start (!e - start)

let strip_spaces = strip_predicate (fun c -> c = ' ' || c = '\t' || c = '\n')

let split_at pos s =
  let len = String.length s in
  if pos > len then
    invalid_arg "splitAt"
  else
    (String.sub s 0 pos, String.sub s pos (len - pos))

let drop n s =
  let len = String.length s in
  if n > len then
    invalid_arg "String.drop"
  else
    String.sub s n (len - n)

let init n s =
  let len = String.length s in
  if n > len then
    invalid_arg "String.init"
  else
    String.sub s 0 (len - n)

let all p s =
  let len = String.length s in
  let rec loop i = if i = len then true else if not (p s.[i]) then false else loop (i + 1) in
  loop 0

let lines s = split '\n' s
let words s = split_pred (fun c -> c = ' ' || c = '\n' || c = '\t') s
let words_noempty s = no_empty "" (words s)
let lines_noempty s = no_empty "" (lines s)
