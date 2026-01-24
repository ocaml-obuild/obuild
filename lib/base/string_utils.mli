(** {1 String operations} *)

val split : ?limit:int -> char -> string -> string list
(** [split ?limit c s] splits string [s] on character [c]. [limit] controls maximum number of splits
    (-1 for unlimited) *)

val split_pred : ?limit:int -> (char -> bool) -> string -> string list
(** [split_pred ?limit p s] splits string [s] at characters satisfying predicate [p]. [limit]
    controls maximum number of splits (-1 for unlimited) *)

val startswith : string -> string -> bool
(** [startswith prefix s] tests if [s] starts with [prefix] *)

val endswith : string -> string -> bool
(** [endswith suffix s] tests if [s] ends with [suffix] *)

val strip_spaces : string -> string
(** Remove leading and trailing whitespace (space, tab, newline) *)

val split_at : int -> string -> string * string
(** [split_at pos s] splits [s] at position [pos] into [(left, right)].
    @raise Invalid_argument if [pos] > length of [s] *)

val drop : int -> string -> string
(** [drop n s] returns [s] with first [n] characters removed.
    @raise Invalid_argument if [n] > length of [s] *)

val init : int -> string -> string
(** [init n s] returns [s] with last [n] characters removed.
    @raise Invalid_argument if [n] > length of [s] *)

val all : (char -> bool) -> string -> bool
(** [all p s] tests if all characters in [s] satisfy predicate [p] *)

val lines : string -> string list
(** Split string on newline characters *)

val words : string -> string list
(** Split string on whitespace (space, newline, tab) *)

val words_noempty : string -> string list
(** Like {!words} but filters out empty strings *)

val lines_noempty : string -> string list
(** Like {!lines} but filters out empty strings *)
