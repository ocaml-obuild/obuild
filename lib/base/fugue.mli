(** Utility functions for functional programming

    This module provides common functional programming utilities including option handling, string
    operations, list utilities, and more. *)

(** {1 Control flow} *)

val finally : (unit -> 'a) -> (unit -> unit) -> 'a
(** [finally fct clean_f] executes [fct ()], then executes [clean_f ()] regardless of whether [fct]
    succeeded or raised an exception. If [fct] raises an exception, [clean_f] is called before
    re-raising. *)

(** {1 Option utilities} *)

val maybe : 'b -> ('a -> 'b) -> 'a option -> 'b
(** [maybe default f opt] returns [f x] if [opt] is [Some x], otherwise [default] *)

val default : 'a -> 'a option -> 'a
(** [default d opt] returns [x] if [opt] is [Some x], otherwise [d] *)

val maybe_unit : ('a -> unit) -> 'a option -> unit
(** [maybe_unit f opt] executes [f x] if [opt] is [Some x], otherwise does nothing *)

val const : 'a -> 'b -> 'a
(** [const v _] always returns [v], ignoring the second argument *)

val maybes_to_list : 'a option list -> 'a list
(** [maybes_to_list opts] filters out [None] values and extracts values from [Some] *)

(** {1 Either type} *)

type ('a, 'b) either =
  | Left of 'a  (** Left alternative *)
  | Right of 'b  (** Right alternative *)

(** {1 Function composition} *)

val ( $ ) : ('a -> 'b) -> 'a -> 'b
(** Function application operator. [f $ x] is equivalent to [f x] but with lower precedence, useful
    for avoiding parentheses *)

val id : 'a -> 'a
(** Identity function *)

(** {1 Character utilities} *)

val char_is_alphanum : char -> bool
(** Test if character is alphanumeric (a-z, A-Z, or 0-9) *)

(** {1 List utilities} *)

val no_empty : 'a -> 'a list -> 'a list
(** [no_empty emptyVal lst] filters out all occurrences of [emptyVal] from [lst] *)

val list_init : 'a list -> 'a list
(** [list_init lst] returns all elements except the last.
    @raise Failure if list is empty *)

val list_last : 'a list -> 'a
(** [list_last lst] returns the last element.
    @raise Failure if list is empty *)

val list_remove : 'a -> 'a list -> 'a list
(** [list_remove e lst] filters out all occurrences of [e] from [lst] *)

val list_iteri : (int -> 'a -> unit) -> 'a list -> unit
(** [list_iteri f lst] applies [f] to each element with its index (starting at 1) *)

val list_eq_noorder : 'a list -> 'a list -> bool
(** [list_eq_noorder l1 l2] tests if all elements of [l1] are in [l2], ignoring order (not
    bidirectional - only checks l1 ⊆ l2) *)

val list_filter_map : ('a -> 'b option) -> 'a list -> 'b list
(** [list_filter_map f lst] applies [f] to each element, keeping only [Some] results *)

val list_uniq : 'a list -> 'a list
(** Remove duplicate elements from list *)

val list_find_map : ('a -> 'b option) -> 'a list -> 'b
(** [list_find_map p lst] returns the first [Some v] result of applying [p].
    @raise Not_found if no element produces [Some] *)

(** {1 Hashtable utilities} *)

val hashtbl_map : ('a -> 'b) -> ('c, 'a) Hashtbl.t -> ('c, 'b) Hashtbl.t
(** [hashtbl_map f h] creates a new hashtable with [f] applied to all values *)

val hashtbl_keys : ('a, 'b) Hashtbl.t -> 'a list
(** Get list of all keys in hashtable *)

val hashtbl_modify_all : ('a -> 'a) -> ('b, 'a) Hashtbl.t -> unit
(** [hashtbl_modify_all f h] applies [f] to all values in [h] *)

val hashtbl_from_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t
(** Create hashtable from association list *)

val hashtbl_to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
(** Convert hashtable to association list *)

(** {1 Tuple utilities} *)

val first : ('a -> 'c) -> 'a * 'b -> 'c * 'b
(** [first f (a, b)] returns [(f a, b)] *)

val second : ('b -> 'c) -> 'a * 'b -> 'a * 'c
(** [second f (a, b)] returns [(a, f b)] *)

(** {1 Conversion exceptions} *)

exception ConversionIntFailed of string * string
(** Raised when integer conversion fails. Contains (location, input) *)

exception ConversionBoolFailed of string * string
(** Raised when boolean conversion fails. Contains (location, input) *)

val user_int_of_string : string -> string -> int
(** [user_int_of_string loc s] converts [s] to int.
    @raise ConversionIntFailed with [loc] if conversion fails *)

val user_bool_of_string : string -> string -> bool
(** [user_bool_of_string loc s] converts [s] to bool.
    @raise ConversionBoolFailed with [loc] if conversion fails *)

(** {1 String set} *)

module StringSet : sig
  include Set.S with type elt = string

  val to_list : t -> string list
  (** Convert set to list *)
end
