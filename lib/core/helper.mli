(** Helper functions for logging and output *)

val print_warnings : string -> unit
(** Print warnings to stderr if non-empty *)

val log : Gconf.verbosity_t -> ('a, out_channel, unit, unit) format4 -> 'a
(** Log message if verbosity level is sufficient *)

val debug : ('a, out_channel, unit, unit) format4 -> 'a
(** Log debug message *)

val report : ('a, out_channel, unit, unit) format4 -> 'a
(** Log report message *)

val verbose : Gconf.verbosity_t -> ('a, out_channel, unit, unit) format4 -> 'a
(** Log message with verbosity level (deprecated, use log instead) *)

val support_color : unit -> bool
(** Check if terminal supports color output *)

val color_red : unit -> string
(** Get ANSI red color escape sequence (or empty if no color) *)

val color_green : unit -> string
(** Get ANSI green color escape sequence (or empty if no color) *)

val color_blue : unit -> string
(** Get ANSI blue color escape sequence (or empty if no color) *)

val color_white : unit -> string
(** Get ANSI white/reset color escape sequence (or empty if no color) *)

(** Performance timing utilities *)
module Timing : sig
  val measure_time : string -> (unit -> 'a) -> 'a
  (** [measure_time name f] executes [f ()] and prints timing info at DebugPlus level *)

  val measure_time_verbose : string -> (unit -> 'a) -> 'a
  (** [measure_time_verbose name f] executes [f ()] with start/end messages at Debug level *)
end
