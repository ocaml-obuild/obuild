(** Project initialization wizard *)

exception ProjectAlreadyExists
(** Raised when a project file already exists in the current directory *)

exception CannotRunNotInteractive
(** Raised when trying to run the wizard in a non-interactive terminal *)

exception AbortedByUser
(** Raised when the user aborts the wizard (e.g., EOF on input) *)

val run : unit -> Project.t
(** [run ()] runs the interactive project initialization wizard.

    Prompts the user for project details and returns a configured Project.t.

    @raise ProjectAlreadyExists if a project file already exists
    @raise CannotRunNotInteractive if not running in an interactive terminal
    @raise AbortedByUser if the user aborts the wizard *)
