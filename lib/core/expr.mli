(** Expression parsing and evaluation for version constraints

    This module provides parsing and evaluation of boolean expressions used
    for version constraints in dependencies (e.g., ">= 1.2.0 && < 2.0.0"). *)

(** {1 Exceptions} *)

exception UnknownSymbol of (string * string)
(** Raised when an unknown operator symbol is encountered *)

exception UnknownExpression of string
(** Raised when expression cannot be parsed *)

exception ExpressionEmpty
(** Raised when expression string is empty *)

exception UnbalancedParenthesis
(** Raised when parentheses don't match *)

exception MalformedExpression
(** Raised when expression has invalid structure *)

exception InvalidDependencyName of string
(** Raised when dependency name is invalid *)

exception CannotParseConstraints of (string * string)
(** Raised when constraint expression parsing fails.
    First string is the package name, second is the error message *)

(** {1 Types} *)

type version = string
(** Version string (e.g., "1.2.3", "4.02.0+beta1") *)

type t =
  | And of t * t         (** Conjunction: both constraints must hold *)
  | Or of t * t          (** Disjunction: either constraint can hold *)
  | Not of t             (** Negation: constraint must not hold *)
  | Paren of t           (** Parenthesized expression *)
  | Eq of version        (** Equality: version must equal specified version *)
  | Le of version        (** Less than or equal: version <= specified *)
  | Lt of version        (** Less than: version < specified *)
  | Ge of version        (** Greater than or equal: version >= specified *)
  | Gt of version        (** Greater than: version > specified *)
  | Ne of version        (** Not equal: version != specified *)
(** Version constraint expression AST *)

(** {1 Expression Operations} *)

val compare_version : version -> version -> int
(** [compare_version v1 v2] compares two version strings.
    Returns -1 if v1 < v2, 0 if v1 = v2, 1 if v1 > v2.
    Handles complex version formats including epoch, release, and patch parts *)

val eval : version -> t -> bool
(** [eval version constraint] evaluates whether a version satisfies a constraint.
    @param version The version to test
    @param constraint The constraint expression
    @return true if version satisfies the constraint *)

val to_string : t -> string
(** [to_string expr] converts constraint expression to string representation *)

(** {1 Parsing} *)

val parse : string -> string -> t option
(** [parse name expr_str] parses a version constraint expression.
    @param name Package name (for error messages)
    @param expr_str The constraint expression string
    @return [Some constraint] if parsing succeeds, [None] if expression is empty
    @raise CannotParseConstraints if parsing fails *)

val parse_builddep : string -> (string * t option)
(** [parse_builddep dep_str] parses a build dependency with optional constraints.
    Format: "package_name constraint_expr"
    Example: "unix >= 4.02.0"
    @return Tuple of (package_name, optional_constraint)
    @raise InvalidDependencyName if package name is invalid
    @raise CannotParseConstraints if constraint expression is invalid *)
