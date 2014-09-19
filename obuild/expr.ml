open Ext.Fugue
open Printf

exception UnknownSymbol of (string * string)
exception UnknownExpression of string
exception ExpressionEmpty
exception UnbalancedParenthesis
exception MalformedExpression
exception InvalidDependencyName of string
exception CannotParseContraints of (string * string)

type version = string

module Token = struct
  type t =
    | VER of string (* version *)
    | ID of string (* ident *)
    | LPAREN
    | RPAREN
    | AND
    | OR
    | NOT
    | EQ
    | NE
    | GT
    | LT
    | GE
    | LE

  let to_string = function
    | VER v  -> v
    | ID s   -> s
    | LPAREN -> "("
    | RPAREN -> ")"
    | AND    -> "&"
    | OR     -> "|"
    | NOT    -> "!"
    | EQ     -> "=="
    | NE     -> "!="
    | GT     -> ">"
    | LT     -> "<"
    | GE     -> ">="
    | LE     -> "<="

  let of_string symbol s = match symbol with
    | "&&" | "&"  -> AND
    | "||" | "|"  -> OR
    | ">"         -> GT
    | "<"         -> LT
    | ">="        -> GE
    | "<="        -> LE
    | "==" | "="  -> EQ
    | "!=" | "/=" -> NE
    | "!"         -> NOT
    | _           -> raise (UnknownSymbol (symbol,s))

  let process_one_char c next =
    match (c,next) with
    | '(', _ -> LPAREN
    | ')', _ -> RPAREN
    | '!', Some '=' -> raise Not_found (* should be parsed as a string != *)
    | '!', _ -> NOT
    | _ -> raise Not_found

  (* valid char per types *)
  let is_symbol_char c = try let _ = String.index "&/|!+=><()" c in true with _ -> false
  let is_ident_char c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
                        (c >= '0' && c <= '9') || c == '_' || c == '.' || c == '-'
  let is_version_char c = (c >= '0' && c <= '9') || c = '.' || c = '*'

  let lexer s =
    let len = String.length s in
    let while_pred pred o =
      let i = ref o in
      while !i < len && pred s.[!i] do i := !i + 1 done;
      (String.sub s o (!i-o), !i)
    in
    (* Per type lexer *)
    let eat_symbol o =
      let (tok,no) =
        let next = if o+1 < len then Some (s.[o+1]) else None in
        try let tok = process_one_char s.[o] next in (tok,o+1)
        with Not_found ->
          let (p, no) = while_pred is_symbol_char o in
          let tok = of_string p s in
          (tok,no)
      in (tok,no)
    in
    let eat_version o = while_pred is_version_char o in
    let eat_ident o = while_pred is_ident_char o in
    (* main lexing loop *)
    let rec loop o =
      if o >= len then []
      else begin
        (* TODO skip chunk of space in one go *)
        if s.[o] == ' ' || s.[o] == '\t' then (
          loop (o+1)
        ) else if is_symbol_char s.[o] then (
          let (sym, no) = eat_symbol o in sym :: loop no
        ) else if (s.[o] >= 'a' && s.[o] <= 'z') ||
                  (s.[o] >= 'A' && s.[o] <= 'Z') then (
          let (id, no) = eat_ident o in ID id :: loop no
        ) else if is_version_char s.[o] then (
          let (ver, no) = eat_version o in VER ver :: loop no
        ) else
          failwith (sprintf "unknown character in expression '%c'" s.[o])
      end
    in
    loop 0
end

type t =
  | And of t * t
  | Or of t * t
  | Not of t
  | Paren of t
  | Eq of version
  | Le of version
  | Lt of version
  | Ge of version
  | Gt of version
  | Ne of version

let rec eval version constr =
  match constr with
  | And (e1,e2) -> (eval version e1) && (eval version e2)
  | Or (e1,e2) -> (eval version e1) || (eval version e2)
  | Not e -> not (eval version e)
  | Paren e -> eval version e
  | Eq v -> version = v
  | Le v -> version <= v
  | Lt v -> version < v
  | Ge v -> version >= v
  | Gt v -> version > v
  | Ne v -> version <> v

let rec to_string = function
  | And (e1,e2) -> (to_string e1) ^ " && " ^ (to_string e2)
  | Or (e1,e2) -> (to_string e1) ^ " || " ^ (to_string e2)
  | Not e -> "! " ^ (to_string e)
  | Paren e -> "(" ^ (to_string e) ^ ")"
  | Eq v -> "=" ^ v
  | Le v -> "<=" ^ v
  | Lt v -> "<" ^ v
  | Ge v -> ">=" ^ v
  | Gt v -> ">" ^ v
  | Ne v -> "!=" ^ v

let showList sep f l = String.concat sep (List.map f l)

let parse_expr l =
  let rec parse_sub_expr l =
    match l with
    | [] -> raise MalformedExpression
    | Token.NOT :: r ->
      let (e, r) = parse_sub_expr r in ((Not e), r)
    | Token.LPAREN :: r ->
      let (e, r) = parse_sub_expr r in
      let rec loop e r =
        (match r with
         | Token.RPAREN :: r -> (Paren e, r)
         | Token.OR :: _ | Token.AND :: _ ->
           let (e, r) = parse_bin_expr e r in
           loop e r
         | _ -> raise UnbalancedParenthesis;
        )
      in
      loop e r
    | Token.GT :: Token.VER v :: r -> (Gt v, r)
    | Token.GE :: Token.VER v :: r -> (Ge v, r)
    | Token.EQ :: Token.VER v :: r -> (Eq v, r)
    | Token.LT :: Token.VER v :: r -> (Lt v, r)
    | Token.LE :: Token.VER v :: r -> (Le v, r)
    | Token.NE :: Token.VER v :: r -> (Ne v, r)
    | z              -> raise (UnknownExpression (showList "," Token.to_string z))
  and parse_bin_expr expr l =
    match l with
    | Token.OR :: r -> let (e, r) = parse_sub_expr r in ((Or (expr,e)), r)
    | Token.AND :: r -> let (e, r) = parse_sub_expr r in ((And (expr,e)), r)
    | _ -> raise MalformedExpression
  in
  let (e, r) = parse_sub_expr l in
  let rec loop e r =
    if(List.length r) = 0 then e
    else let (e,r) = parse_bin_expr e r in
      loop e r
  in
  loop e r

let parse_constraints name cs =
  try
    match cs with
    | []   -> None
    | expr -> let e = parse_expr expr in
      Some e
  with e ->
    let err =
      match e with
      | UnknownExpression z -> "unknown contraints expression \"" ^ z ^ "\""
      | UnbalancedParenthesis -> "unbalanced parenthesis"
      | MalformedExpression -> "malformed expression"
      | _                   -> Printexc.to_string e
    in
    raise (CannotParseContraints (name,err))

let parse name s =
  match Token.lexer s with
  | [] -> raise ExpressionEmpty
  | constraints -> parse_constraints name constraints

let parse_builddep s =
  match Token.lexer s with
  | []                    -> raise ExpressionEmpty
  | Token.ID name :: constraints -> (name, (parse_constraints name constraints))
  | x      :: _           -> raise (InvalidDependencyName (Token.to_string x))
