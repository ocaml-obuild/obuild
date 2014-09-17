open Ext.Fugue
open Printf

exception UnknownSymbol of (string * string)
exception UnknownExpression of string
exception ExpressionEmpty
exception InvalidDependencyName of string
exception CannotParseContraints of (string * string)

type version = string

type token =
    | V of string (* version *)
    | I of string (* ident *)
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

let string_of_token tok =
    match tok with
    | V v    -> "V(" ^ v ^ ")"
    | I s    -> s
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

type expr =
      ExpAnd of expr * expr
    | ExpOr of expr * expr
    | ExpNot of expr
    | ExpParen of expr
    | ExpEq of version
    | ExpLe of version
    | ExpLt of version
    | ExpGe of version
    | ExpGt of version
    | ExpNe of version

let rec eval_expr version constr =
  match constr with
  | ExpAnd (e1,e2) -> (eval_expr version e1) && (eval_expr version e2)
  | ExpOr (e1,e2) -> (eval_expr version e1) || (eval_expr version e2)
  | ExpNot e -> not (eval_expr version e)
  | ExpParen e -> eval_expr version e
  | ExpEq v -> version = v
  | ExpLe v -> version <= v
  | ExpLt v -> version < v
  | ExpGe v -> version >= v
  | ExpGt v -> version > v
  | ExpNe v -> version != v

let rec expr_to_string = function
  | ExpAnd (e1,e2) -> (expr_to_string e1) ^ " && " ^ (expr_to_string e2)
  | ExpOr (e1,e2) -> (expr_to_string e1) ^ " || " ^ (expr_to_string e2)
  | ExpNot e -> "! " ^ (expr_to_string e)
  | ExpParen e -> "(" ^ (expr_to_string e) ^ ")"
  | ExpEq v -> "=" ^ v
  | ExpLe v -> "<=" ^ v
  | ExpLt v -> "<" ^ v
  | ExpGe v -> ">=" ^ v
  | ExpGt v -> ">" ^ v
  | ExpNe v -> "!=" ^ v

let lexer s =
    let len = String.length s in
    (* valid char per types *)
    let isSymbolChar c = try let _ = String.index "&/|!+=><()" c in true with _ -> false in
    let isIdentChar c =
        (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        (c >= '0' && c <= '9') ||
        c == '_' || c == '.' || c == '-'
        in
    let isVersionChar c = (c >= '0' && c <= '9') || c = '.' || c = '*'
        in

    let while_pred pred o =
        let i = ref o in
        while !i < len && pred s.[!i] do i := !i + 1 done;
        (String.sub s o (!i-o), !i)
        in

    (* Per type lexer *)
    let eat_symbol o =
      let c = s.[o] in
      if c = '(' || c = ')' then
        let tok = if c = '(' then LPAREN else RPAREN in
        (tok,o+1)
      else
        let (p, no) = while_pred isSymbolChar o in
        let tok =
            match p with
            | "&&" | "&"  -> AND
            | "||" | "|"  -> OR
            | ">"         -> GT
            | "<"         -> LT
            | ">="        -> GE
            | "<="        -> LE
            | "==" | "="  -> EQ
            | "!=" | "/=" -> NE
            | "!"         -> NOT
            | _           -> raise (UnknownSymbol (p, s))
            in
        (tok,no)
        in
    let eat_version o = while_pred isVersionChar o in
    let eat_ident o = while_pred isIdentChar o in

    (* main lexing loop *)
    let rec loop o =
        if o >= len
            then []
            else (
                (* TODO skip chunk of space in one go *)
                if s.[o] == ' ' || s.[o] == '\t' then (
                    loop (o+1)
                ) else if isSymbolChar s.[o] then (
                    let (sym, no) = eat_symbol o in sym :: loop no
                ) else if (s.[o] >= 'a' && s.[o] <= 'z') ||
                          (s.[o] >= 'A' && s.[o] <= 'Z') then (
                    let (id, no) = eat_ident o in I id :: loop no
                ) else if isVersionChar s.[o] then (
                    let (ver, no) = eat_version o in V ver :: loop no
                ) else
                    failwith (sprintf "unknown character in expression '%c'" s.[o])
            )
        in
    loop 0

let parse_builddep s =
    (* FIXME this is not complete. need to parse properly and/or and nesting *)
    let showList sep f l = String.concat sep (List.map f l) in
    let rec parse_expr l =
        match l with
          | LPAREN :: r -> parse_expr r
          | GT :: V v :: r -> (ExpGt v, r)
          | GE :: V v :: r -> (ExpGe v, r)
          | EQ :: V v :: r -> (ExpEq v, r)
          | LT :: V v :: r -> (ExpLt v, r)
          | LE :: V v :: r -> (ExpLe v, r)
          | NE :: V v :: r -> (ExpNe v, r)
          | z              -> raise (UnknownExpression (showList "," string_of_token z))
    in
    let parse_constraints l =
        match l with
        | []   -> None
        | expr -> let (e,_) = parse_expr expr in
                  Some e
        in
    match lexer s with
    | []                    -> raise ExpressionEmpty
    | I name :: constraints -> let constraints =
                                    try parse_constraints constraints
                                    with e ->
                                        let err =
                                            match e with
                                            | UnknownExpression z -> "unknown contraints expression \"" ^ z ^ "\""
                                            | _                   -> Printexc.to_string e
                                            in
                                        raise (CannotParseContraints (name,err))
                                    in
                               (name, constraints)
    | x      :: _           -> raise (InvalidDependencyName (string_of_token x))
