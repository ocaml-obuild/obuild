open Ext.Fugue

exception UnknownSymbol of (string * string)
exception UnknownExpression of string
exception ExpressionEmpty
exception UnbalancedParenthesis
exception MalformedExpression
exception InvalidDependencyName of string
exception CannotParseConstraints of (string * string)

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
          failwith (Printf.sprintf "unknown character in expression '%c'" s.[o])
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

let compare_version v1 v2 =
  let skip i p s e =
    let rec loop i = if i = e then i else if (p s.[i]) then loop (i + 1) else i
    in loop i
  in
  let split_version v =
    let (p1,rest) = match (string_split ':' v ~limit:2) with
        [ _ ] -> ("", v)
      | [ p1; rest] -> (p1, rest) in
    let (p1, p2, p3) = match (string_split '-' rest ~limit:2) with
        [ _ ] -> (p1, rest, "")
      | [ p2 ; p3 ] -> (p1, p2, p3) in
    (p1, p2, p3)
  in
  let compare_part p1 p2 =
    let l1 = String.length p1 in
    let l2 = String.length p2 in
    let is_digit = function | '0'..'9' -> true | _ -> false in
    let rec loop i1 i2 =
      let compare_numbers i1 i2 =
        let rec loop_numbers n1 n2 last =
          if n2 = last then loop n1 n2
          else
            let comp = Char.compare p1.[n1] p2.[n2] in
            if comp = 0 then loop_numbers (n1 + 1) (n2 + 1) last else comp
        in
        let end1 = skip i1 is_digit p1 l1 in
        let end2 = skip i2 is_digit p2 l2 in
        let comp = compare (end1 - i1) (end2 - i2) in
        if comp = 0 then loop_numbers i1 i2 end1 else comp
      in
      match (i1 = l1, i2 = l2) with
      | true,true -> 0
      | true,false -> let end2 = skip i2 (fun c -> c = '0') p2 l2 in
        if end2 = l2 then 0 else -1
      | false,true -> let end1 = skip i1 (fun c -> c = '0') p1 l1 in
        if end1 = l1 then 0 else 1
      | false,false -> match (is_digit p1.[i1], is_digit p2.[i2]) with
        | true,true ->
          compare_numbers (skip i1 (fun c -> c = '0') p1 l1) (skip i2 (fun c -> c = '0') p2 l2)
        | true,false -> -1
        | false,true -> 1
        | false,false -> let comp = Char.compare p1.[i1] p2.[i2] in
          if comp = 0 then loop (i1 + 1) (i2 + 1) else comp
    in
    loop 0 0
  in
  if v1 = v2 then 0
  else
    let (v1_1, v1_2, v1_3) = split_version v1 in
    let (v2_1, v2_2, v2_3) = split_version v2 in
    let c1 = compare_part v1_1 v2_1 in
    if c1 <> 0 then c1 else
      let c2 = compare_part v1_2 v2_2 in
      if c2 <> 0 then c2 else
        compare_part v1_3 v2_3

let rec eval version constr =
  match constr with
  | And (e1,e2) -> (eval version e1) && (eval version e2)
  | Or (e1,e2) -> (eval version e1) || (eval version e2)
  | Not e -> not (eval version e)
  | Paren e -> eval version e
  | Eq v -> compare_version version v = 0
  | Le v -> compare_version version v <= 0
  | Lt v -> compare_version version v < 0
  | Ge v -> compare_version version v >= 0
  | Gt v -> compare_version version v > 0
  | Ne v -> compare_version version v <> 0

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
      | UnknownExpression z -> "unknown constraints expression \"" ^ z ^ "\""
      | UnbalancedParenthesis -> "unbalanced parenthesis"
      | MalformedExpression -> "malformed expression"
      | _                   -> Printexc.to_string e
    in
    raise (CannotParseConstraints (name,err))

let parse name s =
  match Token.lexer s with
  | [] -> raise ExpressionEmpty
  | constraints -> parse_constraints name constraints

let parse_builddep s =
  match Token.lexer s with
  | []                    -> raise ExpressionEmpty
  | Token.ID name :: constraints -> (name, (parse_constraints name constraints))
  | x :: _           -> raise (InvalidDependencyName (Token.to_string x))
