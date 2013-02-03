open Ext
open Printf

exception UnknownSymbol of (string * string)

type version = string

type token =
      V of string (* version *)
    | B of bool   (* boolean *)
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
    | COMMA

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

let parse s =
    let len = String.length s in
    let lexer s =
        (* valid char per types *)
        let isSymbolChar c = try let _ = String.index "&|!+=><" c in true with _ -> false in
        let isIdentChar c =
            (c >= 'a' && c <= 'z') ||
            (c >= 'A' && c <= 'Z') ||
            (c >= '0' && c <= '9') ||
            c == '_'
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
            let (p, no) = while_pred isSymbolChar o in
            let tok =
                match p with
                | "&&" | "&" -> AND
                | "||" | "|" -> OR
                | ">"        -> GT
                | "<"        -> LT
                | ">="       -> GE
                | "<="       -> LE
                | "==" | "=" -> EQ
                | "!"        -> NOT
                | _          -> raise (UnknownSymbol (p, s))
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
        in
    let _ = lexer s in
    ()
