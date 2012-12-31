open Ext
open Types
open Printf

(* mini lexer *)
type token =
      I of string
    | S of string
    | LPAREN
    | RPAREN
    | MINUS
    | EQ
    | COMMA

type csv = string list

(* preliminaries structures, adjust as needed by meta. *)
type package = { package_name        : string
               ; package_requires    : dep_name list
               ; package_directory   : string
               ; package_description : string
               ; package_exists_if   : string
               ; package_archive     : (csv * string) list
               ; package_version     : string
               ; package_subs        : package list
               }

type meta = package

let rec iterate f package = f package; List.iter (iterate f) package.package_subs

let rec find subs pkg =
    match subs with
    | []    -> pkg
    | x::xs -> find xs (List.find (fun spkg -> spkg.package_name = x) pkg.package_subs)


let showToken tok = match tok with
    | (I s)  -> s
	| (S s)  -> "\"" ^ s ^ "\""
	| LPAREN -> "("
	| RPAREN -> ")"
	| MINUS  -> "-"
	| EQ     -> "="
	| COMMA  -> ","

(* meta files are supposed to be small, so don't bother we
 * a real efficient and incremental read/lex/parse routine.
 *
 * this can be improve later on-needed basis
 *)
let parseFile file =
    let simpleChar = hashtbl_fromList
            [ ('(', LPAREN)
            ; (')', RPAREN)
            ; ('=', EQ)
            ; (',', COMMA)
            ; ('-', MINUS)
            ]
        in
    let lexer s =
        let line = ref 1 in
        let lineoff = ref 0 in
        let len = String.length s in
        let isIdentChar c =
            (c >= 'a' && c <= 'z') ||
            (c >= 'A' && c <= 'Z') ||
            (c >= '0' && c <= '9') ||
            c == '_'
            in
        let eatComment o =
            let i = ref (o+1) in
            while !i < len && s.[!i] <> '\n' do i := !i+1 done;
            line := !line + 1;
            lineoff := !i+1;
            (!i+1)
            in
        let parseIdent o =
            let i = ref (o+1) in
            while !i < len && isIdentChar s.[!i] do i := !i+1 done;
            (String.sub s o (!i-o),!i)
            in
        let parseString o =
            let i = ref (o+1) in
            while !i < len && s.[!i] <> '"' do i := !i+1 done;
            (String.sub s (o+1) (!i-(o+1)),!i+1)
            in
        let rec loop o =
            (*printf "%d: %c\n" o (s.[o]);*)
            if o >= len
                then []
                else (
                    if s.[o] == ' ' || s.[o] == '\t' then (
                        loop (o+1)
                    ) else if s.[o] == '\n' then (
                        line := !line + 1; lineoff := o+1; loop (o+1)
                    ) else if s.[o] == '#' then (
                        loop (eatComment o)
                    ) else if s.[o] == '"' then (
                        let (s, no) = parseString o in S s :: loop no
                    ) else if Hashtbl.mem simpleChar s.[o] then (
                        Hashtbl.find simpleChar s.[o] :: loop (o+1)
                    ) else if (s.[o] >= 'a' && s.[o] <= 'z') ||
                            (s.[o] >= 'A' && s.[o] <= 'Z') then (
                         let (id, no) = parseIdent o in I id :: loop no
                    ) else
                        failwith (sprintf "%d.%d: unknown character '%c'" !line (o - !lineoff) s.[o])
                )
            in
        loop 0
        in
    let newPkg name = { package_name        = name
                      ; package_requires    = []
                      ; package_directory   = ""
                      ; package_description = ""
                      ; package_exists_if   = ""
                      ; package_archive     = []
                      ; package_version     = ""
                      ; package_subs        = []
                      }
                      in
    let rec parseCSVtail tokens =
        match tokens with
        | COMMA :: I s :: xs -> let (l, r) = parseCSVtail xs in (s :: l, r)
        | xs                 -> ([], xs)
        in
    let rec parse acc tokens =
        match tokens with
        | []           -> (acc, [])
        | RPAREN :: xs -> (acc, xs)
        | I "package" :: S name :: LPAREN :: xs ->
            (let (pkg, xs2) = parse (newPkg name) xs in
            let nacc = { acc with package_subs = acc.package_subs @ [pkg]} in
            parse nacc xs2
            )
        | I "requires" :: EQ :: S reqs :: xs ->
            let deps = List.map (fun r ->
                match string_split '.' r with
                | k::l -> { dep_name = k; dep_subname = l }
                | []   -> failwith ("bad dependency in meta file: " ^ r)
                ) $ (List.filter (fun x -> x <> "") $ string_split_pred (fun c -> List.mem c [',';' ']) reqs)
                in
            parse { acc with package_requires = deps } xs
        | I "directory" :: EQ :: S dir :: xs ->
            parse { acc with package_directory = dir } xs
        | I "description" :: EQ :: S dir :: xs ->
            parse { acc with package_description = dir } xs
        | I "browse_interfaces" :: EQ :: S intf :: xs ->
            parse acc xs
        | I "archive" :: LPAREN :: I s :: xs ->
            (let (ss, xs2) = parseCSVtail xs in
            match xs2 with
            | RPAREN :: EQ :: S v :: xs3 ->
                let nacc = { acc with package_archive = acc.package_archive @ [(s::ss, v)] } in
                parse nacc xs3
            | _ ->
                failwith "parsing archive failed"
            )
        | I "version" :: EQ :: S v :: xs ->
            parse { acc with package_version = v } xs
        | I "exists_if" :: EQ :: S v :: xs ->
            parse { acc with package_exists_if = v } xs
        | I "error" :: LPAREN :: xs ->
            let rec consume z =
                match z with
                | RPAREN::zs -> zs
                | _::zs      -> consume zs
                | []         -> failwith "eof in error context"
                in
            (match consume xs with
            | EQ :: S s :: xs2 -> parse acc xs
            | _                -> failwith "parsing error failed"
            )
                
        | I "linkopts" :: EQ :: S _ :: xs ->
            parse acc xs
        | x :: xs ->
                failwith ("unknown token '" ^ showToken x ^ "' in meta file")
        in
    let metaContent = Filesystem.readFile file in
    fst (parse (newPkg "") (lexer metaContent))
