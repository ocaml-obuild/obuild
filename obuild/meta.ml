open Ext.Fugue
open Ext.Filepath
open Ext
open Types
open Printf
open FindlibConf
open Dependencies

module Predicate = struct
  type t =
    | Byte
    | Native
    | Toploop
    | CreateToploop
    | Plugin
    | Mt
    | Mt_vm
    | Mt_posix
    | Gprof
    | Autolink
    | Syntax
    | Preprocessor
    | Camlp4o
    | Camlp4r
    | Unknown of string

  let to_string = function
    | Byte -> "byte"
    | Native -> "native"
    | Toploop -> "toploop"
    | CreateToploop -> "create_toploop"
    | Plugin -> "plugin"
    | Mt -> "mt"
    | Mt_vm -> "mt_vm"
    | Mt_posix -> "mt_posix"
    | Gprof -> "gprof"
    | Autolink -> "autolink"
    | Syntax -> "syntax"
    | Preprocessor -> "preprocessor"
    | Camlp4o -> "camlp4o"
    | Camlp4r -> "camlp4r"
    | Unknown s -> s

  let of_string = function
    | "byte"           -> Byte
    | "native"         -> Native
    | "toploop"        -> Toploop
    | "create_toploop" -> CreateToploop
    | "plugin"         -> Plugin
    | "mt"             -> Mt
    | "mt_vm"          -> Mt_vm
    | "mt_posix"       -> Mt_posix
    | "gprof"          -> Gprof
    | "autolink"       -> Autolink
    | "syntax"         -> Syntax
    | "preprocessor"   -> Preprocessor
    | "camlp4o"        -> Camlp4o
    | "camlp4r"        -> Camlp4r
    | _ as s           -> Unknown s
end

exception LibraryNotFound of string
exception SubpackageNotFound of string
exception ArchiveNotFound of filepath * Libname.t * (Predicate.t list)
exception MetaParseError of filepath * string

module Pkg = struct
(* preliminaries structures, adjust as needed by meta. *)
  type t = {
    name        : string;
    requires    : (Predicate.t list option * dep_name list) list;
    directory   : string;
    description : string;
    exists_if   : string;
    preprocessor : string;
    browse_interface : string;
    type_of_threads : string;
    archives    : (Predicate.t list * string) list;
    version     : string;
    assignment  : (string * string) list;
    linkopts    : (Predicate.t list option * string) list;
    subs        : t list;
  }
  let make name = {
    name;
    requires         = [];
    directory        = "";
    description      = "";
    preprocessor     = "";
    linkopts         = [];
    browse_interface = "";
    type_of_threads  = "";
    exists_if        = "";
    archives         = [];
    version          = "";
    assignment       = [];
    subs             = [];
  }
  let rec iter f package = f package; List.iter (iter f) package.subs
  let rec find subs pkg =
    match subs with
    | []    -> pkg
    | x::xs -> find xs (try List.find (fun spkg -> spkg.name = x) pkg.subs
                        with Not_found -> raise (SubpackageNotFound x))
  let get_syntaxes pkg =
    list_filter_map (fun (preds,s) ->
        if List.mem Predicate.Syntax preds
        then Some (list_remove Predicate.Syntax preds, s)
        else None
      ) pkg.archives

  let is_syntax_ pkg = List.length (get_syntaxes pkg) > 0

  let is_syntax (path, rootPkg) dep = is_syntax_ (find dep.Libname.subnames rootPkg)

  let get_archive_with_filter (path, root) dep pred =
    let pkg = find dep.Libname.subnames root in
    List.find_all (fun (preds,_) -> List.mem pred preds && (not (List.mem Predicate.Toploop preds))) pkg.archives

  let get_archive (path, root) dep csv =
    let pkg = find dep.Libname.subnames root in
    try
      snd (List.find (fun (e,_) -> list_eq_noorder e csv) pkg.archives)
    with Not_found ->
      raise (ArchiveNotFound (path, dep, csv))

  let write path package =
    let out = Buffer.create 1024 in
    let append = Buffer.add_string out in
    let rec write_one indent pkg =
      let indent_str = String.make indent ' ' in
      let output_field field name =
        if field <> "" then
          append (sprintf "%s%s = \"%s\"\n" indent_str name field);
      in
      output_field pkg.description "description";
      output_field pkg.version "version";
      output_field pkg.browse_interface "browse_interface";
      output_field pkg.exists_if "exists_if";

      List.iter (fun (mpred,deps) ->
          let pred_str = match mpred with
            | None -> ""
            | Some l -> "(" ^ String.concat "," (List.map Predicate.to_string l) ^ ")"
          in
          let dep_str = String.concat "," (List.map (fun dep -> Libname.to_string dep) deps) in
          append (sprintf "%srequires%s = \"%s\"\n" indent_str pred_str dep_str);
        ) pkg.requires;

      List.iter (fun (csv,v) ->
          let k = String.concat "," (List.map Predicate.to_string csv) in
          append (sprintf "%sarchive(%s) = \"%s\"\n" indent_str k v)
        ) pkg.archives;
      List.iter (fun spkg ->
          append (sprintf "%spackage \"%s\" (\n" indent_str spkg.name);
          write_one (indent+2) spkg;
          append (sprintf "%s)\n" indent_str)
        ) pkg.subs
    in
    write_one 0 package;
    Filesystem.writeFile path (Buffer.contents out)

end

type t = filepath * Pkg.t

let path_warning = ref false

(* mini lexer *)
type token =
  | I of string
  | S of string
  | LPAREN
  | RPAREN
  | MINUS
  | DOT
  | EQ
  | PLUSEQ
  | COMMA

let showToken = function
  | (I s)  -> "ID[" ^ s ^ "]"
  | (S s)  -> "\"" ^ s ^ "\""
  | LPAREN -> "("
  | RPAREN -> ")"
  | MINUS  -> "-"
  | DOT    -> "."
  | EQ     -> "="
  | PLUSEQ -> "+="
  | COMMA  -> ","

(* meta files are supposed to be small, so don't bother with
 * a real efficient and incremental read/lex/parse routine.
 *
 * this can be improve later on-needed basis
 *)
let parse name content =
    let metaFailed s = raise (MetaParseError (name, s)) in 
    let simpleChar = hashtbl_fromList
            [ ('(', LPAREN)
            ; (')', RPAREN)
            ; ('=', EQ)
            ; (',', COMMA)
            ; ('.', DOT)
            ; ('-', MINUS)
            ]
        in
    let lexer s =
        let line = ref 1 in
        let lineoff = ref 0 in
        let len = String.length s in
        let isIdentChar c = char_is_alphanum c || c == '_' in
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
            let buf = Buffer.create 32 in
            let inEscape = ref false in
            while !i < len && (!inEscape || s.[!i] <> '"') do
                if not !inEscape && s.[!i] = '\\'
                    then inEscape := true
                    else (
                        let c =
                            if !inEscape
                                then
                                    match s.[!i] with
                                    | '\\' -> '\\'
                                    | 'n' -> '\n'
                                    | 't' -> '\t'
                                    | 'r' -> '\r'
                                    | '"' -> '"'
                                    | _   -> s.[!i]
                                else s.[!i]
                            in
                        inEscape := false;
                        Buffer.add_char buf c
                    );
                i := !i+1
            done;
            (Buffer.contents buf, !i+1)
            in
        let rec loop o =
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
                    ) else if s.[o] == '+' && (o+1) < len && s.[o+1] == '=' then (
                        PLUSEQ :: loop (o+2)
                    ) else if (s.[o] >= 'a' && s.[o] <= 'z') ||
                            (s.[o] >= 'A' && s.[o] <= 'Z') then (
                        let (id, no) = parseIdent o in I id :: loop no
                    ) else
                        metaFailed (sprintf "%d.%d: meta lexing error: undefined character '%c'" !line (o - !lineoff) s.[o])
                )
            in
        loop 0
        in
    let rec parseCSVtail tokens =
        match tokens with
        | COMMA :: I s :: xs -> let (l, r) = parseCSVtail xs in (s :: l, r)
        | xs                 -> ([], xs)
        in
    let parse_predicate_list field tokens =
        match tokens with
        | LPAREN :: RPAREN :: xs -> (Some [], xs)
        | LPAREN :: I s :: xs    ->
            (let (ss, xs2) = parseCSVtail xs in
            match xs2 with
            | RPAREN :: xs3 ->
                let preds = List.map Predicate.of_string (s::ss) in
                (Some preds, xs3)
            | _             -> metaFailed ("expecting ')' after " ^ field ^ "'s predicate")
            )
        | xs                     -> (None, xs)
        in
    let parse_requires_eq mpreds tokens =
        match tokens with
        | PLUSEQ :: S reqs :: xs
        | EQ :: S reqs :: xs ->
            let deps = List.map (fun r -> Libname.of_string r)
                $ (List.filter (fun x -> x <> "") $ string_split_pred (fun c -> List.mem c [',';' ']) reqs)
                in
            ((mpreds, (List.rev deps)), xs)
        | _ ->
            metaFailed ("expecting '+=' or '=' after requires")
        in
    let rec parse acc tokens =
        match tokens with
        | []           -> (acc, [])
        | RPAREN :: xs -> (acc, xs)
        | I "package" :: S name :: LPAREN :: xs ->
            (let (pkg, xs2) = parse (Pkg.make name) xs in
            let nacc = { acc with Pkg.subs = acc.Pkg.subs @ [pkg]} in
            parse nacc xs2
            )
        | I "requires" :: xs -> (
            match xs with
            | LPAREN :: I s :: xs2 ->
                let (l,xs3) = parseCSVtail xs2 in
                (match xs3 with
                | RPAREN :: xs4 ->
                    let preds = List.map Predicate.of_string (s::l) in
                    let (req, xs5) = parse_requires_eq (Some preds) xs4 in
                    parse { acc with Pkg.requires = req :: acc.Pkg.requires } xs5
                | _ -> metaFailed "expecting ) after requires's predicate"
                )
            | _ ->
                let (req, xs2) = parse_requires_eq None xs in
                parse { acc with Pkg.requires = req :: acc.Pkg.requires } xs2
            )
        | I "directory" :: EQ :: S dir :: xs ->
            parse { acc with Pkg.directory = dir } xs
        | I "description" :: EQ :: S dir :: xs ->
            parse { acc with Pkg.description = dir } xs
        | I "browse_interfaces" :: EQ :: S intf :: xs ->
            parse acc xs
        | I "archive" :: LPAREN :: I s :: xs ->
            (let (ss, xs2) = parseCSVtail xs in
            let preds = List.map Predicate.of_string (s::ss) in
            match xs2 with
            | RPAREN :: PLUSEQ :: S v :: xs3
            | RPAREN :: EQ :: S v :: xs3 ->
                let nacc = { acc with Pkg.archives = acc.Pkg.archives @ [(preds, v)] } in
                parse nacc xs3
            | _ ->
                metaFailed "parsing archive failed"
            )
        | I "preprocessor" :: EQ :: S v :: xs ->
            parse { acc with Pkg.preprocessor = v } xs
        | I "version" :: EQ :: S v :: xs ->
            parse { acc with Pkg.version = v } xs
        | I "exists_if" :: EQ :: S v :: xs ->
            parse { acc with Pkg.exists_if = v } xs
        | I "error" :: LPAREN :: xs -> (
            let rec consume toks =
                match toks with
                | RPAREN::zs -> zs
                | z::zs      -> consume zs
                | []         -> failwith "eof in error context"
                in
            match consume xs with
            | EQ :: S s :: xs2 -> parse acc xs2
            | _                -> failwith "parsing error failed"
            )
        | I "linkopts" :: xs -> (
            let (preds, xs2) = parse_predicate_list "linkopts" xs in
            match xs2 with
            | EQ :: S s :: xs3 ->
                    parse { acc with Pkg.linkopts = (preds, s) :: acc.Pkg.linkopts } xs3
            | _         -> failwith "parsing linkopts failed, expecting equal"
            )
        | I stuff :: EQ :: S stuffVal :: xs ->
            parse { acc with Pkg.assignment = (stuff, stuffVal) :: acc.Pkg.assignment } xs
        | x :: xs ->
                metaFailed ("unknown token '" ^ showToken x ^ "' in meta file\n" ^ (String.concat " " (List.map showToken xs)) )
        in
    fst (parse (Pkg.make "") (lexer content))

let read path =
    let metaContent = Filesystem.readFile path in
    parse path metaContent

(* get the META file path associated to a library *)
let findLibPath name =
    if !path_warning then (
        eprintf "warning: obuild META search paths and ocaml config mismatch\n\n";
        eprintf "  The ocamlfind configuration file used doesn't list the ocaml standard library \n";
        eprintf "  as part of his search paths. something fishy is going on\n";
        eprintf "  You can solve the issue by:\n";
        eprintf "  * pointing OCAMLFIND_CONF environment to the right configuration file\n";
        eprintf "  * making sure that the ocamlfind program in your path is the right one (ocamlfind printconf)\n";
        eprintf "\n";
        eprintf "  this is likely to cause various compilation problems\n";
        (* then we ignore further warnings *)
        path_warning := false
    );
    let rec find_ret l =
        match l with
        | []    -> raise (LibraryNotFound name)
        | p::ps ->
            let inDir = (p </> fn name) </> fn "META" in
            let asMetaext = p </> (fn ("META") <.> name) in
            if Filesystem.exists inDir then inDir
            else if Filesystem.exists asMetaext then asMetaext
            else find_ret ps
        in
    find_ret (FindlibConf.get_paths ())

let findLib name : t =
    let path = findLibPath name in
    (path, read path)

let getIncludeDir stdlib ((path, pkg) : t) : filepath =
    match pkg.Pkg.directory with
    | ""  -> path_dirname path
    | "^" -> path_dirname (path_dirname path)
    | o   -> match o.[0]  with
             | '^' -> path_dirname (path_dirname path) <//> fp (string_drop 1 o)
             | '+' -> stdlib <//> fp (string_drop 1 o)
             | _   -> fp o
