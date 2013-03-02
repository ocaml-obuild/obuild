open Ext.Fugue
open Ext.Filepath
open Ext
open Types
open Printf
open FindlibConf
open Dependencies

type predicate =
      Pred_Byte
    | Pred_Native
    | Pred_Toploop
    | Pred_CreateToploop
    | Pred_Plugin
    | Pred_Mt
    | Pred_Mt_vm
    | Pred_Mt_posix
    | Pred_Gprof
    | Pred_Autolink
    | Pred_Syntax
    | Pred_Preprocessor
    | Pred_Camlp4o
    | Pred_Camlp4r

exception LibraryNotFound of string
exception SubpackageNotFound of string
exception ArchiveNotFound of filepath * lib_name * (predicate list)
exception MetaParseError of filepath * string
exception MetaUnknownPredicate of string

(* mini lexer *)
type token =
      I of string
    | S of string
    | LPAREN
    | RPAREN
    | MINUS
    | DOT
    | EQ
    | PLUSEQ
    | COMMA

let predicate_to_string p =
    match p with
    | Pred_Byte -> "byte"
    | Pred_Native -> "native"
    | Pred_Toploop -> "toploop"
    | Pred_CreateToploop -> "create_toploop"
    | Pred_Plugin -> "plugin"
    | Pred_Mt -> "mt"
    | Pred_Mt_vm -> "mt_vm"
    | Pred_Mt_posix -> "mt_posix"
    | Pred_Gprof -> "gprof"
    | Pred_Autolink -> "autolink"
    | Pred_Syntax -> "syntax"
    | Pred_Preprocessor -> "preprocessor"
    | Pred_Camlp4o -> "camlp4o"
    | Pred_Camlp4r -> "camlp4r"

let predicate_of_string s =
    match s with
    | "byte"           -> Pred_Byte
    | "native"         -> Pred_Native
    | "toploop"        -> Pred_Toploop
    | "create_toploop" -> Pred_CreateToploop
    | "plugin"         -> Pred_Plugin
    | "mt"             -> Pred_Mt
    | "mt_vm"          -> Pred_Mt_vm
    | "mt_posix"       -> Pred_Mt_posix
    | "gprof"          -> Pred_Gprof
    | "autolink"       -> Pred_Autolink
    | "syntax"         -> Pred_Syntax
    | "preprocessor"   -> Pred_Preprocessor
    | "camlp4o"        -> Pred_Camlp4o
    | "camlp4r"        -> Pred_Camlp4r
    | _                -> raise (MetaUnknownPredicate s)

(* preliminaries structures, adjust as needed by meta. *)
type package = { package_name        : string
               ; package_requires    : (predicate list option * dep_name list) list
               ; package_directory   : string
               ; package_description : string
               ; package_exists_if   : string
               ; package_preprocessor : string
               ; package_browse_interface : string
               ; package_type_of_threads : string
               ; package_archives    : (predicate list * string) list
               ; package_version     : string
               ; package_assignment  : (string * string) list
               ; package_subs        : package list
               }

type meta = filepath * package

let newPkg name = { package_name             = name
                  ; package_requires         = []
                  ; package_directory        = ""
                  ; package_description      = ""
                  ; package_preprocessor     = ""
                  ; package_browse_interface = ""
                  ; package_type_of_threads  = ""
                  ; package_exists_if        = ""
                  ; package_archives         = []
                  ; package_version          = ""
                  ; package_assignment       = []
                  ; package_subs             = []
                  }

let meta_path_warning = ref false

let rec iterate f package = f package; List.iter (iterate f) package.package_subs

let rec find subs pkg =
    match subs with
    | []    -> pkg
    | x::xs -> find xs (try List.find (fun spkg -> spkg.package_name = x) pkg.package_subs
                        with Not_found -> raise (SubpackageNotFound x))

let showToken tok = match tok with
    | (I s)  -> "ID[" ^ s ^ "]"
	| (S s)  -> "\"" ^ s ^ "\""
	| LPAREN -> "("
	| RPAREN -> ")"
	| MINUS  -> "-"
    | DOT    -> "."
	| EQ     -> "="
	| PLUSEQ -> "+="
	| COMMA  -> ","

(* meta files are supposed to be small, so don't bother we
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
    let parse_requires_eq mpreds tokens =
        match tokens with
        | PLUSEQ :: S reqs :: xs
        | EQ :: S reqs :: xs ->
            let deps = List.map (fun r -> lib_name_of_string r)
                $ (List.filter (fun x -> x <> "") $ string_split_pred (fun c -> List.mem c [',';' ']) reqs)
                in
            ((mpreds, deps), xs)
        | _ ->
            metaFailed ("expecting '+=' or '=' after requires")
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
        | I "requires" :: xs -> (
            match xs with
            | LPAREN :: I s :: xs2 ->
                let (l,xs3) = parseCSVtail xs2 in
                (match xs3 with
                | RPAREN :: xs4 ->
                    let preds = List.map predicate_of_string (s::l) in
                    let (req, xs5) = parse_requires_eq (Some preds) xs4 in
                    parse { acc with package_requires = req :: acc.package_requires } xs5
                | _ -> metaFailed "expecting ) after requires's predicate"
                )
            | _ ->
                let (req, xs2) = parse_requires_eq None xs in
                parse { acc with package_requires = req :: acc.package_requires } xs2
            )
        | I "directory" :: EQ :: S dir :: xs ->
            parse { acc with package_directory = dir } xs
        | I "description" :: EQ :: S dir :: xs ->
            parse { acc with package_description = dir } xs
        | I "browse_interfaces" :: EQ :: S intf :: xs ->
            parse acc xs
        | I "archive" :: LPAREN :: I s :: xs ->
            (let (ss, xs2) = parseCSVtail xs in
            let preds = List.map predicate_of_string (s::ss) in
            match xs2 with
            | RPAREN :: PLUSEQ :: S v :: xs3
            | RPAREN :: EQ :: S v :: xs3 ->
                let nacc = { acc with package_archives = acc.package_archives @ [(preds, v)] } in
                parse nacc xs3
            | _ ->
                metaFailed "parsing archive failed"
            )
        | I "preprocessor" :: EQ :: S v :: xs ->
            parse { acc with package_preprocessor = v } xs
        | I "version" :: EQ :: S v :: xs ->
            parse { acc with package_version = v } xs
        | I "exists_if" :: EQ :: S v :: xs ->
            parse { acc with package_exists_if = v } xs
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
        | I "linkopts" :: EQ :: S _ :: xs ->
            parse acc xs
        | I stuff :: EQ :: S stuffVal :: xs ->
            parse { acc with package_assignment = (stuff, stuffVal) :: acc.package_assignment } xs
        | x :: xs ->
                metaFailed ("unknown token '" ^ showToken x ^ "' in meta file\n" ^ (String.concat " " (List.map showToken xs)) )
        in
    fst (parse (newPkg "") (lexer content))

let read path =
    let metaContent = Filesystem.readFile path in
    parse path metaContent

let write path package =
    let out = Buffer.create 1024 in
    let append = Buffer.add_string out in
    let rec write_one indent pkg =
        let indentStr = String.make indent ' ' in
        if pkg.package_description <> ""
            then append (sprintf "%sdescription = \"%s\"\n" indentStr pkg.package_description);
        if pkg.package_version <> ""
            then append (sprintf "%sversion = \"%s\"\n" indentStr pkg.package_version);
        if pkg.package_browse_interface <> ""
            then append (sprintf "%sbrowse_interfaces = \"%s\"\n" indentStr pkg.package_browse_interface);
        if pkg.package_exists_if <> ""
            then append (sprintf "%sexists_if = \"%s\"\n" indentStr pkg.package_exists_if);

        List.iter (fun (mpred,deps) ->
            let predStr =
                match mpred with
                | None -> ""
                | Some l -> "(" ^ String.concat "," (List.map predicate_to_string l) ^ ")"
                in
            let depStr = String.concat "," (List.map (fun dep -> lib_name_to_string dep) deps)
                in
            append (sprintf "%srequires%s = \"%s\"\n" indentStr predStr depStr);
        ) pkg.package_requires;

        List.iter (fun (csv,v) ->
            let k = String.concat "," (List.map predicate_to_string csv) in
            append (sprintf "%sarchive(%s) = \"%s\"\n" indentStr k v)
        ) pkg.package_archives;
        List.iter (fun spkg ->
            append (sprintf "%spackage \"%s\" (\n" indentStr spkg.package_name);
            write_one (indent+2) spkg;
            append (sprintf "%s)\n" indentStr)
        ) pkg.package_subs
        in

    write_one 0 package;
    Filesystem.writeFile path (Buffer.contents out)

(* get the META file path associated to a library *)
let findLibPath name =
    if !meta_path_warning then (
        eprintf "warning: obuild META search paths and ocaml config mismatch\n\n";
        eprintf "  The ocamlfind configuration file used doesn't list the ocaml standard library \n";
        eprintf "  as part of his search paths. something fishy is going on\n";
        eprintf "  You can solve the issue by:\n";
        eprintf "  * pointing OCAMLFIND_CONF environment to the right configuration file\n";
        eprintf "  * making sure that the ocamlfind program in your path is the right one (ocamlfind printconf)\n";
        eprintf "\n";
        eprintf "  this is likely to cause various compilation problems\n";
        (* then we ignore further warnings *)
        meta_path_warning := false
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

let findLib name : meta =
    let path = findLibPath name in
    (path, read path)

let getIncludeDir stdlib ((path, pkg) : meta) : filepath =
    match pkg.package_directory with
    | ""  -> path_dirname path
    | "^" -> path_dirname (path_dirname path)
    | o   -> match o.[0]  with
             | '^' -> path_dirname (path_dirname path) <//> fp (string_drop 1 o)
             | '+' -> stdlib <//> fp (string_drop 1 o)
             | _   -> fp o

let pkg_getSyntaxes pkg =
    list_filter_map (fun (preds,s) ->
        if List.mem Pred_Syntax preds
            then Some (list_remove Pred_Syntax preds, s)
            else None
    ) pkg.package_archives

let pkg_isSyntax pkg = List.length (pkg_getSyntaxes pkg) > 0

let isSyntax (path, rootPkg) dep = pkg_isSyntax (find dep.lib_subnames rootPkg)

let getArchiveWithFilter (path, rootPkg) dep pred =
    let pkg = find dep.lib_subnames rootPkg in
    List.find_all (fun (preds,a) -> List.mem pred preds) pkg.package_archives

let getArchive (path, rootPkg) dep csv =
    let pkg = find dep.lib_subnames rootPkg in
    try
        snd (List.find (fun (e,_) -> list_eq_noorder e csv) pkg.package_archives)
    with Not_found ->
        raise (ArchiveNotFound (path, dep, csv))
