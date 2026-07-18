(* Content-digest based staleness tracking.
 *
 * For each build artifact (dest) we record the digest of every input it was
 * built from.  An artifact is stale when an input's current digest differs
 * from the recorded one, or when the input set itself changed (e.g. a module
 * was removed from a link).  This is immune to the mtime failure modes:
 * saves during a build, equal-mtime ties on coarse filesystems, and clock
 * skew.  It also gives early cutoff: rewriting a byte-identical .cmi does
 * not invalidate dependents.
 *
 * Digests of input files are themselves cached against mtime; a cached
 * digest is only trusted when the file's mtime is older than the time the
 * hash was taken by a safety margin, so files modified around hashing time
 * are re-hashed.
 *
 * Records are written to <dist>/digests.  Records for artifacts about to be
 * rebuilt are snapshotted before the compile runs (record_pending) and only
 * committed once the task's processes all succeed (commit_group), so a
 * failed compile never marks its output fresh.  When no record exists,
 * callers fall back to mtime comparison, so a pre-existing dist upgrades
 * incrementally without a full rebuild. *)

open Filepath

type check_result =
  | Unchanged
  | Changed of filepath
  | InputSetChanged
  | NoRecord

type group = string list

(* input digest cache: path -> (mtime, hash_time, digest) *)
let stat_cache : (string, float * float * Digest.t) Hashtbl.t = Hashtbl.create 256

(* committed records: dest -> (input path, digest) list *)
let records : (string, (string * Digest.t) list) Hashtbl.t = Hashtbl.create 256

(* snapshots taken before a compile, not yet validated by task success *)
let pending : (string, (string * Digest.t) list) Hashtbl.t = Hashtbl.create 64

(* dests snapshotted since the last take_group *)
let current_group : string list ref = ref []
let loaded = ref false
let dirty = ref false

(* don't trust a cached digest when the file mtime is within this margin of
 * the hashing time: the file may have changed within mtime granularity *)
let mtime_margin = 2.0

let db_path () = Dist.get_path () </> fn "digests"

let find_opt tbl k = try Some (Hashtbl.find tbl k) with Not_found -> None

(* hex encoding done locally: Digest.to_hex/from_hex only exist on OCaml >= 4.00
   and obuild supports 3.x *)
let digest_to_hex (d : Digest.t) =
  let b = Buffer.create 32 in
  String.iter (fun c -> Buffer.add_string b (Printf.sprintf "%02x" (Char.code c))) (d :> string);
  Buffer.contents b

let digest_of_hex s : Digest.t =
  let v c =
    match c with
    | '0' .. '9' -> Char.code c - Char.code '0'
    | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
    | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
    | _ -> raise (Invalid_argument "digest_of_hex")
  in
  let n = String.length s / 2 in
  let b = Buffer.create n in
  for i = 0 to n - 1 do
    Buffer.add_char b (Char.chr ((v s.[2 * i] * 16) + v s.[(2 * i) + 1]))
  done;
  Buffer.contents b


let file_digest path =
  let s = fp_to_string path in
  let mtime = Filesystem.get_modification_time path in
  match find_opt stat_cache s with
  | Some (m, ht, d) when m = mtime && mtime < ht -. mtime_margin -> d
  | _ ->
      let d = Digest.file s in
      Hashtbl.replace stat_cache s (mtime, Unix.gettimeofday (), d);
      dirty := true;
      d

let check dest srcs =
  match find_opt records (fp_to_string dest) with
  | None -> NoRecord
  | Some recorded ->
      let cur = List.map fp_to_string srcs in
      if List.sort compare cur <> List.sort compare (List.map fst recorded) then
        InputSetChanged
      else (
        try
          let changed, _ =
            List.find (fun (src, dg) -> try file_digest (fp src) <> dg with _ -> true) recorded
          in
          Changed (fp changed)
        with Not_found -> Unchanged)

let snapshot srcs =
  List.fold_left
    (fun acc src -> try (fp_to_string src, file_digest src) :: acc with _ -> acc)
    [] srcs

let record_pending dest srcs =
  let d = fp_to_string dest in
  Hashtbl.replace pending d (snapshot srcs);
  current_group := d :: !current_group

let record_now dest srcs =
  Hashtbl.replace records (fp_to_string dest) (snapshot srcs);
  dirty := true

let begin_group () = current_group := []

let take_group () =
  let g = !current_group in
  current_group := [];
  g

let commit_group g =
  List.iter
    (fun d ->
      match find_opt pending d with
      | Some snap ->
          Hashtbl.replace records d snap;
          Hashtbl.remove pending d;
          dirty := true
      | None -> ())
    g

(* storage format, one entry per line:
 *   F <mtime> <hash_time> <hex_digest> <path>     stat cache entry
 *   D <path>                                      start of a dest record
 *   I <hex_digest> <path>                         input of the current dest
 * paths may contain spaces (they extend to end of line) but not newlines *)

let split_first s =
  try
    let i = String.index s ' ' in
    (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
  with Not_found -> (s, "")

let load () =
  if not !loaded then begin
    loaded := true;
    let path = db_path () in
    if Filesystem.exists path then
      try
        let content = Filesystem.read_file path in
        let cur_dest = ref None in
        let flush_dest inputs =
          match !cur_dest with
          | Some d -> Hashtbl.replace records d (List.rev inputs)
          | None -> ()
        in
        let inputs = ref [] in
        List.iter
          (fun line ->
            if String.length line > 2 then
              let rest = String.sub line 2 (String.length line - 2) in
              match line.[0] with
              | 'F' ->
                  let mt, rest = split_first rest in
                  let ht, rest = split_first rest in
                  let hex, p = split_first rest in
                  Hashtbl.replace stat_cache p
                    (float_of_string mt, float_of_string ht, digest_of_hex hex)
              | 'D' ->
                  flush_dest !inputs;
                  inputs := [];
                  cur_dest := Some rest
              | 'I' ->
                  let hex, p = split_first rest in
                  inputs := (p, digest_of_hex hex) :: !inputs
              | _ -> ())
          (String_utils.lines_noempty content);
        flush_dest !inputs
      with _ ->
        (* unreadable or corrupted: start fresh, callers fall back to mtimes *)
        Hashtbl.clear stat_cache;
        Hashtbl.clear records
  end

let save () =
  if !dirty then begin
    let buf = Buffer.create 4096 in
    Hashtbl.iter
      (fun p (mt, ht, d) ->
        Buffer.add_string buf
          (Printf.sprintf "F %.17g %.17g %s %s\n" mt ht (digest_to_hex d) p))
      stat_cache;
    Hashtbl.iter
      (fun dest inputs ->
        Buffer.add_string buf ("D " ^ dest ^ "\n");
        List.iter
          (fun (p, d) -> Buffer.add_string buf ("I " ^ digest_to_hex d ^ " " ^ p ^ "\n"))
          inputs)
      records;
    Filesystem.write_file (db_path ()) (Buffer.contents buf);
    dirty := false
  end
