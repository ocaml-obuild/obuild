(** Content-digest based staleness tracking.

    Records, for each build artifact, the digest of every input it was built
    from.  Staleness checks compare current input digests against the record,
    which is immune to mtime races (saves during a build, equal-mtime ties)
    and provides early cutoff when an input is rewritten with identical
    content.  Records persist in [<dist>/digests]. *)

(** Result of checking an artifact against its recorded inputs *)
type check_result =
  | Unchanged                      (** all inputs match the recorded digests *)
  | Changed of Filepath.filepath   (** this input's content changed *)
  | InputSetChanged                (** the set of inputs differs from the record *)
  | NoRecord                       (** no record: caller should fall back to mtimes *)

(** Dests snapshotted during one scheduler dispatch; committed on task success *)
type group

val load : unit -> unit
(** Load persisted records from [<dist>/digests]. Idempotent. *)

val save : unit -> unit
(** Persist records if anything changed since the last save. *)

val file_digest : Filepath.filepath -> Digest.t
(** Content digest of a file, mtime-cached across runs (persisted in the
    stat cache); virtual inputs are served from [set_virtual]. *)

val is_virtual : Filepath.filepath -> bool
(** True for synthetic input paths registered via [set_virtual] — they
    never exist on disk, so mtime fallbacks must skip them. *)

val set_virtual : string -> Digest.t -> unit
(** [set_virtual key d] registers an in-memory digest for the synthetic
    input path [key] (a path that never exists on disk).  Lets a large
    shared input set be digested once per run and referenced as a single
    entry by many records. *)

val check : Filepath.filepath -> Filepath.filepath list -> check_result
(** [check dest srcs] compares current digests of [srcs] against the record
    for [dest]. *)

val record_pending : Filepath.filepath -> Filepath.filepath list -> unit
(** [record_pending dest srcs] snapshots digests of [srcs] now, before the
    rebuild of [dest] runs; committed only by [commit_group]. *)

val record_now : Filepath.filepath -> Filepath.filepath list -> unit
(** [record_now dest srcs] records immediately, for artifacts already known
    valid (mtime-checked) that have no record yet. *)

val begin_group : unit -> unit
(** Start collecting pending dests for the task being dispatched. *)

val take_group : unit -> group
(** Return (and clear) the dests snapshotted since [begin_group]. *)

val commit_group : ?warnings:string -> group -> unit
(** Commit the pending snapshots of a group after its task succeeded.
    [warnings] is the task's accumulated compiler warning text; recorded
    with each dest (cleared when empty) so later fresh skips can replay it. *)

val take_replay : Filepath.filepath -> string option
(** Warnings recorded for [dest] by its last real build, returned at most
    once per distinct text per run — for replay when a staleness check
    skips [dest] as fresh. *)
