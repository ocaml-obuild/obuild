open Filepath

type t = {
  cache : (filepath, float) Hashtbl.t;
  mutable hits : int;
  mutable misses : int;
}

let create () = {
  cache = Hashtbl.create 100;
  hits = 0;
  misses = 0;
}

let get_mtime cache path =
  try
    let mtime = Hashtbl.find cache.cache path in
    cache.hits <- cache.hits + 1;
    mtime
  with Not_found ->
    cache.misses <- cache.misses + 1;
    let mtime =
      try Filesystem.get_modification_time path
      with _ -> 0.0
    in
    Hashtbl.add cache.cache path mtime;
    mtime

let invalidate cache path =
  Hashtbl.remove cache.cache path

let clear cache =
  Hashtbl.clear cache.cache;
  cache.hits <- 0;
  cache.misses <- 0

let stats cache = (cache.hits, cache.misses)
