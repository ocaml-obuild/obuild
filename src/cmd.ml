open Printf

type cmd = {
  name : string;
  args : (Arg.key * Arg.spec * Arg.doc) list;
  fn : string list -> unit;
  short_desc : string;
  long_desc : string;
}

let (cmds : ((string, cmd) Hashtbl.t) ref) = ref (Hashtbl.create 13)

let programName = "obuild"

let register_cmd cmd =
  try
    ignore (Hashtbl.find !cmds cmd.name)
  with Not_found ->
    Hashtbl.add !cmds cmd.name cmd

let find_cmd name =
  Hashtbl.find !cmds name

let require_cmd name =
  try
    Hashtbl.find !cmds name
  with Not_found ->
    eprintf "error: unknown command: %s. See `%s --help'.\n"
      name programName;
    exit 1

let cmds_list () =
  Hashtbl.fold (
    fun key _ acc ->
      key :: acc
  ) !cmds []
