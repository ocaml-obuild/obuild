open Ext.Filepath
open Helper
open Gconf

exception GeneratorFailed of string
exception GeneratorNotFound of string

type t = {
  suffix : string;
  command : (filepath -> filepath -> string list);
  generated_files : (filename -> filename list);
}

let generators = ref [
    { suffix = "mll";
      command = (fun src dest_root -> [Prog.getOcamlLex (); "-o"; (fp_to_string dest_root) ^ ".ml"; fp_to_string src]);
      generated_files = (fun f -> [(chop_extension f) <.>  "ml"])
    };
    { suffix = "mly";
      command = (fun src dest_root -> [Prog.getOcamlYacc (); "-b"; fp_to_string dest_root; fp_to_string src]);
      generated_files = (fun f -> [(chop_extension f) <.>  "ml"])
    };
  ]

let is_generator_ext ext = List.exists (fun gen -> gen.suffix = ext) !generators
let get_generator fp =
  let ext = Filetype.of_filepath fp in
  let s = match ext with Filetype.FileOther s -> s | _ -> raise (GeneratorNotFound (fp_to_string fp)) in
  List.find (fun gen -> gen.suffix = s) !generators
  
let run dest src =
  verbose Debug "  generator dest = %s src = %s\n%!" (fp_to_string dest) (fp_to_string src);
  let gen = get_generator src in
  let args = gen.command src dest in
  match Process.run args with
  | Process.Success (_, warnings,_) -> warnings
  | Process.Failure er -> raise (GeneratorFailed er)
