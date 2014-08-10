open Ext.Filepath

exception GeneratorFailed of string
exception GeneratorNotFound of string

type generator = {
  suffix : string;
  command : string list;
  dest_suffix : string;
}

let generators = ref [ 
    { suffix = "mll"; command = [Prog.getOcamlLex (); "-o"]; dest_suffix = ".ml" };
    { suffix = "mly"; command = [Prog.getOcamlYacc (); "-b"]; dest_suffix = "" };
  ]

let is_generator_ext ext = List.exists (fun gen -> gen.suffix = ext) !generators

let run dest src =
  let ext = Filetype.of_filepath src in
  let s = match ext with Filetype.FileOther s -> s | _ -> raise (GeneratorNotFound (fp_to_string src)) in
  let gen = List.find (fun gen -> gen.suffix = s) !generators in
  let args = gen.command @ [ (fp_to_string dest) ^ gen.dest_suffix; fp_to_string src ] in
  match Process.run args with
  | Process.Success (_, warnings,_) -> warnings
  | Process.Failure er -> raise (GeneratorFailed er)
