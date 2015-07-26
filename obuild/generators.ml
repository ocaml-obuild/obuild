open Ext.Filepath
open Helper
open Gconf

exception GeneratorFailed of string
exception GeneratorNotFound of string

type t = {
  suffix : string;
  modname : (Modname.t -> Modname.t);
  commands : (filepath -> filepath -> string -> string list list);
  generated_files : (filename -> string -> filename);
}

let generators = ref [
    { suffix = "mll";
      modname = (fun m -> m);
      commands = (fun src dest_root _ -> [[Prog.getOcamlLex (); "-o"; (fp_to_string dest_root) ^ ".ml"; fp_to_string src]]);
      generated_files = (fun f _ -> (chop_extension f) <.>  "ml")
    };
    { suffix = "mly";
      modname = (fun m -> m);
      commands = (fun src dest_root _ -> [[Prog.getOcamlYacc (); "-b"; fp_to_string dest_root; fp_to_string src]]);
      generated_files = (fun f _ -> (chop_extension f) <.>  "ml")
    };
    { suffix = "atd";
      modname = (fun m -> Modname.atd_modname m);
      commands = (fun src dest_root moduleName ->
                   let len = String.length moduleName in
                   let ext = String.sub moduleName (len - 2) 2 in
                   match ext with
                   | "_t" ->
                     [[Prog.getAtdGen (); "-t"; fp_to_string src; "-o"; (fp_to_string dest_root)]]
                   | "_v" ->
                     [[Prog.getAtdGen (); "-v"; fp_to_string src; "-o"; (fp_to_string dest_root)]]
                   | "_j" ->
                     [[Prog.getAtdGen (); "-j"; "-j-std"; fp_to_string src; "-o"; (fp_to_string dest_root)]]
                   | _ -> raise (GeneratorFailed ("extension " ^ ext ^ " is unknown"))
                 );
      generated_files = (fun f moduleName -> let base = fn_to_string (chop_extension f) in
                          let len = String.length moduleName in
                          let ext = String.sub moduleName (len - 2) 2 in
                          match ext with
                          | "_t" -> fn (base ^ "_t.ml")
                          | "_v" -> fn (base ^ "_v.ml")
                          | "_j" -> fn (base ^ "_j.ml")
                          | _ -> raise (GeneratorFailed ("extension " ^ ext ^ " is unknown"))
                        )
    };
  ]

let is_generator_ext ext = List.exists (fun gen -> gen.suffix = ext) !generators
let get_generator fp =
  let ext = Filetype.of_filepath fp in
  let s = match ext with Filetype.FileOther s -> s | _ -> raise (GeneratorNotFound (fp_to_string fp)) in
  List.find (fun gen -> gen.suffix = s) !generators
  
let run dest src modName =
  verbose Debug "  generator dest = %s src = %s\n%!" (fp_to_string dest) (fp_to_string src);
  let gen = get_generator src in
  let args = gen.commands src dest modName in
  List.iter (fun arg ->
      match Process.run arg with
      | Process.Success (_, warnings,_) -> print_warnings warnings
      | Process.Failure er -> raise (GeneratorFailed er) ) args
