open Fugue

exception DocumentationBuildingFailed of string

let runOcamldoc pp =
  let args = [ Prog.get_ocamldoc (); "-html" ] @ maybe [] (fun s -> [ "-pp"; s ]) pp @ [] in
  match Process.run args with
  | Process.Failure er -> raise (DocumentationBuildingFailed er)
  | Process.Success (_, _, _) -> ()

let run _projFile = ()
