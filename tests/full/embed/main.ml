(* Test for pattern-based generate blocks *)

let () =
  (* List all embedded files *)
  Printf.printf "Embedded files:\n";
  List.iter (fun name ->
    Printf.printf "  - %s\n" name
  ) (Assets.list ());

  (* Print contents of each file *)
  Printf.printf "\nContents:\n";
  List.iter (fun name ->
    match Assets.get name with
    | Some content -> Printf.printf "%s: %s\n" name content
    | None -> Printf.printf "%s: <not found>\n" name
  ) (Assets.list ());

  (* Verify expected files are present *)
  let expected = ["hello.txt"; "greeting.txt"; "info.txt"] in
  let all_present = List.for_all (fun name ->
    match Assets.get name with
    | Some _ -> true
    | None ->
        Printf.printf "ERROR: Missing expected file: %s\n" name;
        false
  ) expected in

  if all_present && List.length (Assets.list ()) = 3 then begin
    Printf.printf "\nSUCCESS: All %d files embedded correctly!\n" (List.length expected);
    exit 0
  end else begin
    Printf.printf "\nFAILURE: Expected %d files, got %d\n"
      (List.length expected) (List.length (Assets.list ()));
    exit 1
  end
