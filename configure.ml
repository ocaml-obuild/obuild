(* Selects the compat implementation for the current compiler version and
   generates lib/base/compat.ml (compat_common.ml + the matching compat40X.ml).
   Pure OCaml on purpose: no shell commands, so it behaves identically under
   Unix shells, the msys2 shell and cmd.exe. *)

let read_file path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let buf = Buffer.create len in
  (try Buffer.add_channel buf ic len with End_of_file -> ());
  close_in ic;
  Buffer.contents buf

let () =
  let version = Sys.ocaml_version in
  let variant =
    if version < "4.02.0" then "compat401.ml"
    else if version < "4.03.0" then "compat402.ml"
    else "compat403.ml"
  in
  let dest = Filename.concat (Filename.concat "lib" "base") "compat.ml" in
  (try Sys.remove dest with Sys_error _ -> ());
  let oc = open_out_bin dest in
  output_string oc (read_file "compat_common.ml");
  output_string oc "\n";
  output_string oc (read_file variant);
  close_out oc
