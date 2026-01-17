(* Test executable for ctypes_test *)

let () =
  (* Show the size of size_t type using Ctypes.sizeof *)
  Printf.printf "size_t size: %d bytes\n" (Ctypes.sizeof C.Types.size_t);

  (* Show the timespec struct layout - demonstrates struct support *)
  Printf.printf "timespec struct size: %d bytes\n" (Ctypes.sizeof C.Types.timespec);
  Printf.printf "  tv_sec offset: %d\n" (Ctypes.offsetof C.Types.tv_sec);
  Printf.printf "  tv_nsec offset: %d\n" (Ctypes.offsetof C.Types.tv_nsec);

  (* Test the strlen binding - with errno: return, functions return (result, errno) pairs *)
  let test_string = "Hello, ctypes!" in
  let (result, _errno) = C.C_Functions.strlen test_string in
  let len = Unsigned.Size_t.to_int result in
  Printf.printf "strlen(\"%s\") = %d\n" test_string len
