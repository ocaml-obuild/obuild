external adler32 : int -> int = "stub_adler32"

let () =
    let v = adler32 10 in
    Printf.printf "zerror 10 = %x\n" v
