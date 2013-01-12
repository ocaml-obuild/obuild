open Imaginary
open Math

let () =
    let t1 = { Types.real = 0; Types.imag = 1 } in
    let t2 = { Types.real = 1; Types.imag = 2 } in
    let t3 = imaginary_plus t1 t2 in
    Printf.printf "real = %d\n" (Accessor.get_real t3);
    ()
