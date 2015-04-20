open Bar

let () =
    let l = real_list [1;2;3] in
    List.iter
      (fun i ->
         Printf.printf "%d.%d\n"
           (Math.Accessor.get_real i)
           (Math.Accessor.get_imag i))
      l
