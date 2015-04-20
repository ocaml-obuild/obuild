
let imaginary_plus t1 t2 =
      { Math.Types.real = Math.Accessor.get_real t1 + t2.Math.Types.real
      ; Math.Types.imag = t1.Math.Types.imag + Math.Accessor.get_imag t2
      }
