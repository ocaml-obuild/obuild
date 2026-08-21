#include <caml/mlvalues.h>
CAMLprim value caml_cadd(value a, value b) { return Val_int(Int_val(a) + Int_val(b)); }
