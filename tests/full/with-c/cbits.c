#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

CAMLexport value stub_geti(value unit)
{
	CAMLparam1(unit);
        CAMLreturn(Val_int(10));
}
