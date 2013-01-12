#include <zlib.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

CAMLexport value stub_adler32(value i)
{
	CAMLparam1(i);

	/* wrong but that's fine */
	unsigned int adler = adler32(0L, Z_NULL, 0);

	CAMLreturn(Val_int(adler));
}
