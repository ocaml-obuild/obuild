#include <gtk/gtk.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

CAMLexport value stub_gtk_true(value unit)
{
	CAMLparam1(unit);
	int b = gtk_true();
        CAMLreturn(Val_int(b ? 1 : 0));
}
