#include "caml/mlvalues.h"
#include "caml/address_class.h"
#include "caml/memory.h"
#include <stdio.h>
#include <malloc.h>
#include <stdbool.h>

static bool called_free = false;

CAMLprim value free_was_called()
{
  return Val_bool(called_free);
}

CAMLprim value called_free_reset()
{
  called_free = false;
  return Val_unit;
}


CAMLextern intnat __real_caml_free_external(intnat);

CAMLprim void __wrap_caml_free_external(intnat x)
{
  called_free = true;
  __real_caml_free_external(x);
}
