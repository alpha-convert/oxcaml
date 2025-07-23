#include "caml/mlvalues.h"
#include "caml/address_class.h"
#include "caml/memory.h"

CAMLprim value is_young(value v){
    return Val_bool(Is_young(v));
}
