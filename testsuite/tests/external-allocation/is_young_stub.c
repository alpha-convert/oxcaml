#include "caml/mlvalues.h"
#include "caml/address_class.h"
#include "caml/memory.h"
#include <stdio.h>

CAMLprim value is_young(value v){
    return Val_bool(Is_young(v));
}

CAMLprim value print_block(value v, uint64_t fields, value name){
    printf("Test %s:\n", String_val(name));
    for(int i = 0; i < fields; i++){
        printf("  Field %d: %u\n",i,(uint64_t) Field(v,i));
    }
    printf("\n");
    return Val_unit;
}
