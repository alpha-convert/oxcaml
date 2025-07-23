(* TEST
 modules = "is_young_stub.c";
 native;
*)

external is_young : ('a : word) -> bool = "is_young" "is_young"

let is_a_malloc name f =
  let before = Gc.allocated_bytes () in
  let v = f () in
  let after = Gc.allocated_bytes () in
  Format.printf "%s: bytes allocated: %f, in the small heap: %b\n" name (after -. before) (is_young v);
  ()

(* Tuple allocations with different patterns *)
let () = is_a_malloc "tuple" (fun () -> malloc_ (1,3))

(* Record allocations *)
type t = {x : int; y : int}
let () = is_a_malloc "record" (fun () -> malloc_ {x = 3; y = 4})

type t' = {x : int; y : int64#}
let () = is_a_malloc "record_mixed" (fun () -> malloc_ {x = 1; y = #42L})

type mixed_first = {unboxed_field : int64#; boxed_field : string; another_boxed : int}
let () = is_a_malloc "mixed_int64" (fun () -> malloc_ {unboxed_field = #100L; boxed_field = "test"; another_boxed = 42})

type mixed_middle = {start : int; middle_unboxed : float#; end_field : string}
let () = is_a_malloc "mixed_float" (fun () -> malloc_ {start = 10; middle_unboxed = #3.14; end_field = "end"})

type mixed_last = {first_boxed : bool; second_boxed : int; last_unboxed : int32#}
let () = is_a_malloc "mixed_int32" (fun () -> malloc_ {first_boxed = true; second_boxed = 99; last_unboxed = #777l})

type bool_record = {flag : bool; id : int}
let () = is_a_malloc "bool_record_mixed" (fun () -> malloc_ {flag = true; id = 0})

(* Variant allocations *)
type 'a variant = Bar of 'a
let () = is_a_malloc "variant_allocation" (fun () -> malloc_ (Bar 10))
let () = is_a_malloc "variant_string_arg" (fun () -> malloc_ (Bar "test"))

type int_variant = B of int | C of int * string
let () = is_a_malloc "int_variant_single" (fun () -> malloc_ (B 5))
let () = is_a_malloc "int_variant_tuple" (fun () -> malloc_ (C (10, "data")))
let () = is_a_malloc "int_variant_const_tuple" (fun () -> malloc_ (C (1, "constant")))

let () = is_a_malloc "option_allocation" (fun () -> malloc_ (Some 15))
let () = is_a_malloc "option_string_some" (fun () -> malloc_ (Some "hello"))

(* List allocations *)
let () = is_a_malloc "single_element_list" (fun () -> malloc_ [5])

(* GADT allocations *)
type gadt = Pack : 'a -> gadt
let () = is_a_malloc "gadt_allocation" (fun () -> malloc_ (Pack 100))
let () = is_a_malloc "gadt_const_int" (fun () -> malloc_ (Pack 42))
let () = is_a_malloc "gadt_const_string" (fun () -> malloc_ (Pack "test"))

type 'a typed_gadt = IntPack : int -> int typed_gadt | StringPack : string -> string typed_gadt
let () = is_a_malloc "typed_gadt_int" (fun () -> malloc_ (IntPack 99))
let () = is_a_malloc "typed_gadt_string" (fun () -> malloc_ (StringPack "hello"))
let () = is_a_malloc "typed_gadt_const_int" (fun () -> malloc_ (IntPack 42))

(* Polymorphic variants *)
let () = is_a_malloc "poly_variant" (fun () -> malloc_ (`Apple 7))
let () = is_a_malloc "poly_apple_const" (fun () -> malloc_ (`Apple 42))
let () = is_a_malloc "poly_orange_const" (fun () -> malloc_ (`Orange "citrus"))
let () = is_a_malloc "poly_orange_var" (fun () -> malloc_ (`Orange "fruit"))

let () = is_a_malloc "poly_int_tag" (fun () -> malloc_ (`Number 123))
let () = is_a_malloc "poly_string_tag" (fun () -> malloc_ (`Text "content"))
