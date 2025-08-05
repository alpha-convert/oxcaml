(* TEST
 include stdlib_upstream_compatible;
  modules = "free-stubs.c";
  flags = "-cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_free_external";
  native;
*)

module Int64_u = Stdlib_upstream_compatible.Int64_u
module Float_u = Stdlib_upstream_compatible.Float_u

external globalize_float : float @ local -> float = "%obj_dup"

external free_was_called : unit -> bool = "free_was_called" "free_was_called"
external called_free_reset : unit -> unit = "called_free_reset" "called_free_reset"

let test_with_malloc_tracking name f =
  let () = called_free_reset () in
  f ();
  let msg =
    if free_was_called () then "free was called" else "FREE WAS NOT CALLED" in
  Printf.printf "%s: %s\n\n" name msg

(* Tuples *)
let g x y = malloc_ (x,y)
let () =
  let m = g 2 3 in
  test_with_malloc_tracking "tuple" (fun () ->
    let (a,b) = free_stack_ m in
    Printf.printf "%d %d\n" a b)

let g x y z = malloc_ (x,y,z)
let () =
  let m = g 2 3 4 in
  test_with_malloc_tracking "tuple3" (fun () ->
    let (a,b,c) = free_stack_ m in
    Printf.printf "%d %d %d\n" a b c)

type t1 = {x : int}
let g1 x = malloc_ {x}
let () =
  let m = g1 2 in
  test_with_malloc_tracking "singleton record" (fun () ->
    let {x} = free_stack_ m in
    Printf.printf "%d\n" x)

type t2 = {x : int; y : int}
let g2 x y = malloc_ {x;y}
let () =
  let m = g2 2 3 in
  test_with_malloc_tracking "two-element record" (fun () ->
    let {x;y} = free_stack_ m in
    Printf.printf "%d %d\n" x y)

type t2' = {mutable x : int; mutable y : int}
let g2' x y = malloc_ {x;y}
let () =
  let m = g2' 2 3 in
  test_with_malloc_tracking "mutable two-element record" (fun () ->
    let {x;y} = free_stack_ m in
    Printf.printf "%d %d\n" x y)

type t3 = {x : int; y : int64#}
let g3 x y = malloc_ {x;y}
let () =
  let m = g3 2 #3L in
  test_with_malloc_tracking "mixed record" (fun () ->
    let {x;y} = free_stack_ m in
    Printf.printf "%d %Ld\n" x (Int64_u.to_int64 y))

type t4 = {x : int64#; y : int}
let g4 x y = malloc_ {x;y}
let () =
  let m = g4 #2L 3 in
  test_with_malloc_tracking "mixed record 2" (fun () ->
    let {x;y} = free_stack_ m in
    Printf.printf "%Ld %d\n" (Int64_u.to_int64 x) y)

type t5 = {x : int64#; y : int64#}
let g5 x y = malloc_ {x;y}
let () =
  let m = g5 #2L #3L in
  test_with_malloc_tracking "oops all unboxed" (fun () ->
    let {x;y} = free_stack_ m in
    Printf.printf "%Ld %Ld\n" (Int64_u.to_int64 x) (Int64_u.to_int64 y))

type t6 = {x : float; y : float; z : float}
let g6 x y z = malloc_ {x;y;z}
let () =
  let m = g6 1.0 2.0 3.0 in
  test_with_malloc_tracking "float record" (fun () ->
    let {x;y;z} = free_stack_ m in
    Printf.printf "%f %f %f\n" (globalize_float x) (globalize_float y) (globalize_float z))

type t7 = {x : float#; y : float#; z : float#}
let g7 x y z = malloc_ {x;y;z}
let () =
  let m = g7 #1.0 #2.0 #3.0 in
  test_with_malloc_tracking "ufloat record" (fun () ->
    let {x;y;z} = free_stack_ m in
    print_float (Float_u.to_float x);
    print_float (Float_u.to_float y);
    print_float (Float_u.to_float z);
    ())

type t8 = {a : int; b : char; c : float; d : int64#}
let g8 a b c d = malloc_ {a;b;c;d}
let () =
  let m = g8 1 'x' 2.5 #42L in
  test_with_malloc_tracking "four-field mixed record" (fun () ->
    let {a;b;c;d} = free_stack_ m in
    Printf.printf "%d %c %f %Ld\n" a b (globalize_float c) (Int64_u.to_int64 d))

type t9 = {w : float#; x : int; y : char; z : int64#}
let g9 w x y z = malloc_ {w;x;y;z}
let () =
  let m = g9 #3.14 5 'a' #100L in
  test_with_malloc_tracking "four-field mixed with ufloat record" (fun () ->
    let {w;x;y;z} = free_stack_ m in
    Printf.printf "%f %d %c %Ld\n" (Float_u.to_float w) x y (Int64_u.to_int64 z))

type t10 = {p : int; q : int; r : int; s : int}
let g10 p q r s = malloc_ {p;q;r;s}
let () =
  let m = g10 1 2 3 4 in
  test_with_malloc_tracking "four-field all int record" (fun () ->
    let {p;q;r;s} = free_stack_ m in
    Printf.printf "%d %d %d %d\n" p q r s)

type t11 = {mutable ma : int; mb : float; mutable mc : char}
let g11 ma mb mc = malloc_ {ma;mb;mc}
let () =
  let m = g11 10 1.5 'z' in
  test_with_malloc_tracking "three-field mixed mutable record" (fun () ->
    let {ma;mb;mc} = free_stack_ m in
    Printf.printf "%d %f %c\n" ma (globalize_float mb) mc)

type t12 = {fa : float#; fb : float#; fc : float#; fd : float#}
let g12 fa fb fc fd = malloc_ {fa;fb;fc;fd}
let () =
  let m = g12 #1.1 #2.2 #3.3 #4.4 in
  test_with_malloc_tracking "four-field all ufloat record" (fun () ->
    let {fa;fb;fc;fd} = free_stack_ m in
    Printf.printf "%f %f %f %f\n" (Float_u.to_float fa) (Float_u.to_float fb) (Float_u.to_float fc) (Float_u.to_float fd))

(*
Foo of int
Foo of {x : int; y : int}
Foo of {x : int; y : int64#}
Foo of {x : int64#; y : int64#}
Foo of {x : int64#; y : int}
Foo of {x : float; y : float}
Foo of {x : float#; y : float#}

Foo of int | Bar of char
Foo of {x : int; y : int} | Bar of char
Foo of {x : int; y : int64#} | Bar of char
Foo of {x : int64#; y : int64#} | Bar of char
Foo of {x : float; y : float} | Bar of char
Foo of {x : float#; y : float#} | Bar of char

Foo of int | Bar of {x : int; y : int}
Foo of {x : int; y : int} | Bar of {x : int; y : int}
Foo of {x : int; y : int64#} | Bar of {x : int; y : int}
Foo of {x : int64#; y : int64#} | Bar of {x : int; y : int}
Foo of {x : float; y : float} | Bar of {x : int; y : int}
Foo of {x : float#; y : float#} | Bar of {x : int; y : int}

Foo of int | Bar of {x : int; y : int64#}
Foo of {x : int; y : int} | Bar of {x : int; y : int64#}
Foo of {x : int; y : int64#} | Bar of {x : int; y : int64#}
Foo of {x : int64#; y : int64#} | Bar of {x : int; y : int64#}
Foo of {x : float; y : float} | Bar of {x : int; y : int64#}
Foo of {x : float#; y : float#} | Bar of {x : int; y : int64#}


*)


(* Single constructor variants *)

type v1 = Foo of int
let gv1 x = malloc_ (Foo x)
let () =
  let m = gv1 42 in
  test_with_malloc_tracking "single constructor int" (fun () ->
    match free_stack_ m with
    | Foo x -> Printf.printf "Foo %d\n" x)

type v2 = Foo of {x : int; y : int}
let gv2 x y = malloc_ (Foo {x;y})
let () =
  let m = gv2 1 2 in
  test_with_malloc_tracking "single constructor record" (fun () ->
    match free_stack_ m with
    | Foo {x;y} -> Printf.printf "Foo %d %d\n" x y)

type v3 = Foo of {x : int; y : int64#}
let gv3 x y = malloc_ (Foo {x;y})
let () =
  let m = gv3 1 #2L in
  test_with_malloc_tracking "single constructor mixed record" (fun () ->
    match free_stack_ m with
    | Foo {x;y} -> Printf.printf "Foo %d %Ld\n" x (Int64_u.to_int64 y))

type v4 = Foo of {x : int64#; y : int64#}
let gv4 x y = malloc_ (Foo {x;y})
let () =
  let m = gv4 #1L #2L in
  test_with_malloc_tracking "single constructor unboxed record" (fun () ->
    match free_stack_ m with
    | Foo {x;y} -> Printf.printf "Foo %Ld %Ld\n" (Int64_u.to_int64 x) (Int64_u.to_int64 y))

type v5 = Foo of {x : int64#; y : int}
let gv5 x y = malloc_ (Foo {x;y})
let () =
  let m = gv5 #1L 2 in
  test_with_malloc_tracking "single constructor mixed record 2" (fun () ->
    match free_stack_ m with
    | Foo {x;y} -> Printf.printf "Foo %Ld %d\n" (Int64_u.to_int64 x) y)

type v6 = Foo of {x : float; y : float}
let gv6 x y = malloc_ (Foo {x;y})
let () =
  let m = gv6 1.0 2.0 in
  test_with_malloc_tracking "single constructor float record" (fun () ->
    match free_stack_ m with
    | Foo {x;y} -> Printf.printf "Foo %f %f\n" (globalize_float x) (globalize_float y))

type v7 = Foo of {x : float#; y : float#}
let gv7 x y = malloc_ (Foo {x;y})
let () =
  let m = gv7 #1.0 #2.0 in
  test_with_malloc_tracking "single constructor ufloat record" (fun () ->
    match free_stack_ m with
    | Foo {x;y} -> Printf.printf "Foo %f %f\n" (Float_u.to_float x) (Float_u.to_float y))

(* Two constructor variants - mixed constructor types *)

type v8 = Foo of int | Bar of char
let gv8a x = malloc_ (Foo x)
let gv8b x = malloc_ (Bar x)

let print_v8 = function
  | Foo x -> Printf.printf "Foo %d\n" x
  | Bar x -> Printf.printf "Bar %c\n" x
let () =
  let m1 = gv8a 42 in
  let m2 = gv8b 'c' in
  test_with_malloc_tracking "two constructors int/char, Foo case" (fun () ->
    print_v8 (free_stack_ m1));
  test_with_malloc_tracking "two constructors int/char, Bar case" (fun () ->
    print_v8 (free_stack_ m2))

type v9 = Foo of {x : int; y : int} | Bar of char
let gv9a x y = malloc_ (Foo {x;y})
let gv9b x = malloc_ (Bar x)
let () =
  let m1 = gv9a 1 2 in
  let m2 = gv9b 'c' in
  test_with_malloc_tracking "two constructors record/char, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo {x;y} -> Printf.printf "Foo %d %d\n" x y
    | Bar x -> Printf.printf "Bar %c\n" x);
  test_with_malloc_tracking "two constructors record/char, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo {x;y} -> Printf.printf "Foo %d %d\n" x y
    | Bar x -> Printf.printf "Bar %c\n" x)

type v10 = Foo of {x : int; y : int64#} | Bar of char
let gv10a x y = malloc_ (Foo {x;y})
let gv10b x = malloc_ (Bar x)
let () =
  let m1 = gv10a 1 #2L in
  let m2 = gv10b 'c' in
  test_with_malloc_tracking "two constructors mixed record/char, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo {x;y} -> Printf.printf "Foo %d %Ld\n" x (Int64_u.to_int64 y)
    | Bar x -> Printf.printf "Bar %c\n" x);
  test_with_malloc_tracking "two constructors mixed record/char, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo {x;y} -> Printf.printf "Foo %d %Ld\n" x (Int64_u.to_int64 y)
    | Bar x -> Printf.printf "Bar %c\n" x)

type v11 = Foo of {x : int64#; y : int64#} | Bar of char
let gv11a x y = malloc_ (Foo {x;y})
let gv11b x = malloc_ (Bar x)
let () =
  let m1 = gv11a #1L #2L in
  let m2 = gv11b 'c' in
  test_with_malloc_tracking "two constructors unboxed record/char, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo {x;y} -> Printf.printf "Foo %Ld %Ld\n" (Int64_u.to_int64 x) (Int64_u.to_int64 y)
    | Bar x -> Printf.printf "Bar %c\n" x);
  test_with_malloc_tracking "two constructors unboxed record/char, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo {x;y} -> Printf.printf "Foo %Ld %Ld\n" (Int64_u.to_int64 x) (Int64_u.to_int64 y)
    | Bar x -> Printf.printf "Bar %c\n" x)

type v12 = Foo of {x : float; y : float} | Bar of char
let gv12a x y = malloc_ (Foo {x;y})
let gv12b x = malloc_ (Bar x)
let () =
  let m1 = gv12a 1.0 2.0 in
  let m2 = gv12b 'c' in
  test_with_malloc_tracking "two constructors float record/char, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo {x;y} -> Printf.printf "Foo %f %f\n" (globalize_float x) (globalize_float y)
    | Bar x -> Printf.printf "Bar %c\n" x);
  test_with_malloc_tracking "two constructors float record/char, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo {x;y} -> Printf.printf "Foo %f %f\n" (globalize_float x) (globalize_float y)
    | Bar x -> Printf.printf "Bar %c\n" x)

type v13 = Foo of {x : float#; y : float#} | Bar of char
let gv13a x y = malloc_ (Foo {x;y})
let gv13b x = malloc_ (Bar x)
let () =
  let m1 = gv13a #1.0 #2.0 in
  let m2 = gv13b 'c' in
  test_with_malloc_tracking "two constructors ufloat record/char, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo {x;y} -> Printf.printf "Foo %f %f\n" (Float_u.to_float x) (Float_u.to_float y)
    | Bar x -> Printf.printf "Bar %c\n" x);
  test_with_malloc_tracking "two constructors ufloat record/char, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo {x;y} -> Printf.printf "Foo %f %f\n" (Float_u.to_float x) (Float_u.to_float y)
    | Bar x -> Printf.printf "Bar %c\n" x)

(* Two constructor variants - both record types *)

type v14 = Foo of int | Bar of {x : int; y : int}
let gv14a x = malloc_ (Foo x)
let gv14b x y = malloc_ (Bar {x;y})
let () =
  let m1 = gv14a 42 in
  let m2 = gv14b 1 2 in
  test_with_malloc_tracking "two constructors int/record, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo x -> Printf.printf "Foo %d\n" x
    | Bar {x;y} -> Printf.printf "Bar %d %d\n" x y);
  test_with_malloc_tracking "two constructors int/record, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo x -> Printf.printf "Foo %d\n" x
    | Bar {x;y} -> Printf.printf "Bar %d %d\n" x y)

type v15 = Foo of {x : int; y : int} | Bar of {x : int; y : int}
let gv15a x y = malloc_ (Foo {x;y})
let gv15b x y = malloc_ (Bar {x;y})
let () =
  let m1 = gv15a 1 2 in
  let m2 = gv15b 3 4 in
  test_with_malloc_tracking "two constructors both regular records, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo {x;y} -> Printf.printf "Foo %d %d\n" x y
    | Bar {x;y} -> Printf.printf "Bar %d %d\n" x y);
  test_with_malloc_tracking "two constructors both regular records, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo {x;y} -> Printf.printf "Foo %d %d\n" x y
    | Bar {x;y} -> Printf.printf "Bar %d %d\n" x y)

type v16 = Foo of {x : int; y : int64#} | Bar of {x : int; y : int}
let gv16a x y = malloc_ (Foo {x;y})
let gv16b x y = malloc_ (Bar {x;y})
let () =
  let m1 = gv16a 1 #2L in
  let m2 = gv16b 3 4 in
  test_with_malloc_tracking "two constructors mixed/regular records, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo {x;y} -> Printf.printf "Foo %d %Ld\n" x (Int64_u.to_int64 y)
    | Bar {x;y} -> Printf.printf "Bar %d %d\n" x y);
  test_with_malloc_tracking "two constructors mixed/regular records, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo {x;y} -> Printf.printf "Foo %d %Ld\n" x (Int64_u.to_int64 y)
    | Bar {x;y} -> Printf.printf "Bar %d %d\n" x y)

type v17 = Foo of {x : int64#; y : int64#} | Bar of {x : int; y : int}
let gv17a x y = malloc_ (Foo {x;y})
let gv17b x y = malloc_ (Bar {x;y})
let () =
  let m1 = gv17a #1L #2L in
  let m2 = gv17b 3 4 in
  test_with_malloc_tracking "two constructors unboxed/regular records, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo {x;y} -> Printf.printf "Foo %Ld %Ld\n" (Int64_u.to_int64 x) (Int64_u.to_int64 y)
    | Bar {x;y} -> Printf.printf "Bar %d %d\n" x y);
  test_with_malloc_tracking "two constructors unboxed/regular records, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo {x;y} -> Printf.printf "Foo %Ld %Ld\n" (Int64_u.to_int64 x) (Int64_u.to_int64 y)
    | Bar {x;y} -> Printf.printf "Bar %d %d\n" x y)

type v18 = Foo of {x : float; y : float} | Bar of {x : int; y : int}
let gv18a x y = malloc_ (Foo {x;y})
let gv18b x y = malloc_ (Bar {x;y})
let () =
  let m1 = gv18a 1.0 2.0 in
  let m2 = gv18b 3 4 in
  test_with_malloc_tracking "two constructors float/regular records, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo {x;y} -> Printf.printf "Foo %f %f\n" (globalize_float x) (globalize_float y)
    | Bar {x;y} -> Printf.printf "Bar %d %d\n" x y);
  test_with_malloc_tracking "two constructors float/regular records, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo {x;y} -> Printf.printf "Foo %f %f\n" (globalize_float x) (globalize_float y)
    | Bar {x;y} -> Printf.printf "Bar %d %d\n" x y)

type v19 = Foo of {x : float#; y : float#} | Bar of {x : int; y : int}
let gv19a x y = malloc_ (Foo {x;y})
let gv19b x y = malloc_ (Bar {x;y})
let () =
  let m1 = gv19a #1.0 #2.0 in
  let m2 = gv19b 3 4 in
  test_with_malloc_tracking "two constructors ufloat/regular records, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo {x;y} -> Printf.printf "Foo %f %f\n" (Float_u.to_float x) (Float_u.to_float y)
    | Bar {x;y} -> Printf.printf "Bar %d %d\n" x y);
  test_with_malloc_tracking "two constructors ufloat/regular records, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo {x;y} -> Printf.printf "Foo %f %f\n" (Float_u.to_float x) (Float_u.to_float y)
    | Bar {x;y} -> Printf.printf "Bar %d %d\n" x y)

(* Mixed record types variants *)

type v20 = Foo of int | Bar of {x : int; y : int64#}
let gv20a x = malloc_ (Foo x)
let gv20b x y = malloc_ (Bar {x;y})
let () =
  let m1 = gv20a 42 in
  let m2 = gv20b 1 #2L in
  test_with_malloc_tracking "two constructors int/mixed record, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo x -> Printf.printf "Foo %d\n" x
    | Bar {x;y} -> Printf.printf "Bar %d %Ld\n" x (Int64_u.to_int64 y));
  test_with_malloc_tracking "two constructors int/mixed record, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo x -> Printf.printf "Foo %d\n" x
    | Bar {x;y} -> Printf.printf "Bar %d %Ld\n" x (Int64_u.to_int64 y))

type v21 = Foo of {x : int; y : int} | Bar of {x : int; y : int64#}
let gv21a x y = malloc_ (Foo {x;y})
let gv21b x y = malloc_ (Bar {x;y})
let () =
  let m1 = gv21a 1 2 in
  let m2 = gv21b 3 #4L in
  test_with_malloc_tracking "two constructors regular/mixed records, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo {x;y} -> Printf.printf "Foo %d %d\n" x y
    | Bar {x;y} -> Printf.printf "Bar %d %Ld\n" x (Int64_u.to_int64 y));
  test_with_malloc_tracking "two constructors regular/mixed records, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo {x;y} -> Printf.printf "Foo %d %d\n" x y
    | Bar {x;y} -> Printf.printf "Bar %d %Ld\n" x (Int64_u.to_int64 y))

type v22 = Foo of {x : int; y : int64#} | Bar of {x : int; y : int64#}
let gv22a x y = malloc_ (Foo {x;y})
let gv22b x y = malloc_ (Bar {x;y})
let () =
  let m1 = gv22a 1 #2L in
  let m2 = gv22b 3 #4L in
  test_with_malloc_tracking "two constructors both mixed records, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo {x;y} -> Printf.printf "Foo %d %Ld\n" x (Int64_u.to_int64 y)
    | Bar {x;y} -> Printf.printf "Bar %d %Ld\n" x (Int64_u.to_int64 y));
  test_with_malloc_tracking "two constructors both mixed records, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo {x;y} -> Printf.printf "Foo %d %Ld\n" x (Int64_u.to_int64 y)
    | Bar {x;y} -> Printf.printf "Bar %d %Ld\n" x (Int64_u.to_int64 y))

type v23 = Foo of {x : int64#; y : int64#} | Bar of {x : int; y : int64#}
let gv23a x y = malloc_ (Foo {x;y})
let gv23b x y = malloc_ (Bar {x;y})
let () =
  let m1 = gv23a #1L #2L in
  let m2 = gv23b 3 #4L in
  test_with_malloc_tracking "two constructors unboxed/mixed records, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo {x;y} -> Printf.printf "Foo %Ld %Ld\n" (Int64_u.to_int64 x) (Int64_u.to_int64 y)
    | Bar {x;y} -> Printf.printf "Bar %d %Ld\n" x (Int64_u.to_int64 y));
  test_with_malloc_tracking "two constructors unboxed/mixed records, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo {x;y} -> Printf.printf "Foo %Ld %Ld\n" (Int64_u.to_int64 x) (Int64_u.to_int64 y)
    | Bar {x;y} -> Printf.printf "Bar %d %Ld\n" x (Int64_u.to_int64 y))

type v24 = Foo of {x : float; y : float} | Bar of {x : int; y : int64#}
let gv24a x y = malloc_ (Foo {x;y})
let gv24b x y = malloc_ (Bar {x;y})
let () =
  let m1 = gv24a 1.0 2.0 in
  let m2 = gv24b 3 #4L in
  test_with_malloc_tracking "two constructors float/mixed records, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo {x;y} -> Printf.printf "Foo %f %f\n" (globalize_float x) (globalize_float y)
    | Bar {x;y} -> Printf.printf "Bar %d %Ld\n" x (Int64_u.to_int64 y));
  test_with_malloc_tracking "two constructors float/mixed records, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo {x;y} -> Printf.printf "Foo %f %f\n" (globalize_float x) (globalize_float y)
    | Bar {x;y} -> Printf.printf "Bar %d %Ld\n" x (Int64_u.to_int64 y))

type v25 = Foo of {x : float#; y : float#} | Bar of {x : int; y : int64#}
let gv25a x y = malloc_ (Foo {x;y})
let gv25b x y = malloc_ (Bar {x;y})
let () =
  let m1 = gv25a #1.0 #2.0 in
  let m2 = gv25b 3 #4L in
  test_with_malloc_tracking "two constructors ufloat/mixed records, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo {x;y} -> Printf.printf "Foo %f %f\n" (Float_u.to_float x) (Float_u.to_float y)
    | Bar {x;y} -> Printf.printf "Bar %d %Ld\n" x (Int64_u.to_int64 y));
  test_with_malloc_tracking "two constructors ufloat/mixed records, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo {x;y} -> Printf.printf "Foo %f %f\n" (Float_u.to_float x) (Float_u.to_float y)
    | Bar {x;y} -> Printf.printf "Bar %d %Ld\n" x (Int64_u.to_int64 y))

(* Mutable record variants *)

type v26 = Foo of {mutable x : int; y : int}
let gv26 x y = malloc_ (Foo {x;y})
let () =
  let m = gv26 1 2 in
  test_with_malloc_tracking "single constructor mutable record" (fun () ->
    match free_stack_ m with
    | Foo {x;y} -> Printf.printf "Foo %d %d\n" x y)

type v27 = Foo of {mutable x : int; y : int} | Bar of {x : int; mutable y : int}
let gv27a x y = malloc_ (Foo {x;y})
let gv27b x y = malloc_ (Bar {x;y})

let print_v27 = function
  | Foo {x;y} -> Printf.printf "Foo %d %d\n" x y
  | Bar {x;y} -> Printf.printf "Bar %d %d\n" x y
let () =
  let m1 = gv27a 1 2 in
  let m2 = gv27b 3 4 in
  test_with_malloc_tracking "two constructors both mutable records, Foo case" (fun () ->
    print_v27 (free_stack_ m1));
  test_with_malloc_tracking "two constructors both mutable records, Bar case" (fun () ->
    print_v27 (free_stack_ m2))

(* Tuple constructor variants - single constructors *)

type t28 = Foo of int * char
let g28 x y = malloc_ (Foo (x, y))
let () =
  let m = g28 42 'a' in
  test_with_malloc_tracking "single constructor tuple int*char" (fun () ->
    match free_stack_ m with
    | Foo (x, y) -> Printf.printf "Foo %d %c\n" x y)

type t29 = Foo of int * int
let g29 x y = malloc_ (Foo (x, y))
let () =
  let m = g29 1 2 in
  test_with_malloc_tracking "single constructor tuple int*int" (fun () ->
    match free_stack_ m with
    | Foo (x, y) -> Printf.printf "Foo %d %d\n" x y)

type t30 = Foo of int * int64#
let g30 x y = malloc_ (Foo (x, y))
let () =
  let m = g30 1 #2L in
  test_with_malloc_tracking "single constructor tuple int*int64#" (fun () ->
    match free_stack_ m with
    | Foo (x, y) -> Printf.printf "Foo %d %Ld\n" x (Int64_u.to_int64 y))

type t31 = Foo of int64# * int64#
let g31 x y = malloc_ (Foo (x, y))
let () =
  let m = g31 #1L #2L in
  test_with_malloc_tracking "single constructor tuple int64#*int64#" (fun () ->
    match free_stack_ m with
    | Foo (x, y) -> Printf.printf "Foo %Ld %Ld\n" (Int64_u.to_int64 x) (Int64_u.to_int64 y))

type t32 = Foo of float * float
let g32 x y = malloc_ (Foo (x, y))
let () =
  let m = g32 1.0 2.0 in
  test_with_malloc_tracking "single constructor tuple float*float" (fun () ->
    match free_stack_ m with
    | Foo (x, y) -> Printf.printf "Foo %f %f\n" (globalize_float x) (globalize_float y))

type t33 = Foo of float# * float#
let g33 x y = malloc_ (Foo (x, y))
let () =
  let m = g33 #1.0 #2.0 in
  test_with_malloc_tracking "single constructor tuple float#*float#" (fun () ->
    match free_stack_ m with
    | Foo (x, y) -> Printf.printf "Foo %f %f\n" (Float_u.to_float x) (Float_u.to_float y))

type t33b = Foo of int64# * int
let g33b x y = malloc_ (Foo (x, y))
let () =
  let m = g33b #1L 2 in
  test_with_malloc_tracking "single constructor tuple int64#*int" (fun () ->
    match free_stack_ m with
    | Foo (x, y) -> Printf.printf "Foo %Ld %d\n" (Int64_u.to_int64 x) y)

type t33c = Foo of float * int
let g33c x y = malloc_ (Foo (x, y))
let () =
  let m = g33c 1.0 2 in
  test_with_malloc_tracking "single constructor tuple float*int" (fun () ->
    match free_stack_ m with
    | Foo (x, y) -> Printf.printf "Foo %f %d\n" (globalize_float x) y)

type t33d = Foo of float# * int
let g33d x y = malloc_ (Foo (x, y))
let () =
  let m = g33d #1.0 2 in
  test_with_malloc_tracking "single constructor tuple float#*int" (fun () ->
    match free_stack_ m with
    | Foo (x, y) -> Printf.printf "Foo %f %d\n" (Float_u.to_float x) y)

type t33e = Foo of int * float#
let g33e x y = malloc_ (Foo (x, y))
let () =
  let m = g33e 1 #2.0 in
  test_with_malloc_tracking "single constructor tuple int*float#" (fun () ->
    match free_stack_ m with
    | Foo (x, y) -> Printf.printf "Foo %d %f\n" x (Float_u.to_float y))

type t34 = Foo of int * char * float
let g34 x y z = malloc_ (Foo (x, y, z))
let () =
  let m = g34 42 'a' 3.14 in
  test_with_malloc_tracking "single constructor tuple int*char*float" (fun () ->
    match free_stack_ m with
    | Foo (x, y, z) -> Printf.printf "Foo %d %c %f\n" x y (globalize_float z))

(* Tuple constructor variants - two constructors *)

type t35 = Foo of int * char | Bar of int * char
let g35a x y = malloc_ (Foo (x, y))
let g35b x y = malloc_ (Bar (x, y))

let print_t35 = function
  | Foo (x, y) -> Printf.printf "Foo %d %c\n" x y
  | Bar (x, y) -> Printf.printf "Bar %d %c\n" x y
let () =
  let m1 = g35a 1 'a' in
  let m2 = g35b 2 'b' in
  test_with_malloc_tracking "two tuple constructors int*char, Foo case" (fun () ->
    print_t35 (free_stack_ m1));
  test_with_malloc_tracking "two tuple constructors int*char, Bar case" (fun () ->
    print_t35 (free_stack_ m2))

type t36 = Foo of int * int | Bar of char * char
let g36a x y = malloc_ (Foo (x, y))
let g36b x y = malloc_ (Bar (x, y))

let print_t36 = function
  | Foo (x, y) -> Printf.printf "Foo %d %d\n" x y
  | Bar (x, y) -> Printf.printf "Bar %c %c\n" x y
let () =
  let m1 = g36a 1 2 in
  let m2 = g36b 'a' 'b' in
  test_with_malloc_tracking "two tuple constructors int*int vs char*char, Foo case" (fun () ->
    print_t36 (free_stack_ m1));
  test_with_malloc_tracking "two tuple constructors int*int vs char*char, Bar case" (fun () ->
    print_t36 (free_stack_ m2))

type t37 = Foo of int * int64# | Bar of char * float
let g37a x y = malloc_ (Foo (x, y))
let g37b x y = malloc_ (Bar (x, y))
let () =
  let m1 = g37a 1 #2L in
  let m2 = g37b 'a' 3.14 in
  test_with_malloc_tracking "two tuple constructors mixed types, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo (x, y) -> Printf.printf "Foo %d %Ld\n" x (Int64_u.to_int64 y)
    | Bar (x, y) -> Printf.printf "Bar %c %f\n" x (globalize_float y));
  test_with_malloc_tracking "two tuple constructors mixed types, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo (x, y) -> Printf.printf "Foo %d %Ld\n" x (Int64_u.to_int64 y)
    | Bar (x, y) -> Printf.printf "Bar %c %f\n" x (globalize_float y))

type t38 = Foo of int64# * int64# | Bar of float# * float#
let g38a x y = malloc_ (Foo (x, y))
let g38b x y = malloc_ (Bar (x, y))
let () =
  let m1 = g38a #1L #2L in
  let m2 = g38b #1.0 #2.0 in
  test_with_malloc_tracking "two tuple constructors unboxed types, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo (x, y) -> Printf.printf "Foo %Ld %Ld\n" (Int64_u.to_int64 x) (Int64_u.to_int64 y)
    | Bar (x, y) -> Printf.printf "Bar %f %f\n" (Float_u.to_float x) (Float_u.to_float y));
  test_with_malloc_tracking "two tuple constructors unboxed types, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo (x, y) -> Printf.printf "Foo %Ld %Ld\n" (Int64_u.to_int64 x) (Int64_u.to_int64 y)
    | Bar (x, y) -> Printf.printf "Bar %f %f\n" (Float_u.to_float x) (Float_u.to_float y))

(* Single value constructor variants *)

type t38a = Foo of int
let g38a_single x = malloc_ (Foo x)
let () =
  let m = g38a_single 42 in
  test_with_malloc_tracking "single value constructor int" (fun () ->
    match free_stack_ m with
    | Foo x -> Printf.printf "Foo %d\n" x)

type t38b = Foo of int64#
let g38b_single x = malloc_ (Foo x)
let () =
  let m = g38b_single #42L in
  test_with_malloc_tracking "single value constructor int64#" (fun () ->
    match free_stack_ m with
    | Foo x -> Printf.printf "Foo %Ld\n" (Int64_u.to_int64 x))

type t38c = Foo of float#
let g38c_single x = malloc_ (Foo x)
let () =
  let m = g38c_single #3.14 in
  test_with_malloc_tracking "single value constructor float#" (fun () ->
    match free_stack_ m with
    | Foo x -> Printf.printf "Foo %f\n" (Float_u.to_float x))

(* More two-constructor combinations *)

type t38d = Foo of int64# * int | Bar of float * float#
let g38da x y = malloc_ (Foo (x, y))
let g38db x y = malloc_ (Bar (x, y))
let () =
  let m1 = g38da #1L 2 in
  let m2 = g38db 3.0 #4.0 in
  test_with_malloc_tracking "two tuple constructors int64#*int vs float*float#, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo (x, y) -> Printf.printf "Foo %Ld %d\n" (Int64_u.to_int64 x) y
    | Bar (x, y) -> Printf.printf "Bar %f %f\n" (globalize_float x) (Float_u.to_float y));
  test_with_malloc_tracking "two tuple constructors int64#*int vs float*float#, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo (x, y) -> Printf.printf "Foo %Ld %d\n" (Int64_u.to_int64 x) y
    | Bar (x, y) -> Printf.printf "Bar %f %f\n" (globalize_float x) (Float_u.to_float y))

type t38e = Foo of float# * int64# | Bar of int * char
let g38ea x y = malloc_ (Foo (x, y))
let g38eb x y = malloc_ (Bar (x, y))
let () =
  let m1 = g38ea #1.0 #2L in
  let m2 = g38eb 3 'c' in
  test_with_malloc_tracking "two tuple constructors float#*int64# vs int*char, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo (x, y) -> Printf.printf "Foo %f %Ld\n" (Float_u.to_float x) (Int64_u.to_int64 y)
    | Bar (x, y) -> Printf.printf "Bar %d %c\n" x y);
  test_with_malloc_tracking "two tuple constructors float#*int64# vs int*char, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo (x, y) -> Printf.printf "Foo %f %Ld\n" (Float_u.to_float x) (Int64_u.to_int64 y)
    | Bar (x, y) -> Printf.printf "Bar %d %c\n" x y)

(* Mixed tuple/record constructor variants *)

type t39 = Foo of int * char | Bar of {x : int; y : int}
let g39a x y = malloc_ (Foo (x, y))
let g39b x y = malloc_ (Bar {x; y})
let () =
  let m1 = g39a 1 'a' in
  let m2 = g39b 2 3 in
  test_with_malloc_tracking "tuple vs record constructors, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo (x, y) -> Printf.printf "Foo %d %c\n" x y
    | Bar {x; y} -> Printf.printf "Bar %d %d\n" x y);
  test_with_malloc_tracking "tuple vs record constructors, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo (x, y) -> Printf.printf "Foo %d %c\n" x y
    | Bar {x; y} -> Printf.printf "Bar %d %d\n" x y)

type t40 = Foo of {x : int; y : char} | Bar of int * char
let g40a x y = malloc_ (Foo {x; y})
let g40b x y = malloc_ (Bar (x, y))
let () =
  let m1 = g40a 1 'a' in
  let m2 = g40b 2 'b' in
  test_with_malloc_tracking "record vs tuple constructors, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo {x; y} -> Printf.printf "Foo %d %c\n" x y
    | Bar (x, y) -> Printf.printf "Bar %d %c\n" x y);
  test_with_malloc_tracking "record vs tuple constructors, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo {x; y} -> Printf.printf "Foo %d %c\n" x y
    | Bar (x, y) -> Printf.printf "Bar %d %c\n" x y)

type t41 = Foo of int * int64# | Bar of {x : float; y : float#}
let g41a x y = malloc_ (Foo (x, y))
let g41b x y = malloc_ (Bar {x; y})
let () =
  let m1 = g41a 1 #2L in
  let m2 = g41b 3.0 #4.0 in
  test_with_malloc_tracking "mixed tuple vs mixed record constructors, Foo case" (fun () ->
    match free_stack_ m1 with
    | Foo (x, y) -> Printf.printf "Foo %d %Ld\n" x (Int64_u.to_int64 y)
    | Bar {x; y} -> Printf.printf "Bar %f %f\n" (globalize_float x) (Float_u.to_float y));
  test_with_malloc_tracking "mixed tuple vs mixed record constructors, Bar case" (fun () ->
    match free_stack_ m2 with
    | Foo (x, y) -> Printf.printf "Foo %d %Ld\n" x (Int64_u.to_int64 y)
    | Bar {x; y} -> Printf.printf "Bar %f %f\n" (globalize_float x) (Float_u.to_float y))

(* Multi-constructor variants - 4 constructors *)

type t42 = A of int | B of char | C of float | D of int64#
let g42a x = malloc_ (A x)
let g42b x = malloc_ (B x)
let g42c x = malloc_ (C x)
let g42d x = malloc_ (D x)

let print_t42 = function
  | A x -> Printf.printf "A %d\n" x
  | B x -> Printf.printf "B %c\n" x
  | C x -> Printf.printf "C %f\n" (globalize_float x)
  | D x -> Printf.printf "D %Ld\n" (Int64_u.to_int64 x)
let () =
  let m1 = g42a 1 in
  let m2 = g42b 'x' in
  let m3 = g42c 3.14 in
  let m4 = g42d #42L in
  test_with_malloc_tracking "four constructors single values, A case" (fun () ->
    print_t42 (free_stack_ m1));
  test_with_malloc_tracking "four constructors single values, B case" (fun () ->
    print_t42 (free_stack_ m2));
  test_with_malloc_tracking "four constructors single values, C case" (fun () ->
    print_t42 (free_stack_ m3));
  test_with_malloc_tracking "four constructors single values, D case" (fun () ->
    print_t42 (free_stack_ m4))

type t43 = Red of int * char | Green of {x : int; y : float} | Blue of float# * int64# | Yellow of int
let g43a x y = malloc_ (Red (x, y))
let g43b x y = malloc_ (Green {x; y})
let g43c x y = malloc_ (Blue (x, y))
let g43d x = malloc_ (Yellow x)

let print_t43 = function
  | Red (x, y) -> Printf.printf "Red %d %c\n" x y
  | Green {x; y} -> Printf.printf "Green %d %f\n" x (globalize_float y)
  | Blue (x, y) -> Printf.printf "Blue %f %Ld\n" (Float_u.to_float x) (Int64_u.to_int64 y)
  | Yellow x -> Printf.printf "Yellow %d\n" x
let () =
  let m1 = g43a 1 'r' in
  let m2 = g43b 10 2.5 in
  let m3 = g43c #3.14 #100L in
  let m4 = g43d 42 in
  test_with_malloc_tracking "four constructors mixed types, Red case" (fun () ->
    print_t43 (free_stack_ m1));
  test_with_malloc_tracking "four constructors mixed types, Green case" (fun () ->
    print_t43 (free_stack_ m2));
  test_with_malloc_tracking "four constructors mixed types, Blue case" (fun () ->
    print_t43 (free_stack_ m3));
  test_with_malloc_tracking "four constructors mixed types, Yellow case" (fun () ->
    print_t43 (free_stack_ m4))

(* Multi-constructor variants - 5 constructors *)

type t44 = One of int | Two of int * int | Three of int * int * int | Four of {a : int; b : int; c : int; d : int} | Five of float#
let g44a x = malloc_ (One x)
let g44b x y = malloc_ (Two (x, y))
let g44c x y z = malloc_ (Three (x, y, z))
let g44d a b c d = malloc_ (Four {a; b; c; d})
let g44e x = malloc_ (Five x)

let print_t44 = function
  | One x -> Printf.printf "One %d\n" x
  | Two (x, y) -> Printf.printf "Two %d %d\n" x y
  | Three (x, y, z) -> Printf.printf "Three %d %d %d\n" x y z
  | Four {a; b; c; d} -> Printf.printf "Four %d %d %d %d\n" a b c d
  | Five x -> Printf.printf "Five %f\n" (Float_u.to_float x)
let () =
  let m1 = g44a 1 in
  let m2 = g44b 2 3 in
  let m3 = g44c 4 5 6 in
  let m4 = g44d 7 8 9 10 in
  let m5 = g44e #2.71 in
  test_with_malloc_tracking "five constructors progression, One case" (fun () ->
    print_t44 (free_stack_ m1));
  test_with_malloc_tracking "five constructors progression, Two case" (fun () ->
    print_t44 (free_stack_ m2));
  test_with_malloc_tracking "five constructors progression, Three case" (fun () ->
    print_t44 (free_stack_ m3));
  test_with_malloc_tracking "five constructors progression, Four case" (fun () ->
    print_t44 (free_stack_ m4));
  test_with_malloc_tracking "five constructors progression, Five case" (fun () ->
    print_t44 (free_stack_ m5))

type t45 = Alpha of {x : int; y : char} | Beta of float * float# | Gamma of int64# * int * char | Delta of int | Epsilon of {mutable a : int; b : float#}
let g45a x y = malloc_ (Alpha {x; y})
let g45b x y = malloc_ (Beta (x, y))
let g45c x y z = malloc_ (Gamma (x, y, z))
let g45d x = malloc_ (Delta x)
let g45e a b = malloc_ (Epsilon {a; b})

let print_t45 = function
  | Alpha {x; y} -> Printf.printf "Alpha %d %c\n" x y
  | Beta (x, y) -> Printf.printf "Beta %f %f\n" (globalize_float x) (Float_u.to_float y)
  | Gamma (x, y, z) -> Printf.printf "Gamma %Ld %d %c\n" (Int64_u.to_int64 x) y z
  | Delta x -> Printf.printf "Delta %d\n" x
  | Epsilon {a; b} -> Printf.printf "Epsilon %d %f\n" a (Float_u.to_float b)
let () =
  let m1 = g45a 1 'a' in
  let m2 = g45b 2.0 #3.0 in
  let m3 = g45c #4L 5 'g' in
  let m4 = g45d 6 in
  let m5 = g45e 7 #8.0 in
  test_with_malloc_tracking "five constructors complex, Alpha case" (fun () ->
    print_t45 (free_stack_ m1));
  test_with_malloc_tracking "five constructors complex, Beta case" (fun () ->
    print_t45 (free_stack_ m2));
  test_with_malloc_tracking "five constructors complex, Gamma case" (fun () ->
    print_t45 (free_stack_ m3));
  test_with_malloc_tracking "five constructors complex, Delta case" (fun () ->
    print_t45 (free_stack_ m4));
  test_with_malloc_tracking "five constructors complex, Epsilon case" (fun () ->
    print_t45 (free_stack_ m5))
