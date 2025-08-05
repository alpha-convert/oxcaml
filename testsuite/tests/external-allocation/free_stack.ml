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
  Printf.printf "%s: %s\n" name msg

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
