(* TEST
 include stdlib_upstream_compatible;
  modules = "stubs.c";
  native;
*)

module Int64_u = Stdlib_upstream_compatible.Int64_u
module Float_u = Stdlib_upstream_compatible.Float_u

external get_malloc_bytes : unit -> int = "get_malloc_bytes" "get_malloc_bytes"

let test_with_malloc_tracking name f =
  let before = get_malloc_bytes () in
  f ();
  let after = get_malloc_bytes () in
  Printf.printf "%s: Allocation Delta: %d\n" name (after - before)

(* Tuples *)
let g x y = malloc_ (x,y)
let () =
  let m = g 2 3 in
  test_with_malloc_tracking "tuple" (fun () ->
    let #(a,b) = free_ m in
    Printf.printf "%d %d\n" a b)

let g x y z = malloc_ (x,y,z)
let () =
  let m = g 2 3 4 in
  test_with_malloc_tracking "tuple3" (fun () ->
    let #(a,b,c) = free_ m in
    Printf.printf "%d %d %d\n" a b c)

type t1 = {x : int}
let g1 x = malloc_ {x}
let () =
  let m = g1 2 in
  test_with_malloc_tracking "singleton record" (fun () ->
    let #{x} = free_ m in
    Printf.printf "%d\n" x)

type t2 = {x : int; y : int}
let g2 x y = malloc_ {x;y}
let () =
  let m = g2 2 3 in
  test_with_malloc_tracking "two-element record" (fun () ->
    let #{x;y} = free_ m in
    Printf.printf "%d %d\n" x y)

type t3 = {x : int; y : int64#}
let g3 x y = malloc_ {x;y}
let () =
  let m = g3 2 #3L in
  test_with_malloc_tracking "mixed record" (fun () ->
    let #{x;y} = free_ m in
    Printf.printf "%d %Ld\n" x (Int64_u.to_int64 y))

type t4 = {x : int64#; y : int}
let g4 x y = malloc_ {x;y}
let () =
  let m = g4 #2L 3 in
  test_with_malloc_tracking "mixed record 2" (fun () ->
    let #{x;y} = free_ m in
    Printf.printf "%Ld %d\n" (Int64_u.to_int64 x) y)

type t5 = {x : int64#; y : int64#}
let g5 x y = malloc_ {x;y}
let () =
  let m = g5 #2L #3L in
  test_with_malloc_tracking "mixed record all unboxed" (fun () ->
    let #{x;y} = free_ m in
    Printf.printf "%Ld %Ld\n" (Int64_u.to_int64 x) (Int64_u.to_int64 y))
