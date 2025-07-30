(* TEST
 include stdlib_upstream_compatible;
  native;
*)

module Int64_u = Stdlib_upstream_compatible.Int64_u

external use : 'a mallocd @ local unique -> ('a @ local external_ -> 'b @ unique) @ local once -> #('b * 'a mallocd) @ unique = "%use_mallocd"

let print_and_add x y =
  print_endline (Int.to_string x);
  print_endline (Int.to_string y);
  x + y

(* You can use malloc'd items of all the different mallocable types *)
let [@inline never] f (m : (int * int) mallocd) =
  let #(r,_) = use m @@ fun (x,y) -> print_and_add x y in
  print_endline (Int.to_string r);
  ()

let () =
  print_endline "pair:";
  f (malloc_ (2,3));
  print_endline "\n"

type t1 = {x : int; y : int}
let f (m : t1 mallocd) =
  let #(r,_) = use m @@ fun {x;y} -> print_and_add x y in
  print_endline (Int.to_string r);
  ()

let () =
  print_endline "record:";
  f (malloc_ {x = 2;y = 3});
  print_endline "\n"

type t2 = Foo of {x : int; y : int}
let f (m : t2 mallocd) =
  let #(r,_) = use m @@ fun (Foo {x;y}) -> print_and_add x y in
  print_endline (Int.to_string r);
  ()

let () =
  print_endline "constructor with record field:";
  f (malloc_ (Foo {x = 2;y = 3}));
  print_endline "\n"

type t3 = Foo of int
let f (m : t3 mallocd) =
  let #(r,_) = use m @@ fun (Foo x) -> print_and_add x x in
  print_endline (Int.to_string r);
  ()

let () =
  print_endline "constructor with int field:";
  f (malloc_ (Foo 2));
  print_endline "\n"

type t4 = Foo of int * int
let f (m : t4 mallocd) =
  let #(r,_) = use m @@ fun (Foo (x,y)) -> print_and_add x y in
  print_endline (Int.to_string r);
  ()

let () =
  print_endline "constructor with tuple field:";
  f (malloc_ (Foo (2,3)));
  print_endline "\n"

type t5 = Foo of int * int | Bar of int
let f (m : t5 mallocd) =
  let #(r,_) = use m @@ fun t ->
    match t with
    | Foo(x,y) -> print_and_add x y
    | Bar x -> print_and_add x x
  in
  print_endline (Int.to_string r);
  ()

let () =
  print_endline "type with two constructors";
  f (malloc_ (Foo (2,3)));
  print_endline "and";
  f (malloc_ (Bar 3));
  print_endline "\n"

type t6 = FooRec of {a : int; b : int} | BarRec of {x : int; y : int}
let f (m : t6 mallocd) =
  let #(r,_) = use m @@ fun t ->
    match t with
    | FooRec {a; b} -> print_and_add a b
    | BarRec {x; y} -> print_and_add x y
  in
  print_endline (Int.to_string r);
  ()

let () =
  print_endline "two constructors both with record arguments";
  f (malloc_ (FooRec {a = 2; b = 3}));
  print_endline "and";
  f (malloc_ (BarRec {x = 4; y = 5}));
  print_endline "\n"

type t7 = FooRec2 of {a : int; b : int} | BarTuple of int * int
let f (m : t7 mallocd) =
  let #(r,_) = use m @@ fun t ->
    match t with
    | FooRec2 {a; b} -> print_and_add a b
    | BarTuple (x, y) -> print_and_add x y
  in
  print_endline (Int.to_string r);
  ()

let () =
  print_endline "constructor with record vs tuple";
  f (malloc_ (FooRec2 {a = 2; b = 3}));
  print_endline "and";
  f (malloc_ (BarTuple (4, 5)));
  print_endline "\n"

(* One constructor with record, other with int *)
type t8 = FooRec3 of {a : int; b : int} | BarInt of int
let f (m : t8 mallocd) =
  let #(r,_) = use m @@ fun t ->
    match t with
    | FooRec3 {a; b} -> print_and_add a b
    | BarInt x -> print_and_add x x
  in
  print_endline (Int.to_string r);
  ()

let () =
  print_endline "constructor with record vs int";
  f (malloc_ (FooRec3 {a = 2; b = 3}));
  print_endline "and";
  f (malloc_ (BarInt 4));
  print_endline "\n"

type t9 = {x : int64#; y : int64#;}
let f (m : t9 mallocd) =
  let #(r,_) = use m @@ fun {x;y} ->
    print_and_add (Int64_u.to_int x) (Int64_u.to_int y);
  in
  print_endline (Int.to_string r);
  ()

let () =
  print_endline "record with unboxed fields:";
  f (malloc_ ({x = #2L;y = #3L}));
  print_endline "\n"

type t10 = {x : int; y : int64#;}
let f (m : t10 mallocd) =
  let #(r,_) = use m @@ fun {x;y} ->
    print_and_add x (Int64_u.to_int y);
  in
  print_endline (Int.to_string r);
  ()

let () =
  print_endline "mixed record:";
  f (malloc_ ({x = 2;y = #3L}));
  print_endline "\n"

type t11 = {mutable x : int; mutable y : int}
let f (m : t11 mallocd) =
  let #(r,m) = use m @@ fun r ->
    ignore (print_and_add r.x r.y);
    r.x <- r.x + 1;
    r.y <- r.y + 1;
    ()
  in
  let #(r,_) = use m @@ fun {x;y} ->
    print_and_add x y;
  in
  print_endline (Int.to_string r);
  ()

let () =
  print_endline "record with mutable fields, used twice:";
  f (malloc_ ({x = 2;y = 3}));
  print_endline "\n"

type t12 = {mutable x : int; mutable y : int64#}
let f (m : t12 mallocd) =
  let #(r,m) = use m @@ fun r ->
    ignore (print_and_add r.x (Int64_u.to_int r.y));
    r.x <- r.x + 1;
    r.y <- Int64_u.add r.y #1L;
    ()
  in
  let #(r,_) = use m @@ fun {x;y} ->
    print_and_add x (Int64_u.to_int y);
  in
  print_endline (Int.to_string r);
  ()

let () =
  print_endline "mixed record with mutable fields, used twice:";
  f (malloc_ ({x = 2;y = #3L}));
  print_endline "\n"

let f (m : [`Foo of int] mallocd) =
  let #(r,_) = use m @@ fun (`Foo x) -> print_and_add x x in
  print_endline (Int.to_string r);
  ()

let () =
  print_endline "polymorphic variant:";
  f (malloc_ (`Foo 2));
  print_endline "\n"


(*** Nested use tests ***)

(* Two tuples nested *)
let nested_tuples (m1 : (int * int) mallocd) (m2 : (int * int) mallocd) =
  let #(r1, m1') = use m1 @@ fun (a, b) ->
    let #(r2, _) = use m2 @@ fun (c, d) ->
      print_and_add (a + c) (b + d)
    in
    print_endline (Int.to_string r2);
    a * b
  in
  print_endline (Int.to_string r1);
  ()

let () =
  print_endline "nested tuples:";
  nested_tuples (malloc_ (2, 3)) (malloc_ (4, 5));
  print_endline "\n"

(* Two records nested *)
let nested_records (m1 : t1 mallocd) (m2 : t1 mallocd) =
  let #(r1, _) = use m1 @@ fun {x = x1; y = y1} ->
    let #(r2, _) = use m2 @@ fun {x = x2; y = y2} ->
      print_and_add (x1 + x2) (y1 + y2)
    in
    print_endline (Int.to_string r2);
    x1 * y1
  in
  print_endline (Int.to_string r1);
  ()

let () =
  print_endline "nested records:";
  nested_records (malloc_ {x = 2; y = 3}) (malloc_ {x = 4; y = 5});
  print_endline "\n"

(* Two constructors nested *)
let nested_constructors (m1 : t3 mallocd) (m2 : t3 mallocd) =
  let #(r1, _) = use m1 @@ fun (Foo x1) ->
    let #(r2, _) = use m2 @@ fun (Foo x2) ->
      print_and_add x1 x2
    in
    print_endline (Int.to_string r2);
    x1 * 2
  in
  print_endline (Int.to_string r1);
  ()

let () =
  print_endline "nested constructors:";
  nested_constructors (malloc_ (Foo 3)) (malloc_ (Foo 7));
  print_endline "\n"

(* Tuple and record nested *)
let nested_tuple_record (m1 : (int * int) mallocd) (m2 : t1 mallocd) =
  let #(r1, _) = use m1 @@ fun (a, b) ->
    let #(r2, _) = use m2 @@ fun {x; y} ->
      print_and_add (a + x) (b + y)
    in
    print_endline (Int.to_string r2);
    a + b
  in
  print_endline (Int.to_string r1);
  ()

let () =
  print_endline "nested tuple and record:";
  nested_tuple_record (malloc_ (2, 3)) (malloc_ {x = 4; y = 5});
  print_endline "\n"

(* Record and constructor nested *)
let nested_record_constructor (m1 : t1 mallocd) (m2 : t2 mallocd) =
  let #(r1, _) = use m1 @@ fun {x; y} ->
    let #(r2, _) = use m2 @@ fun (Foo {x = x2; y = y2}) ->
      print_and_add (x + x2) (y + y2)
    in
    print_endline (Int.to_string r2);
    x * y
  in
  print_endline (Int.to_string r1);
  ()

let () =
  print_endline "nested record and constructor:";
  nested_record_constructor (malloc_ {x = 3; y = 4}) (malloc_ (Foo {x = 5; y = 6}));
  print_endline "\n"

(* Constructor and tuple nested *)
let nested_constructor_tuple (m1 : t4 mallocd) (m2 : (int * int) mallocd) =
  let #(r1, _) = use m1 @@ fun (Foo (x1, y1)) ->
    let #(r2, _) = use m2 @@ fun (x2, y2) ->
      print_and_add (x1 + x2) (y1 + y2)
    in
    print_endline (Int.to_string r2);
    x1 * y1
  in
  print_endline (Int.to_string r1);
  ()

let () =
  print_endline "nested constructor and tuple:";
  nested_constructor_tuple (malloc_ (Foo (2, 3))) (malloc_ (4, 5));
  print_endline "\n"

(* Mutable record nested with mutation *)
let nested_mutable_records (m1 : t11 mallocd) (m2 : t11 mallocd) =
  let #(r1, m1') = use m1 @@ fun r1 ->
    let initial_sum = r1.x + r1.y in
    r1.x <- r1.x + 10;
    r1.y <- r1.y + 20;
    let #(r2, m2') = use m2 @@ fun r2 ->
      let inner_sum = r2.x + r2.y in
      r2.x <- r2.x * 2;
      r2.y <- r2.y * 3;
      print_and_add initial_sum inner_sum
    in
    print_endline (Int.to_string r2);
    (* Use the mutated m2 again *)
    let #(_ , _) = use m2' @@ fun r2_mutated ->
      print_endline ("Final m2 state : " ^ Int.to_string r2_mutated.x ^ ", " ^ Int.to_string r2_mutated.y)
    in
    ()
  in
  (* Use the mutated m1 again *)
  let #(_, _) = use m1' @@ fun r1_mutated ->
    print_endline ("Final m1 state: " ^ Int.to_string r1_mutated.x ^ ", " ^ Int.to_string r1_mutated.y);
  in
  ()

let () =
  print_endline "nested mutable records with mutations:";
  nested_mutable_records (malloc_ {x = 1; y = 2}) (malloc_ {x = 3; y = 4});
  print_endline "\n"

(* Mixed types with unboxed fields nested *)
let nested_mixed_records (m1 : t10 mallocd) (m2 : t9 mallocd) =
  let #(r1, _) = use m1 @@ fun {x; y} ->
    let #(r2, _) = use m2 @@ fun {x = x2; y = y2} ->
      print_and_add x (Int64_u.to_int y + Int64_u.to_int x2 + Int64_u.to_int y2)
    in
    print_endline (Int.to_string r2);
    x + Int64_u.to_int y
  in
  print_endline (Int.to_string r1);
  ()

let () =
  print_endline "nested mixed records:";
  nested_mixed_records (malloc_ {x = 5; y = #10L}) (malloc_ {x = #15L; y = #20L});
  print_endline "\n"

(*** Sequential use tests ***)

(* Sequential use with mutable record - two uses *)
let f (m : t11 mallocd) =
  print_endline "First use:";
  let #(r1, m) = use m @@ fun r ->
    print_endline ("Initial: x=" ^ Int.to_string r.x ^ ", y=" ^ Int.to_string r.y);
    r.x <- r.x + 1;
    r.y <- r.y + 1;
    r.x + r.y
  in
  print_endline ("Result 1: " ^ Int.to_string r1);

  print_endline "Second use:";
  let #(r2, _) = use m @@ fun r ->
    print_endline ("After first mutation: x=" ^ Int.to_string r.x ^ ", y=" ^ Int.to_string r.y);
    r.x <- r.x + 1;
    r.y <- r.y + 1;
    r.x + r.y
  in
  print_endline ("Result 2: " ^ Int.to_string r2);
  ()

let () =
  print_endline "sequential mutable record - two uses:";
  f (malloc_ {x = 1; y = 2});
  print_endline "\n"

(* Sequential use with mutable record - three uses *)
let f (m : t11 mallocd) =
  print_endline "First use:";
  let #(r1, m) = use m @@ fun r ->
    print_endline ("Initial: x=" ^ Int.to_string r.x ^ ", y=" ^ Int.to_string r.y);
    r.x <- r.x + 1;
    r.y <- r.y + 1;
    r.x + r.y
  in
  print_endline ("Result 1: " ^ Int.to_string r1);

  print_endline "Second use:";
  let #(r2, m) = use m @@ fun r ->
    print_endline ("After first: x=" ^ Int.to_string r.x ^ ", y=" ^ Int.to_string r.y);
    r.x <- r.x + 1;
    r.y <- r.y + 1;
    r.x + r.y
  in
  print_endline ("Result 2: " ^ Int.to_string r2);

  print_endline "Third use:";
  let #(r3, _) = use m @@ fun r ->
    print_endline ("After second: x=" ^ Int.to_string r.x ^ ", y=" ^ Int.to_string r.y);
    r.x <- r.x + 1;
    r.y <- r.y + 1;
    r.x + r.y
  in
  print_endline ("Result 3: " ^ Int.to_string r3);
  ()

let () =
  print_endline "sequential mutable record - three uses:";
  f (malloc_ {x = 1; y = 2});
  print_endline "\n"

(* Constructor with mutable record field *)
type t13 = MutRec of {mutable a : int; mutable b : int}

let f (m : t13 mallocd) =
  print_endline "First use:";
  let #(r1, m) = use m @@ fun (MutRec r) ->
    print_endline ("Initial: a=" ^ Int.to_string r.a ^ ", b=" ^ Int.to_string r.b);
    r.a <- r.a + 1;
    r.b <- r.b + 1;
    r.a + r.b
  in
  print_endline ("Result 1: " ^ Int.to_string r1);

  print_endline "Second use:";
  let #(r2, m) = use m @@ fun (MutRec r) ->
    print_endline ("After first: a=" ^ Int.to_string r.a ^ ", b=" ^ Int.to_string r.b);
    r.a <- r.a + 1;
    r.b <- r.b + 1;
    r.a + r.b
  in
  print_endline ("Result 2: " ^ Int.to_string r2);

  print_endline "Third use:";
  let #(r3, _) = use m @@ fun (MutRec r) ->
    print_endline ("After second: a=" ^ Int.to_string r.a ^ ", b=" ^ Int.to_string r.b);
    r.a <- r.a + 1;
    r.b <- r.b + 1;
    r.a + r.b
  in
  print_endline ("Result 3: " ^ Int.to_string r3);
  ()

let () =
  print_endline "sequential constructor with mutable record:";
  f (malloc_ (MutRec {a = 1; b = 2}));
  print_endline "\n"

(* Mixed sequential uses - different record types *)
let f (m1 : t11 mallocd) (m2 : t12 mallocd) =
  print_endline "First record, first use:";
  let #(r1, m1) = use m1 @@ fun r ->
    print_endline ("t11 initial: x=" ^ Int.to_string r.x ^ ", y=" ^ Int.to_string r.y);
    r.x <- r.x + 1;
    r.y <- r.y + 1;
    r.x + r.y
  in
  print_endline ("t11 result 1: " ^ Int.to_string r1);

  print_endline "Second record, first use:";
  let #(r2, m2) = use m2 @@ fun r ->
    print_endline ("t12 initial: x=" ^ Int.to_string r.x ^ ", y=" ^ Int64_u.to_string r.y);
    r.x <- r.x + 1;
    r.y <- Int64_u.add r.y #1L;
    r.x + Int64_u.to_int r.y
  in
  print_endline ("t12 result 1: " ^ Int.to_string r2);

  print_endline "First record, second use:";
  let #(r3, _) = use m1 @@ fun r ->
    print_endline ("t11 after first: x=" ^ Int.to_string r.x ^ ", y=" ^ Int.to_string r.y);
    r.x <- r.x + 1;
    r.y <- r.y + 1;
    r.x + r.y
  in
  print_endline ("t11 result 2: " ^ Int.to_string r3);

  print_endline "Second record, second use:";
  let #(r4, _) = use m2 @@ fun r ->
    print_endline ("t12 after first: x=" ^ Int.to_string r.x ^ ", y=" ^ Int64_u.to_string r.y);
    r.x <- r.x + 1;
    r.y <- Int64_u.add r.y #1L;
    r.x + Int64_u.to_int r.y
  in
  print_endline ("t12 result 2: " ^ Int.to_string r4);
  ()

let () =
  print_endline "sequential mixed mutable records:";
  f (malloc_ {x = 1; y = 2}) (malloc_ {x = 1; y = #2L});
  print_endline "\n"

(*** GC during and around use tests ***)

(* Sequential use with GC between uses *)
let f (m : t11 mallocd) =
  print_endline "First use:";
  let #(_, m) = use m @@ fun r ->
    print_endline ("Initial: x=" ^ Int.to_string r.x ^ ", y=" ^ Int.to_string r.y);
    r.x <- r.x + 1;
    r.y <- r.y + 1;
    ()
  in
  print_endline "Running GC...";
  Gc.full_major ();
  let #(_, _) = use m @@ fun r ->
    print_endline ("After GC: x=" ^ Int.to_string r.x ^ ", y=" ^ Int.to_string r.y);
    r.x <- r.x + 1;
    r.y <- r.y + 1;
    ()
  in
  ()

let () =
  print_endline "sequential use with GC between uses:";
  f (malloc_ {x = 1; y = 2});
  print_endline "\n"

(* Nested use with GCs *)
let f (m1 : t11 mallocd) (m2 : t11 mallocd) =
  let #(_, _) = use m1 @@ fun r1 ->
    print_endline ("Outer initial: x=" ^ Int.to_string r1.x ^ ", y=" ^ Int.to_string r1.y);
    Gc.full_major ();
    r1.x <- r1.x + 1;
    r1.y <- r1.y + 1;
    Gc.full_major ();

    let #(_, _) = use m2 @@ fun r2 ->
      Gc.full_major ();
      print_endline ("Inner after GC: x=" ^ Int.to_string r2.x ^ ", y=" ^ Int.to_string r2.y);
      r2.x <- r2.x + 1;
      r2.y <- r2.y + 1;
      Gc.full_major ();
      ()
    in
    ()
  in
  ()

let () =
  print_endline "nested use with GCs:";
  f (malloc_ {x = 1; y = 2}) (malloc_ {x = 1; y = 2});
  print_endline "\n"
