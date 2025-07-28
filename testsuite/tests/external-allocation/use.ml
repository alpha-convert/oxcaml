(* TEST
  native;
*)
module Aliased : sig
  type 'a t = {aliased : 'a @@ aliased} [@@unboxed]
end = struct
  type 'a t = {aliased : 'a @@ aliased} [@@unboxed]
end

external use : 'a mallocd @ local unique -> ('a @ local external_ -> 'b) -> #('b Aliased.t * 'a mallocd) @ unique = "%use_mallocd"


let print_and_add x y =
  print_endline (Int.to_string x);
  print_endline (Int.to_string y);
  x + y


(* You can use malloc'd items of all the different mallocable types *)
let [@inline never] f (m : (int * int) mallocd) =
  let #(r,_) = use m @@ fun (x,y) -> print_and_add x y in
  print_endline (Int.to_string r.aliased);
  ()

let () =
  print_endline "pair:";
  f (malloc_ (2,3));
  print_endline "\n"

type t0 = {x : int; y : int}
let f (m : t0 mallocd) =
  let #(r,_) = use m @@ fun {x;y} -> print_and_add x y in
  print_endline (Int.to_string r.aliased);
  ()

let () =
  print_endline "record:";
  f (malloc_ {x = 2;y = 3});
  print_endline "\n"

type t1 = Foo of {x : int; y : int}
let f (m : t1 mallocd) =
  let #(r,_) = use m @@ fun (Foo {x;y}) -> print_and_add x y in
  print_endline (Int.to_string r.aliased);
  ()

let () =
  print_endline "constructor with record field:";
  f (malloc_ (Foo {x = 2;y = 3}));
  print_endline "\n"


let f x y =
  let #(r,m) = use (malloc_ (x,y)) @@ fun (a,b) ->
    print_endline (Int.to_string (a + b));
    a * b
  in
  print_endline (Int.to_string (r.aliased + 1));
  let #(_,m') = use m @@ fun (a,b) ->
    print_endline (Int.to_string (a * b))
  in
  m'

let _ = f 2 3

(*
type 'a lst = 'a cell mallocd
and 'a cell = Cons of {hd : 'a; tl : 'a lst} | Nil of unit

let rec alloc_list (xs : int list @ internal) : int lst @ unique =
  match xs with
  | [] -> malloc_ (Nil ())
  | y :: ys ->
    let tl = alloc_list ys in
    malloc_ (Cons {hd = y; tl = tl})

let rec iter ~f xs =
  let #(_,xs) = use xs @@ fun cell ->
    (match cell with
    | Nil () -> ()
    | Cons {hd;tl} ->
        f hd;
        let _ = iter ~f tl in
        ())
  in
  xs

let _ =
  iter ~f:(fun i -> print_endline (Int.to_string i)) (alloc_list [1;2;3;4;5;6])
  (* use mutable, immutable, mixed, multiple things at thesame time (nested, sequential). *)
  Get the GC to run in the middle of use and make sure it's all fine *)
