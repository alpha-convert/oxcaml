(* TEST
 expect;
*)

(* Type must be mallocd *)
let f (x : int) =
  free_ x
[%%expect {|
Line 2, characters 8-9:
2 |   free_ x
            ^
Error: This expression has type "int" but an expression was expected of type
         "'a mallocd"
|}]

(* Free requires its argument uniquely *)
let f (x @ aliased) = free_ x
[%%expect{|
Line 1, characters 28-29:
1 | let f (x @ aliased) = free_ x
                                ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* Cannot free thing of unknown type... *)
let f (x : 'a mallocd) = free_ x
[%%expect{|
Line 1, characters 31-32:
1 | let f (x : 'a mallocd) = free_ x
                                   ^
Error: Cannot free values of type "'a"
|}]

(* The typechecking of free is annoyingly path-dependent. *)
let f (x : 'a mallocd) (y : 'a) =
  let (a,b) = y in
  let _ = free_ x in
  a + b
[%%expect {|
val f : (int * int) mallocd @ unique -> int * int -> int = <fun>
|}, Principal{|
Line 3, characters 10-17:
3 |   let _ = free_ x in
              ^^^^^^^
Warning 18 [not-principal]: this use of free is not principal.

val f : (int * int) mallocd @ unique -> int * int -> int = <fun>
|}]

let f (x : 'a mallocd) (y : 'a) =
  ignore (free_ x);
  let (a,b) = y in
  a + b
[%%expect {|
Line 2, characters 16-17:
2 |   ignore (free_ x);
                    ^
Error: Cannot free values of type "'a"
|}]

(*
Successful free-to-unbox of tuples
*)
let f (x : (_ * _) mallocd)  = free_ x
[%%expect {|
val f : ('a * 'b) mallocd @ unique -> #('a * 'b) = <fun>
|}]

let f (x : (_ * _ * _) mallocd)  = free_ x
[%%expect {|
val f : ('a * 'b * 'c) mallocd @ unique -> #('a * 'b * 'c) = <fun>
|}]

type 'a t = 'a * 'a
let f (x : 'a t mallocd)  = free_ x
[%%expect {|
type 'a t = 'a * 'a
val f : 'a t mallocd @ unique -> #('a * 'a) = <fun>
|}]

type 'a t = 'a * 'a * 'a
let f (x : (_ * _ * _) mallocd)  = free_ x
[%%expect {|
type 'a t = 'a * 'a * 'a
val f : ('a * 'b * 'c) mallocd @ unique -> #('a * 'b * 'c) = <fun>
|}]

(* Succssful free-to-unbox of regular records of various sorts... *)
type t = {x : int; y : string @@ external_}
let f (x : t mallocd)  = free_ x
[%%expect {|
type t = { x : int; y : string @@ external_; }
val f : t mallocd @ unique -> t# = <fun>
|}]

type t = {x : int; y : string}
let f (x : t mallocd)  = free_ x
[%%expect {|
type t = { x : int; y : string; }
val f : t mallocd @ unique -> t# = <fun>
|}]

type t = {mutable x : int; mutable y : string @@ external_}
let f (x : t mallocd)  = free_ x
[%%expect {|
type t = { mutable x : int; mutable y : string @@ external_; }
val f : t mallocd @ unique -> t# = <fun>
|}]

type t = {mutable x : int64#; mutable y : string @@ external_}
let f (x : t mallocd)  = free_ x
[%%expect {|
type t = { mutable x : int64#; mutable y : string @@ external_; }
val f : t mallocd @ unique -> t# = <fun>
|}]

type t = {mutable x : int64#; y : float#}
let f (x : t mallocd)  = free_ x
[%%expect {|
type t = { mutable x : int64#; y : float#; }
val f : t mallocd @ unique -> t# = <fun>
|}]

(* Some things have unbxoed versions even though we can'externally t allocate
them yet, but it's fine to say that they can be free.
*)
let f (x : float mallocd) = free_ x
[%%expect {|
val f : float mallocd @ unique -> float# = <fun>
|}]

let f (x : int64 mallocd) = free_ x
[%%expect {|
val f : int64 mallocd @ unique -> int64# = <fun>
|}]

let f (x : int32 mallocd) = free_ x
[%%expect {|
val f : int32 mallocd @ unique -> int32# = <fun>
|}]

type t = float
let f (x : t mallocd) = free_ x
[%%expect {|
type t = float
val f : t mallocd @ unique -> float# = <fun>
|}]

type t = int64
let f (x : t mallocd) = free_ x
[%%expect {|
type t = int64
val f : t mallocd @ unique -> int64# = <fun>
|}]

type t = int32
let f (x : t mallocd) = free_ x
[%%expect {|
type t = int32
val f : t mallocd @ unique -> int32# = <fun>
|}]

(* Finding the unboxed version of a type works deeply through modules,
if the type is exposed in the signature *)
module M : sig
  type t = int * int
end = struct
  type t = int * int
end

let f (x : M.t mallocd) = free_ x
[%%expect{|
module M : sig type t = int * int end
val f : M.t mallocd @ unique -> #(int * int) = <fun>
|}]

module M : sig
  type t = {x : int; y: int}
end = struct
  type t = {x : int; y: int}
end

let f (x : M.t mallocd) = free_ x
[%%expect{|
module M : sig type t = { x : int; y : int; } end
val f : M.t mallocd @ unique -> M.t# = <fun>
|}]

module M : sig
  type t
end = struct
  type t = int * int
end

let f (x : M.t mallocd) = free_ x
[%%expect{|
module M : sig type t end
Line 7, characters 32-33:
7 | let f (x : M.t mallocd) = free_ x
                                    ^
Error: Type "M.t" does not have an unboxed version, try free_stack_
|}]

module M : sig
  type t
end = struct
  type t = {x : int; y: int}
end

let f (x : M.t mallocd) = free_ x
[%%expect{|
module M : sig type t end
Line 7, characters 32-33:
7 | let f (x : M.t mallocd) = free_ x
                                    ^
Error: Type "M.t" does not have an unboxed version, try free_stack_
|}]


(* Cannot unboxed-free sum types, they do not have unboxed versions,
even those with definite shape *)
type t = Foo of int
let f (x : t mallocd) = free_ x
[%%expect {|
type t = Foo of int
Line 2, characters 30-31:
2 | let f (x : t mallocd) = free_ x
                                  ^
Error: Type "t" does not have an unboxed version, try free_stack_
|}]

type t = Foo of int | Bar of char
let f (x : t mallocd) = free_ x
[%%expect {|
type t = Foo of int | Bar of char
Line 2, characters 30-31:
2 | let f (x : t mallocd) = free_ x
                                  ^
Error: Type "t" does not have an unboxed version, try free_stack_
|}]

(* GADTs work the same as variants *)
type t = Pack : 'a. 'a -> t
let f (t : t mallocd) = free_ t
[%%expect{|
type t = Pack : 'a -> t
Line 2, characters 30-31:
2 | let f (t : t mallocd) = free_ t
                                  ^
Error: Type "t" does not have an unboxed version, try free_stack_
|}]

type t = Pack : 'a. 'a -> t | Secret_other_option : int -> t
let f (t : t mallocd) = free_ t
[%%expect{|
type t = Pack : 'a -> t | Secret_other_option : int -> t
Line 2, characters 30-31:
2 | let f (t : t mallocd) = free_ t
                                  ^
Error: Type "t" does not have an unboxed version, try free_stack_
|}]

(* Cannot unboxed-free a polymorphic variant, even with definite shape *)

let f (x : [`Foo of int] mallocd) = free_ x
[%%expect{|
Line 1, characters 42-43:
1 | let f (x : [`Foo of int] mallocd) = free_ x
                                              ^
Error: Type "[ `Foo of int ]" does not have an unboxed version, try free_stack_
|}]

let f (x : [`Foo of int | `Bar of int * int] mallocd) = free_ x
[%%expect{|
Line 1, characters 62-63:
1 | let f (x : [`Foo of int | `Bar of int * int] mallocd) = free_ x
                                                                  ^
Error: Type "[ `Bar of int * int | `Foo of int ]" does not have an unboxed version, try free_stack_
|}]

type t = [`Foo of int]
let f (x : t mallocd) = free_ x
[%%expect{|
type t = [ `Foo of int ]
Line 2, characters 30-31:
2 | let f (x : t mallocd) = free_ x
                                  ^
Error: Type "t" does not have an unboxed version, try free_stack_
|}]

type t = [`Foo of int | `Bar of int * int]
let f (x : t mallocd) = free_ x
[%%expect{|
type t = [ `Bar of int * int | `Foo of int ]
Line 2, characters 30-31:
2 | let f (x : t mallocd) = free_ x
                                  ^
Error: Type "t" does not have an unboxed version, try free_stack_
|}]

(* Other types one cannot free *)
let f (x : (int -> int) mallocd) = free_ x
[%%expect{|
Line 1, characters 41-42:
1 | let f (x : (int -> int) mallocd) = free_ x
                                             ^
Error: Cannot free values of type "int -> int"
|}]

let f (x : <get : int; set : int> mallocd) = free_ x
[%%expect{|
Line 1, characters 51-52:
1 | let f (x : <get : int; set : int> mallocd) = free_ x
                                                       ^
Error: Cannot free values of type "< get : int; set : int >"
|}]

type t
let f (x : t mallocd) = free_ x
[%%expect{|
type t
Line 2, characters 30-31:
2 | let f (x : t mallocd) = free_ x
                                  ^
Error: Type "t" does not have an unboxed version, try free_stack_
|}]

let f (x : int mallocd) =
  free_ x
[%%expect {|
Line 2, characters 8-9:
2 |   free_ x
            ^
Error: Type "int" does not have an unboxed version, try free_stack_
|}]

let f (t : (int -> int) mallocd) = free_ t
[%%expect{|
Line 1, characters 41-42:
1 | let f (t : (int -> int) mallocd) = free_ t
                                             ^
Error: Cannot free values of type "int -> int"
|}]

let f (t : < get : int > mallocd) = free_ t
[%%expect{|
Line 1, characters 42-43:
1 | let f (t : < get : int > mallocd) = free_ t
                                              ^
Error: Cannot free values of type "< get : int >"
|}]

let f (t : [`Foo of int | `Bar of char] mallocd) = free_ t
[%%expect{|
Line 1, characters 57-58:
1 | let f (t : [`Foo of int | `Bar of char] mallocd) = free_ t
                                                             ^
Error: Type "[ `Bar of char | `Foo of int ]" does not have an unboxed version, try free_stack_
|}]
