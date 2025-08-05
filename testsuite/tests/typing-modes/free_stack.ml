(* TEST
 expect;
*)

(* Type must be mallocd *)
let f (x : int) =
  free_stack_ x
[%%expect {|
Line 2, characters 14-15:
2 |   free_stack_ x
                  ^
Error: This expression has type "int" but an expression was expected of type
         "'a mallocd"
|}]

(* Freestack also requires its argument uniquely,
  this is checked first, before the type *)
let f (x @ aliased) = free_stack_ x
[%%expect{|
Line 1, characters 34-35:
1 | let f (x @ aliased) = free_stack_ x
                                      ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* free_stack returns locally *)
let f(x : (_ * _) mallocd ) =
  let _ @ global = free_stack_ x in
  ()
[%%expect{|
Line 2, characters 19-32:
2 |   let _ @ global = free_stack_ x in
                       ^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

let f(x : (_ * _) mallocd ) =
  let _ @ local = free_stack_ x in
  ()
[%%expect{|
val f : ('a * 'b) mallocd @ unique -> unit = <fun>
|}, Principal{|
Line 2, characters 18-31:
2 |   let _ @ local = free_stack_ x in
                      ^^^^^^^^^^^^^
Warning 18 [not-principal]: typing this term eagerly matches on the type 'a * 'b, which is not principal.

val f : ('a * 'b) mallocd @ unique -> unit = <fun>
|}]

(* Cannot free thing of unknown type... *)
let f (x : 'a mallocd) = exclave_ (free_stack_ x)
[%%expect{|
Line 1, characters 47-48:
1 | let f (x : 'a mallocd) = exclave_ (free_stack_ x)
                                                   ^
Error: Cannot free values of type "'a"
|}]

let f (x : 'a mallocd) (y : 'a) =
  let (a,b) = y in
  let _ = free_stack_ x in
  a + b
[%%expect {|
val f : (int * int) mallocd @ unique -> int * int -> int = <fun>
|}, Principal{|
Line 3, characters 10-23:
3 |   let _ = free_stack_ x in
              ^^^^^^^^^^^^^
Warning 18 [not-principal]: typing this term eagerly matches on the type 'a * 'b, which is not principal.

val f : (int * int) mallocd @ unique -> int * int -> int = <fun>
|}]

let f (x : 'a mallocd) (y : 'a) =
  ignore (free_stack_ x);
  let (a,b) = y in
  a + b
[%%expect {|
Line 2, characters 22-23:
2 |   ignore (free_stack_ x);
                          ^
Error: Cannot free values of type "'a"
|}]

(*
Successful free-to-stack of tuples
*)
let f (x : (_ * _) mallocd) = exclave_ (free_stack_ x)
[%%expect {|
val f : ('a * 'b) mallocd @ unique -> local_ 'a * 'b = <fun>
|}, Principal{|
Line 1, characters 39-54:
1 | let f (x : (_ * _) mallocd) = exclave_ (free_stack_ x)
                                           ^^^^^^^^^^^^^^^
Warning 18 [not-principal]: typing this term eagerly matches on the type 'a * 'b, which is not principal.

val f : ('a * 'b) mallocd @ unique -> local_ 'a * 'b = <fun>
|}]

let f (x : (_ * _ * _) mallocd) = exclave_ (free_stack_ x)
[%%expect {|
val f : ('a * 'b * 'c) mallocd @ unique -> local_ 'a * 'b * 'c = <fun>
|}, Principal{|
Line 1, characters 43-58:
1 | let f (x : (_ * _ * _) mallocd) = exclave_ (free_stack_ x)
                                               ^^^^^^^^^^^^^^^
Warning 18 [not-principal]: typing this term eagerly matches on the type 'a * 'b * 'c, which is not principal.

val f : ('a * 'b * 'c) mallocd @ unique -> local_ 'a * 'b * 'c = <fun>
|}]

type 'a t = 'a * 'a
let f (x : 'a t mallocd) = exclave_ (free_stack_ x)
[%%expect {|
type 'a t = 'a * 'a
val f : 'a t mallocd @ unique -> local_ 'a t = <fun>
|}, Principal{|
type 'a t = 'a * 'a
Line 2, characters 36-51:
2 | let f (x : 'a t mallocd) = exclave_ (free_stack_ x)
                                        ^^^^^^^^^^^^^^^
Warning 18 [not-principal]: typing this term eagerly matches on the type 'a t, which is not principal.

val f : 'a t mallocd @ unique -> local_ 'a t = <fun>
|}]

type 'a t = 'a * 'a * 'a
let f (x : (_ * _ * _) mallocd) = exclave_ (free_stack_ x)
[%%expect {|
type 'a t = 'a * 'a * 'a
val f : ('a * 'b * 'c) mallocd @ unique -> local_ 'a * 'b * 'c = <fun>
|}, Principal{|
type 'a t = 'a * 'a * 'a
Line 2, characters 43-58:
2 | let f (x : (_ * _ * _) mallocd) = exclave_ (free_stack_ x)
                                               ^^^^^^^^^^^^^^^
Warning 18 [not-principal]: typing this term eagerly matches on the type 'a * 'b * 'c, which is not principal.

val f : ('a * 'b * 'c) mallocd @ unique -> local_ 'a * 'b * 'c = <fun>
|}]

(* Succssful free-to-stack of regular records of various sorts... *)
type t = {x : int; y : string @@ external_}
let f (x : t mallocd) = exclave_ (free_stack_ x)
[%%expect {|
type t = { x : int; y : string @@ external_; }
val f : t mallocd @ unique -> local_ t = <fun>
|}, Principal{|
type t = { x : int; y : string @@ external_; }
Line 2, characters 33-48:
2 | let f (x : t mallocd) = exclave_ (free_stack_ x)
                                     ^^^^^^^^^^^^^^^
Warning 18 [not-principal]: typing this term eagerly matches on the type t, which is not principal.

val f : t mallocd @ unique -> local_ t = <fun>
|}]

type t = {x : int; y : string}
let f (x : t mallocd)  = exclave_ (free_stack_ x)
[%%expect {|
type t = { x : int; y : string; }
val f : t mallocd @ unique -> local_ t = <fun>
|}, Principal{|
type t = { x : int; y : string; }
Line 2, characters 34-49:
2 | let f (x : t mallocd)  = exclave_ (free_stack_ x)
                                      ^^^^^^^^^^^^^^^
Warning 18 [not-principal]: typing this term eagerly matches on the type t, which is not principal.

val f : t mallocd @ unique -> local_ t = <fun>
|}]

type t = {mutable x : int; mutable y : string @@ external_}
let f (x : t mallocd)  = exclave_ (free_stack_ x)
[%%expect {|
type t = { mutable x : int; mutable y : string @@ external_; }
val f : t mallocd @ unique -> local_ t = <fun>
|}, Principal{|
type t = { mutable x : int; mutable y : string @@ external_; }
Line 2, characters 34-49:
2 | let f (x : t mallocd)  = exclave_ (free_stack_ x)
                                      ^^^^^^^^^^^^^^^
Warning 18 [not-principal]: typing this term eagerly matches on the type t, which is not principal.

val f : t mallocd @ unique -> local_ t = <fun>
|}]

type t = {mutable x : int64#; mutable y : string @@ external_}
let f (x : t mallocd)  = exclave_ (free_stack_ x)
[%%expect {|
type t = { mutable x : int64#; mutable y : string @@ external_; }
val f : t mallocd @ unique -> local_ t = <fun>
|}, Principal{|
type t = { mutable x : int64#; mutable y : string @@ external_; }
Line 2, characters 34-49:
2 | let f (x : t mallocd)  = exclave_ (free_stack_ x)
                                      ^^^^^^^^^^^^^^^
Warning 18 [not-principal]: typing this term eagerly matches on the type t, which is not principal.

val f : t mallocd @ unique -> local_ t = <fun>
|}]

type t = {mutable x : int64#; y : float#}
let f (x : t mallocd)  = exclave_ (free_stack_ x)
[%%expect {|
type t = { mutable x : int64#; y : float#; }
val f : t mallocd @ unique -> local_ t = <fun>
|}, Principal{|
type t = { mutable x : int64#; y : float#; }
Line 2, characters 34-49:
2 | let f (x : t mallocd)  = exclave_ (free_stack_ x)
                                      ^^^^^^^^^^^^^^^
Warning 18 [not-principal]: typing this term eagerly matches on the type t, which is not principal.

val f : t mallocd @ unique -> local_ t = <fun>
|}]

(* CR jcutler for ccasinghino: it would be a bit of work to
   allow stack-freeing of predefs that are allocated. Worth it or no?
*)
let f (x : float mallocd) = exclave_ (free_stack_ x)
[%%expect {|
Line 1, characters 50-51:
1 | let f (x : float mallocd) = exclave_ (free_stack_ x)
                                                      ^
Error: Cannot free values of type "float"
|}]

let f (x : int64 mallocd) = exclave_ (free_stack_ x)
[%%expect {|
Line 1, characters 50-51:
1 | let f (x : int64 mallocd) = exclave_ (free_stack_ x)
                                                      ^
Error: Cannot free values of type "int64"
|}]

let f (x : int32 mallocd) = exclave_ (free_stack_ x)
[%%expect {|
Line 1, characters 50-51:
1 | let f (x : int32 mallocd) = exclave_ (free_stack_ x)
                                                      ^
Error: Cannot free values of type "int32"
|}]

type t = float
let f (x : t mallocd) = exclave_ (free_stack_ x)
[%%expect {|
type t = float
Line 2, characters 46-47:
2 | let f (x : t mallocd) = exclave_ (free_stack_ x)
                                                  ^
Error: Cannot free values of type "t"
|}]

type t = int64
let f (x : t mallocd) = exclave_ (free_stack_ x)
[%%expect {|
type t = int64
Line 2, characters 46-47:
2 | let f (x : t mallocd) = exclave_ (free_stack_ x)
                                                  ^
Error: Cannot free values of type "t"
|}]

type t = int32
let f (x : t mallocd) = exclave_ (free_stack_ x)
[%%expect {|
type t = int32
Line 2, characters 46-47:
2 | let f (x : t mallocd) = exclave_ (free_stack_ x)
                                                  ^
Error: Cannot free values of type "t"
|}]

(* Stack-freeing similarly works only when the type is known and in-scope.  *)
module M : sig
  type t = int * int
end = struct
  type t = int * int
end

let f (x : M.t mallocd) = exclave_ (free_stack_ x)
[%%expect{|
module M : sig type t = int * int end
val f : M.t mallocd @ unique -> local_ M.t = <fun>
|}, Principal{|
module M : sig type t = int * int end
Line 7, characters 35-50:
7 | let f (x : M.t mallocd) = exclave_ (free_stack_ x)
                                       ^^^^^^^^^^^^^^^
Warning 18 [not-principal]: typing this term eagerly matches on the type M.t, which is not principal.

val f : M.t mallocd @ unique -> local_ M.t = <fun>
|}]

module M : sig
  type t = {x : int; y: int}
end = struct
  type t = {x : int; y: int}
end

let f (x : M.t mallocd) = exclave_ (free_stack_ x)
[%%expect{|
module M : sig type t = { x : int; y : int; } end
val f : M.t mallocd @ unique -> local_ M.t = <fun>
|}, Principal{|
module M : sig type t = { x : int; y : int; } end
Line 7, characters 35-50:
7 | let f (x : M.t mallocd) = exclave_ (free_stack_ x)
                                       ^^^^^^^^^^^^^^^
Warning 18 [not-principal]: typing this term eagerly matches on the type M.t, which is not principal.

val f : M.t mallocd @ unique -> local_ M.t = <fun>
|}]

module M : sig
  type t
end = struct
  type t = int * int
end

let f (x : M.t mallocd) = exclave_ (free_stack_ x)
[%%expect{|
module M : sig type t end
Line 7, characters 48-49:
7 | let f (x : M.t mallocd) = exclave_ (free_stack_ x)
                                                    ^
Error: Cannot free values of type "M.t"
|}]

module M : sig
  type t
end = struct
  type t = {x : int; y: int}
end

let f (x : M.t mallocd) = exclave_ (free_stack_ x)
[%%expect{|
module M : sig type t end
Line 7, characters 48-49:
7 | let f (x : M.t mallocd) = exclave_ (free_stack_ x)
                                                    ^
Error: Cannot free values of type "M.t"
|}]


(* Variants can be stack-freed *)
type t = Foo of int
let f (x : t mallocd) = exclave_ (free_stack_ x)
[%%expect {|
type t = Foo of int
val f : t mallocd @ unique -> local_ t = <fun>
|}, Principal{|
type t = Foo of int
Line 2, characters 33-48:
2 | let f (x : t mallocd) = exclave_ (free_stack_ x)
                                     ^^^^^^^^^^^^^^^
Warning 18 [not-principal]: typing this term eagerly matches on the type t, which is not principal.

val f : t mallocd @ unique -> local_ t = <fun>
|}]

type t = Foo of int | Bar of char
let f (x : t mallocd) = exclave_ (free_stack_ x)
[%%expect {|
type t = Foo of int | Bar of char
val f : t mallocd @ unique -> local_ t = <fun>
|}, Principal{|
type t = Foo of int | Bar of char
Line 2, characters 33-48:
2 | let f (x : t mallocd) = exclave_ (free_stack_ x)
                                     ^^^^^^^^^^^^^^^
Warning 18 [not-principal]: typing this term eagerly matches on the type t, which is not principal.

val f : t mallocd @ unique -> local_ t = <fun>
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

(* Can free anonymous variants *)
let f (x : [`Foo of int] mallocd) = exclave_ (free_stack_ x)
[%%expect{|
val f : [ `Foo of int ] mallocd @ unique -> local_ [ `Foo of int ] = <fun>
|}, Principal{|
Line 1, characters 45-60:
1 | let f (x : [`Foo of int] mallocd) = exclave_ (free_stack_ x)
                                                 ^^^^^^^^^^^^^^^
Warning 18 [not-principal]: typing this term eagerly matches on the type [ `Foo of int ], which is not principal.

val f : [ `Foo of int ] mallocd @ unique -> local_ [ `Foo of int ] = <fun>
|}]

let f (x : [`Foo of int | `Bar of int * int] mallocd) = exclave_ (free_stack_ x)
[%%expect{|
val f :
  [ `Bar of int * int | `Foo of int ] mallocd @ unique -> local_
  [ `Bar of int * int | `Foo of int ] = <fun>
|}, Principal{|
Line 1, characters 65-80:
1 | let f (x : [`Foo of int | `Bar of int * int] mallocd) = exclave_ (free_stack_ x)
                                                                     ^^^^^^^^^^^^^^^
Warning 18 [not-principal]: typing this term eagerly matches on the type [ `Bar of int * int
                                             | `Foo of int ], which is not principal.

val f :
  [ `Bar of int * int | `Foo of int ] mallocd @ unique -> local_
  [ `Bar of int * int | `Foo of int ] = <fun>
|}]

type t = [`Foo of int]
let f (x : t mallocd) = exclave_ (free_stack_ x)
[%%expect{|
type t = [ `Foo of int ]
val f : t mallocd @ unique -> local_ t = <fun>
|}, Principal{|
type t = [ `Foo of int ]
Line 2, characters 33-48:
2 | let f (x : t mallocd) = exclave_ (free_stack_ x)
                                     ^^^^^^^^^^^^^^^
Warning 18 [not-principal]: typing this term eagerly matches on the type t, which is not principal.

val f : t mallocd @ unique -> local_ t = <fun>
|}]

type t = [`Foo of int | `Bar of int * int]
let f (x : t mallocd) = exclave_ (free_stack_ x)
[%%expect{|
type t = [ `Bar of int * int | `Foo of int ]
val f : t mallocd @ unique -> local_ t = <fun>
|}, Principal{|
type t = [ `Bar of int * int | `Foo of int ]
Line 2, characters 33-48:
2 | let f (x : t mallocd) = exclave_ (free_stack_ x)
                                     ^^^^^^^^^^^^^^^
Warning 18 [not-principal]: typing this term eagerly matches on the type t, which is not principal.

val f : t mallocd @ unique -> local_ t = <fun>
|}]

(* Types that cannot be freed-to-stack *)
let f (x : (int -> int) mallocd) = exclave_ (free_stack_ x)
[%%expect{|
Line 1, characters 57-58:
1 | let f (x : (int -> int) mallocd) = exclave_ (free_stack_ x)
                                                             ^
Error: Cannot free values of type "int -> int"
|}]

let f (x : <get : int; set : int> mallocd) = exclave_ (free_stack_ x)
[%%expect{|
Line 1, characters 67-68:
1 | let f (x : <get : int; set : int> mallocd) = exclave_ (free_stack_ x)
                                                                       ^
Error: Cannot free values of type "< get : int; set : int >"
|}]

type t
let f (x : t mallocd) = exclave_ (free_stack_ x)
[%%expect{|
type t
Line 2, characters 46-47:
2 | let f (x : t mallocd) = exclave_ (free_stack_ x)
                                                  ^
Error: Cannot free values of type "t"
|}]

let f (x : int mallocd) =
  exclave_ (free_stack_ x)
[%%expect {|
Line 2, characters 24-25:
2 |   exclave_ (free_stack_ x)
                            ^
Error: Cannot free values of type "int"
|}]

let f (t : (int -> int) mallocd) = exclave_ (free_stack_ t)
[%%expect{|
Line 1, characters 57-58:
1 | let f (t : (int -> int) mallocd) = exclave_ (free_stack_ t)
                                                             ^
Error: Cannot free values of type "int -> int"
|}]

let f (t : < get : int > mallocd) = exclave_ (free_stack_ t)
[%%expect{|
Line 1, characters 58-59:
1 | let f (t : < get : int > mallocd) = exclave_ (free_stack_ t)
                                                              ^
Error: Cannot free values of type "< get : int >"
|}]

let f (t : [`Foo of int | `Bar of char] mallocd) = exclave_ (free_stack_ t)
[%%expect{|
val f :
  [ `Bar of char | `Foo of int ] mallocd @ unique -> local_
  [ `Bar of char | `Foo of int ] = <fun>
|}, Principal{|
Line 1, characters 60-75:
1 | let f (t : [`Foo of int | `Bar of char] mallocd) = exclave_ (free_stack_ t)
                                                                ^^^^^^^^^^^^^^^
Warning 18 [not-principal]: typing this term eagerly matches on the type [ `Bar of char | `Foo of int ], which is not principal.

val f :
  [ `Bar of char | `Foo of int ] mallocd @ unique -> local_
  [ `Bar of char | `Foo of int ] = <fun>
|}]
