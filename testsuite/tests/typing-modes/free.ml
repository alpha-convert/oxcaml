(* TEST
 expect;
*)

let f (x @ external_) (y @ external_) =
  free_ (malloc_ (x,y))
[%%expect{|
val f : 'a @ unique -> 'b @ unique -> #('a * 'b) = <fun>
|}]

let f (x : (int * int) mallocd) =
  free_ x
[%%expect {|
val f : (int * int) mallocd @ unique -> #(int * int) = <fun>
|}]

let f (x : (int * int) mallocd @ aliased) =
  free_ x
[%%expect {|
Line 2, characters 8-9:
2 |   free_ x
            ^
Error: This value is "aliased" but expected to be "unique".
|}]


let f (x : ('a * 'a) mallocd) (y : 'a) =
  let #(_,_) = free_ x in
  y + 1
[%%expect {|
val f : (int * int) mallocd @ unique -> int -> int = <fun>
|}]

let [@warning "-10"] f (x : 'a mallocd) (y : 'a) =
  let (a,b) = y in
  free_ x;
  a + b
[%%expect {|
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

let f (x : int) =
  free_ x
[%%expect {|
Line 2, characters 8-9:
2 |   free_ x
            ^
Error: This expression has type "int" but an expression was expected of type
         "'a mallocd"
|}]

(* CR jcutler: this is a terrible error message *)
let f (x : int mallocd) =
  free_ x
[%%expect {|
Line 2, characters 8-9:
2 |   free_ x
            ^
Error: Type "int" does not have an unboxed version, try free_stack_
|}]

(* CR jcutler for ccasinghino: we can't allocate these atm, but should be find to
   allow people to free them? *)
let f (x : int64 mallocd) =
  free_ x
[%%expect {|
val f : int64 mallocd @ unique -> int64# = <fun>
|}]

let f (x : ('a * 'a) mallocd) =
  free_ x
[%%expect{|
val f : ('a * 'a) mallocd @ unique -> #('a * 'a) = <fun>
|}]

type t = {x : int; y : int}
let f (t : t mallocd) = free_ t
[%%expect {|
type t = { x : int; y : int; }
val f : t mallocd @ unique -> t# = <fun>
|}]

type 'a t = {x : 'a; y : 'a}
let f (t : 'a t mallocd) = free_ t
[%%expect {|
type 'a t = { x : 'a; y : 'a; }
val f : 'a t mallocd @ unique -> 'a t# = <fun>
|}]

type t = {mutable x : string; y : int}
let f (t : t mallocd) = free_ t
[%%expect {|
type t = { mutable x : string; y : int; }
val f : t mallocd @ unique -> t# = <fun>
|}]

type t = Foo of {z : char ; x : string; y : int}
let f (t : t mallocd) = free_ t
[%%expect {|
type t = Foo of { z : char; x : string; y : int; }
Line 2, characters 30-31:
2 | let f (t : t mallocd) = free_ t
                                  ^
Error: Type "t" does not have an unboxed version, try free_stack_
|}]


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

type t = Pack : 'a. 'a -> t
let f (t : t mallocd) = free_ t
[%%expect{|
type t = Pack : 'a -> t
Line 2, characters 30-31:
2 | let f (t : t mallocd) = free_ t
                                  ^
Error: Type "t" does not have an unboxed version, try free_stack_
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



(* STACK FREE*)

let generic_free_stack (x : 'a mallocd) = exclave_ (free_stack_ x)
[%%expect {|
Line 1, characters 64-65:
1 | let generic_free_stack (x : 'a mallocd) = exclave_ (free_stack_ x)
                                                                    ^
Error: Cannot free values of type "'a"
|}]

let f x = exclave_ (free_stack_ x)
[%%expect {|
Line 1, characters 32-33:
1 | let f x = exclave_ (free_stack_ x)
                                    ^
Error: Cannot free values of type "'a"
|}]

let f x @ global = free_stack_ x
[%%expect {|
Line 1, characters 31-32:
1 | let f x @ global = free_stack_ x
                                   ^
Error: Cannot free values of type "'a"
|}]

let f (x @ external_) (y @ external_) =
  exclave_ (free_stack_ (malloc_ (x,y)))
[%%expect{|
val f : 'a @ unique -> 'b @ unique -> local_ 'a * 'b = <fun>
|}]

let f (x : (int * int) mallocd) =
  exclave_ (free_stack_ x)
[%%expect {|
val f : (int * int) mallocd @ unique -> local_ int * int = <fun>
|}]

let f (x : (int * int) mallocd @ aliased) =
  free_stack_ x
[%%expect {|
Line 2, characters 14-15:
2 |   free_stack_ x
                  ^
Error: This value is "aliased" but expected to be "unique".
|}]


let f (x : ('a * 'a) mallocd) (y : 'a) =
  let (_,_) = free_stack_ x in
  y + 1
[%%expect {|
val f : (int * int) mallocd @ unique -> int -> int = <fun>
|}]

let [@warning "-10"] f (x : 'a mallocd) (y : 'a) =
  let (a,b) = y in
  free_stack_ x;
  a + b
[%%expect {|
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

let f (x : int) =
  free_stack_ x
[%%expect {|
Line 2, characters 14-15:
2 |   free_stack_ x
                  ^
Error: This expression has type "int" but an expression was expected of type
         "'a mallocd"
|}]

(* CR jcutler: this is a terrible error message *)
let f (x : int mallocd) =
  free_stack_ x
[%%expect {|
Line 2, characters 14-15:
2 |   free_stack_ x
                  ^
Error: Cannot free values of type "int"
|}]

(* CR jcutler: hmm... but we could free_ them... *)
let f (x : int64 mallocd) =
  free_stack_ x
[%%expect {|
Line 2, characters 14-15:
2 |   free_stack_ x
                  ^
Error: Cannot free values of type "int64"
|}]

let f (x : ('a * 'a) mallocd) =
  exclave_ (free_stack_ x)
[%%expect{|
val f : ('a * 'a) mallocd @ unique -> local_ 'a * 'a = <fun>
|}]

type t = {x : int; y : int}
let f (t : t mallocd) = exclave_ (free_stack_ t)
[%%expect {|
type t = { x : int; y : int; }
val f : t mallocd @ unique -> local_ t = <fun>
|}]

type 'a t = {x : 'a; y : 'a}
let f (t : 'a t mallocd) = exclave_ (free_stack_ t)
[%%expect {|
type 'a t = { x : 'a; y : 'a; }
val f : 'a t mallocd @ unique -> local_ 'a t = <fun>
|}]

type t = {mutable x : string; y : int}
let f (t : t mallocd) = exclave_ (free_stack_ t)
[%%expect {|
type t = { mutable x : string; y : int; }
val f : t mallocd @ unique -> local_ t = <fun>
|}]

type t = Foo of {z : char ; x : string; y : int}
let f (t : t mallocd) = exclave_ (free_stack_ t)
[%%expect {|
type t = Foo of { z : char; x : string; y : int; }
val f : t mallocd @ unique -> local_ t = <fun>
|}]

type t = Foo of {z : char ; x : string; y : int} | Bar of char * string
let f (t : t mallocd) = exclave_ (free_stack_ t)
[%%expect {|
type t = Foo of { z : char; x : string; y : int; } | Bar of char * string
val f : t mallocd @ unique -> local_ t = <fun>
|}]


module M : sig
  type t = int * int
end = struct
  type t = int * int
end

let f (x : M.t mallocd) = exclave_ (free_stack_ x)
[%%expect{|
module M : sig type t = int * int end
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
|}]

module M : sig
  type t
end = struct
  type t = int * int
end

let f (x : M.t mallocd) = free_stack_ x
[%%expect{|
module M : sig type t end
Line 7, characters 38-39:
7 | let f (x : M.t mallocd) = free_stack_ x
                                          ^
Error: Cannot free values of type "M.t"
|}]

module M : sig
  type t
end = struct
  type t = {x : int; y: int}
end

let f (x : M.t mallocd) = free_stack_ x
[%%expect{|
module M : sig type t end
Line 7, characters 38-39:
7 | let f (x : M.t mallocd) = free_stack_ x
                                          ^
Error: Cannot free values of type "M.t"
|}]

type t = Pack : 'a. 'a -> t
let f (t : t mallocd) = exclave_ (free_stack_ t)
[%%expect{|
type t = Pack : 'a -> t
val f : t mallocd @ unique -> local_ t = <fun>
|}]

let f (t : (int -> int) mallocd) = free_stack_ t
[%%expect{|
Line 1, characters 47-48:
1 | let f (t : (int -> int) mallocd) = free_stack_ t
                                                   ^
Error: Cannot free values of type "int -> int"
|}]

let f (t : < get : int > mallocd) = free_stack_ t
[%%expect{|
Line 1, characters 48-49:
1 | let f (t : < get : int > mallocd) = free_stack_ t
                                                    ^
Error: Cannot free values of type "< get : int >"
|}]

let f (t : [`Foo of int | `Bar of char] mallocd) = exclave_ (free_stack_ t)
[%%expect{|
val f :
  [ `Bar of char | `Foo of int ] mallocd @ unique -> local_
  [ `Bar of char | `Foo of int ] = <fun>
|}]
