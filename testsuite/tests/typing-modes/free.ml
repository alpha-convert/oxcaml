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

let f (x : ('a * 'a) mallocd) (y : 'a) =
  let #(_,_) = free_ x in
  y + 1
[%%expect {|
val f : (int * int) mallocd @ unique -> int -> int = <fun>
|}]



(* Only free malloc'd
   Only free unique mallocd'
*)

let f (x : int) =
  free_ x
[%%expect {|
Line 2, characters 8-9:
2 |   free_ x
            ^
Error: This expression has type "int" but an expression was expected of type
         "'a mallocd"
|}]

let f (x : (int * int) mallocd @ aliased) =
  free_ x
[%%expect {|
Line 2, characters 8-9:
2 |   free_ x
            ^
Error: This value is "aliased" but expected to be "unique".
|}]

type t = {x : int; y : int}
let f (t : t mallocd) = free_ t
[%%expect {|
type t = { x : int; y : int; }
val f : t mallocd @ unique -> #(x:int * y:int) = <fun>
|}]

type t = {x : int; y : int}
let f (t : t mallocd) = free_ t
[%%expect {|
type t = { x : int; y : int; }
val f : t mallocd @ unique -> #(x:int * y:int) = <fun>
|}]

type t = {mutable x : string; y : int}
let f (t : t mallocd) = free_ t
[%%expect {|
|}]

type t = Foo of {z : char ; x : string; y : int}
let f (t : t mallocd) = free_ t
[%%expect {||}]


type t = Foo of {mutable x : string; y : int}
let f (t : t mallocd) = free_ t
[%%expect {||}]

(*CR jcutler: ensure this works with module business.
module M : sig type t = ... free M.t

*)
