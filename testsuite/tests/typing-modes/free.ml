(* TEST
 expect;
*)

let f (x @ external_) (y @ external_) =
  free_ (malloc_ (x,y))
[%%expect{|
val f : 'a @ unique -> 'b @ unique -> #('a * 'b) = <fun>
|}]

let f (x @ external_ aliased) =
  free_ (malloc_ (x,x))
[%%expect{||}]



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
