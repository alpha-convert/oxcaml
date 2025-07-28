(* TEST
  expect;
*)

module Aliased = struct
  type 'a t = {aliased : 'a @@ aliased} [@@unboxed]
end
(* Use can be given the most precise type *)
external use : 'a mallocd @ local unique -> ('a @ local external_ -> 'b) -> #('b Aliased.t * 'a mallocd) @ unique = "%use_mallocd"
[%%expect{|
module Aliased : sig type 'a t = { aliased : 'a @@ aliased; } [@@unboxed] end
external use :
  'a mallocd @ local unique ->
  ('a @ local external_ -> 'b) -> #('b Aliased.t * 'a mallocd) @ unique
  = "%use_mallocd"
|}]

(* First argument must be of layout word *)
external use : int64# -> int -> #('b Aliased.t * 'a mallocd) @ unique = "%use_mallocd"
[%%expect{|
Line 1, characters 15-69:
1 | external use : int64# -> int -> #('b Aliased.t * 'a mallocd) @ unique = "%use_mallocd"
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%use_mallocd] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

(* Second must be of layout value *)
external use : int -> int64# -> #('b Aliased.t * 'a mallocd) @ unique = "%use_mallocd"
[%%expect{|
Line 1, characters 15-69:
1 | external use : int -> int64# -> #('b Aliased.t * 'a mallocd) @ unique = "%use_mallocd"
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%use_mallocd] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

(* return must be of layout value & word *)
external use : 'a mallocd @ local unique -> int -> int @ unique = "%use_mallocd"
[%%expect{|
Line 1, characters 15-63:
1 | external use : 'a mallocd @ local unique -> int -> int @ unique = "%use_mallocd"
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%use_mallocd] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

external use : 'a mallocd @ local unique -> int -> #(int * int) @ unique = "%use_mallocd"
[%%expect{|
Line 1, characters 15-72:
1 | external use : 'a mallocd @ local unique -> int -> #(int * int) @ unique = "%use_mallocd"
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%use_mallocd] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

external use : 'a mallocd @ local unique -> int -> #(int * int64#) @ unique = "%use_mallocd"
[%%expect{|
Line 1, characters 15-75:
1 | external use : 'a mallocd @ local unique -> int -> #(int * int64#) @ unique = "%use_mallocd"
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%use_mallocd] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]
