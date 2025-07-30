(* TEST
  expect;
*)

(* Use can be given the most precise type *)
external use : 'a mallocd @ local unique -> ('a @ local external_ -> 'b @ unique) @ local once -> #('b * 'a mallocd) @ unique = "%use_mallocd"
[%%expect{|
external use :
  'a mallocd @ local unique ->
  ('a @ local external_ -> 'b @ unique) @ local once ->
  #('b * 'a mallocd) @ unique = "%use_mallocd"
|}]

(* First argument must be of layout word *)
external use : int64# -> int -> #('b * 'a mallocd) @ unique = "%use_mallocd"
[%%expect{|
Line 1, characters 15-59:
1 | external use : int64# -> int -> #('b * 'a mallocd) @ unique = "%use_mallocd"
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%use_mallocd] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

(* Second must be of layout value *)
external use : int -> int64# -> #('b * 'a mallocd) @ unique = "%use_mallocd"
[%%expect{|
Line 1, characters 15-59:
1 | external use : int -> int64# -> #('b * 'a mallocd) @ unique = "%use_mallocd"
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
