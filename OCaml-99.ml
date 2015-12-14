(* 99-Problems, implemented in OCaml *)
(* Look at https://ocaml.org/learn/tutorials/99problems.html *)



(* Working with Lists *)

(* Question 1 *)

let rec last l = match l with
  | [] -> None
  | [x] -> Some x
  | _::ls -> last ls




(* Question 2 *)

let rec lastButOne l = match l with
  | [] -> None
  | [_] -> None
  | [x;_] -> Some x
  | _::ls -> lastButOne ls




(* Question 3 *)

let rec kthElement l k = match l, k with
  | [], _ -> None
  | x::_, 1 -> Some x
  | _::ls, k -> if (k > 1)
      then kthElement ls (k-1)
      else None 




(* Question 4 *)

let lengthOfList l =
  let rec helper l acc = ( match l with
    | [] -> acc
    | _::ls -> helper ls (acc + 1) )
in helper l 0




(* Question 5 *)

let reverseList l =
  let rec helper l acc = ( match l with
    | [] -> acc
    | x::ls -> helper ls (x::acc) )
in helper l []




(* Question 6 *)

let isPallindrome l =
  let r = reverseList l
in (l = r)




(* Question 7 *)

type 'a node =
  | One of 'a
  | Many of 'a  node list

let rec flatten (l: 'a node list) : 'a list = 
  let rec helper l acc = ( match l with
    | [] -> acc
    | (One x)::ls -> helper ls (acc@[x])
    | (Many z)::ls -> helper ls (acc@(flatten z)) )
in helper l []




(* Question 8 *)

let compress l =
  let rec helper l acc = ( match l with 
    | [] -> acc
    | x::y::ls -> if (x = y)
        then helper (y::ls) acc
        else helper (y::ls) (acc@[x])
    | [x] -> (acc@[x]) )
in helper l []




(* Question 9 *)
(*
let pack l =
  let rec helper l acc = ( match l with
    | [] -> acc
    | x::y::ls -> if (x = y)
        then helper (y::ls)  )
*)




(* Question 10 *)




(* Arithmetic *)

(* Question 29 *)

let isPrime n =
  let rec helper n acc = ( if (n < 2)
      then acc
      else helper (n-1) (n::acc) )
in
  if (n < 2)
    then false
    else List.fold_left (fun y -> (fun x -> y && not ((n mod x) = 0)) ) true (helper (n-1) [])




(* Question 30 *)

let rec gcd x y = match x, y with
  | 0, _ -> y
  | _, 0 -> x
  | _, _ -> gcd y (x mod y)




(* Question 31 *)

let coprime x y = if (gcd x y = 1)
    then true
    else false




(* Question 32 *)

let phi m =
  let rec helper n acc = ( if (n < 1)
      then acc
      else helper (n-1) (n::acc) )
in
  lengthOfList (List.filter (fun x -> coprime x m) (helper (m-1) []))




(* Question 33 *)




(* Question 38 *)

let goldbach n = if (n mod 2 = 0)
    then let rec helper n acc =
        if (isPrime acc) && (isPrime (n-acc))
          then (acc, n-acc)
          else helper n (acc+1)
      in
        helper n 1
    else (0, n)




(* Question 39 *)

let goldbach_list x y = 
  let rec helper x y acc = if (x < y)
      then ( match (goldbach x) with
          | (0, _) -> helper (x+1) y acc
          | z -> helper (x+1) y (acc@[(x, z)]) )
      else acc
in
  helper x y []

let goldbach_limit x y z =
  let l = goldbach_list x y
  in
    let rec helper l' acc = ( match l' with
      | [] -> acc
      | (p, (q, r))::ls -> if (q > z) && (r > z)
          then helper ls acc@[(p, (q, r))]
          else helper ls acc )
in
  helper l []




(* Logic and Codes *)

type bool_exp =
  | Var of string
  | Not of bool_exp
  | And of bool_exp * bool_exp
  | Or of bool_exp * bool_exp

(* Question 40 *)

let table2 x y e =
  let rec eval (x, y) (p, q) e = ( match e with
    | Var z -> if (z = x) then p else q
    | Not e' -> not (eval (x, y) (p, q) e')
    | And (e1, e2) -> (eval (x, y) (p, q) e1) && (eval (x, y) (p, q) e2)
    | Or (e1, e2) -> (eval (x, y) (p, q) e1) || (eval (x, y) (p, q) e2)  )
in
  [ (true, true, (eval (x, y) (true, true) e));
    (true, false, (eval (x, y) (true, false) e));
    (false, true, (eval (x, y) (false, true) e));
    (false, false, (eval (x, y) (false, false) e)) ]




(* Question 41 *)




(* Question 42 *)

let gray n =
  let rec helper1 k acc = ( match k with
    | 0 -> acc
    | _ -> helper1 (k/2) ((string_of_int (k mod 2))^acc) )
  in
    let rec helper2 n acc = ( if (n < 0)
        then acc
        else helper2 (n-1) ((helper1 n "")::acc) )
in
  helper2 (Pervasives.truncate (2.0 ** (Pervasives.float n) -. 1.0 )) []




(* Question 43 *)
(*
let huffman fs = 
*)





