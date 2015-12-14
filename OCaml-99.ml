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
  List.fold_left (fun y -> (fun x -> y && not ((n mod x) = 0)) ) true (helper (n-1) [])




(* Question 30 *)








