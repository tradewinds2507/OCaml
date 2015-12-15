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

let factors n =
  let rec helper n k acc = if (n > 1) 
      then ( if (n mod k = 0) && (isPrime k)
        then helper (n/k) k (acc@[k])
        else helper n (k+1) acc )
      else acc
in
  helper n 2 []




(* Question 34 *)

let factors2 n =
  let rec helper1 n k acc = ( if (n mod k = 0) then helper1 (n/k) k (acc+1) else acc )
  in
    let rec helper2 n k acc = ( if (n > 1)
        then ( if (n mod k = 0) && (isPrime k)
          then ( let m = helper1 n k 0
           in
             let rec p x y acc = if y > 0 then p x (y-1) (acc*x) else acc
             in
               helper2 (n/p k m 1) (k+1) (acc@[(k, m)]) )
          else helper2 n (k+1) acc )
        else acc )
in
  helper2 n 2 []




(* Question 35 *)

let phi_improved n =
  let rec helper l acc = ( match l with
    | [] -> acc
    | (x, y)::ls -> 
      let rec p x y acc = if y > 0 then p x (y-1) (acc*x) else acc
      in
        helper ls (acc*(x-1)*(p x (y-1) 1)) )
in
  helper (factors2 n) 1




(* Question 36 *)

let timeit f x : float =
  let it = Sys.time ()
  in
    ignore(f x); let ft = Sys.time ()
in
  (ft-.it)




(* Question 37 *)

let all_primes lower upper =
  let rec helper l u acc = if (l < u)
      then ( if isPrime l then helper (l+1) u (acc@[l]) else helper (l+1) u acc )
      else acc
in
  helper lower upper []




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




(* Binary Trees *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

(* Quesion 44 *)




(* Question 46 *)

let rec construct l =
  let rec helper1 t x = ( match t with
    | Empty -> Node (x, Empty, Empty)
    | Node (y, l, r) -> ( if x = y then Node (y, l, r) else if x < y
        then Node (y, helper1 l x, r)
        else Node (y, l, helper1 r x) ) )
  in
    let rec helper2 l acc = ( match l with
      | [] -> acc
      | x::ls -> helper2 ls (helper1 acc x) )
in
  helper2 l Empty

let isSymmetric t = 
  let rec helper (l, r) = ( match l, r with
    | Empty, Empty -> true
    | Node (y, l1, r1), Node (z, l2, r2) ->
      (helper (l1, l2)) && (helper (r1, r2))
    | _, _ -> false )
in
  ( match t with
    | Empty -> true
    | Node (_, l, r) -> helper (l, r) )




(* Question 47 *)




(* Question 50 *)

let countLeaves t =
  let c = ref 0 in
    let rec helper t = ( match t with
      | Empty -> ()
      | Node (_, Empty, Empty) -> c := (!c+1)
      | Node (_, l, r) -> helper l; helper r )
in
  helper t; !c




(* Question 52 *)

let internals t =
  let rec helper t acc = ( match t with
    | Empty -> acc
    | Node (_, Empty, Empty) -> acc
    | Node (x, l, r) -> helper r (helper l acc@[x]) )
in
  helper t []




(* Question 53 *)

let atLevel t l =
  let rec helper t l acc = ( match t, l with
    | Empty, _ -> acc
    | Node (x, l, r), 1 -> acc@[x]
    | Node (_, l, r), l' -> if (l' > 1)
        then helper r (l'-1) (helper l (l'-1) acc)
        else acc )
in
  helper t l []




(* Question 58 *)

let rec string_of_tree (t: 'a binary_tree) (f: 'a -> string) = match t with
  | Empty -> ""
  | Node (x, Empty, Empty) -> f x
  | Node (x, l, r) ->
    let (p, q) = (string_of_tree l f, string_of_tree r f)
    in
      (f x) ^ "(" ^ p ^ "," ^ q ^ ")"




(* Question 59 *)

let rec preorder (t: 'a binary_tree) (f: 'a -> string) = match t with
  | Empty -> ""
  | Node (x, l, r) -> (f x) ^ (preorder l f) ^ (preorder r f)

let rec inorder (t: 'a binary_tree) (f: 'a -> string) = match t with
   | Empty -> ""
   | Node (x, l, r) -> (inorder l f) ^ (f x) ^ (inorder r f)

let rec postorder (t: 'a binary_tree) (f: 'a -> string) = match t with
    | Empty -> ""
    | Node (x, l, r) -> (postorder l f) ^ (postorder r f) ^ (f x)




(* Multiway Trees *)

type 'a mult_tree = T of 'a * 'a mult_tree list

(* Look at dict.ml in tradewinds2507/COMP302hw3 *)




(* Graphs *)

type 'a graph = { vertices : 'a list; edges : ('a * 'a) list }

(* Question 67 *)
(* Path from one node to another *)




type ('a, 'b) labeled_graph = { vertices : 'a list; edges : ('a * 'a * 'b) list }

(* Question 75 *)

let isBipartite (g: 'a graph) =
  let rec helper1 v acc = ( match v with
    | [] -> acc
    | x::xs -> helper1 xs ((x, 0)::acc) )
  in
    let rec helper2 e f acc = ( match e with
      | [] -> acc
      | (x, y)::xs -> ( match List.assoc x acc, List.assoc y acc with
        | 0, 0 -> helper2 xs f ((x, 1)::(y, 2)::(List.remove_assoc x (List.remove_assoc y acc)))
        | 0, c -> helper2 ((x, List.assoc x xs)::(List.remove_assoc x xs)) f ((x, f c)::(List.remove_assoc x acc))
        | c, 0 -> helper2 ((y, List.assoc y xs)::(List.remove_assoc y xs)) f ((y, f c)::(List.remove_assoc y acc))
        | _, _ -> helper2 xs f acc ) )
    in
      let rec helper3 e l acc = ( match e with
        | [] -> acc
        | (x, y)::xs -> helper3 xs l ((not (List.assoc x l = List.assoc y l)) && acc) )
      in
        let (g', f) = ((helper1 g.vertices []), (fun c -> if c = 1 then 2 else 1))
in
  (helper3 g.edges (helper2 g.edges f g') true)




(* Miscellaneous Problems *)

(* Question 77 *)

let queens'Positions n =
  (* If the Queen at (x1, y1) conflicts with another Queen at (x2, y2). *)
  let helper1 (x1, y1) (x2, y2) = ( (not (x1 = x2)) && (not (y1 = y2))
    && (not (x2-x1 = y2-y1)) && (not (x2-x1 = y1-y2)) )
  in
    (* If the list of Queens at positions in l are valid. *)
    let rec helper2 l = ( match l with
      | [] -> true
      | (x, y)::xs -> let z = (List.map (fun z -> helper1 (x, y) z) xs)
        in
          (List.fold_left (&&) true z) && (helper2 xs) )
    in
      (* Computes the positions of the Queens using Continuations. *)
      let rec helper3 (k, l) acc (* (k, l) is the position of last kept Queen *)
        (scCont: (int * int) list -> (int * int) list list)
        (flCont: unit -> (int * int) list list) = if (k < n)
          then ( if (l < n)
            then (                helper3 (k+1, l+1) ((k+1, l+1)::acc)
              (fun acc' -> acc'::(helper3 (k+1, l+1) ((k+1, l+1)::acc) scCont flCont))
              (fun () ->          helper3 (k+1, l+1) ((k+1, l+1)::acc) scCont flCont) )
            else (                helper3 (k+1, 0) ((k+1, 0)::acc) 
              (fun acc' -> acc'::(helper3 (k+1, 0) ((k+1, 0)::acc) scCont flCont))
              (fun () ->          helper3 (k+1, 0) ((k+1, 0)::acc) scCont flCont) ) )
          else ( if (helper2 acc) then (scCont acc) else (flCont ()) )
in
  helper3 (1, 1) [] (fun x -> x::[]) (fun () -> [])









