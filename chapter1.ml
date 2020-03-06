(* Question 1 *)

let update_budget budget expenses = List.fold_left ( - ) budget expenses

let rec _update_budget budget expenses =
        match expenses with
        [] -> budget
        | head::tail -> _update_budget (budget - head) tail

(* Question 2 *)
let length l = List.fold_left (fun acc _el -> acc + 1) 0 l

(* Question 3 *)
let last l = List.fold_left (fun _acc el -> Some(el)) None l

(* Question 4 *)
let reverse l = List.fold_left (fun acc el -> el::acc) [] l

(* Question 5 *)
let mem base l = List.fold_left (fun acc el -> acc || el == base) false l

(* Question 6 *)
let join l = List.fold_left (fun acc el -> if acc = "" then el else acc ^ " " ^ el) "" l

(* Question 7 *)
type 'a tree =
        Lf
        | Br of 'a  * 'a tree * 'a tree

let rec fold_tree f e t =
        match t with
        Lf ->  e
        | Br (x, l, r) -> f x (fold_tree f e l) (fold_tree f e r)

let example_tree = Br (1, Br (0, Lf, Lf), Br (6, Br (4, Lf, Lf), Lf))

let max a b = if a > b then a else b

let depth tree = fold_tree (fun _el left right -> 1 + max left right) 0 tree
