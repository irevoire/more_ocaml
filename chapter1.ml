(* Question 1 *)

let update_budget budget expenses = List.fold_left ( - ) budget expenses

let rec _update_budget budget expenses =
        match expenses with
        [] -> budget
        | head::tail -> _update_budget (budget - head) tail

(* Question 2 *)
let length l = List.fold_left (fun acc _el -> acc + 1) 0 l

(* Question 3 *)
let last l = List.fold_left (fun _acc el -> Some(el)) None l;;
