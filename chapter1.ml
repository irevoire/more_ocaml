(* Question 1 *)

let update_budget budget expenses = List.fold_left (-) budget expenses

let rec _update_budget budget expenses =
        match expenses with
        [] -> budget
        | head::tail -> _update_budget (budget - head) tail
;;
