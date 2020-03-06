(* Being Lazy *)

type 'a lazylist = Cons of 'a * (unit -> 'a lazylist)

let lhd (Cons (n, _)) = n
let ltl (Cons (_, tf)) = tf ()

let rec ltake (Cons (h, tf)) n =
        match n with
        0 -> []
        | _ -> h :: ltake (tf ()) (n - 1)

let rec ldrop (Cons (h, tf) as ll) n =
        match n with
        0 -> ll
        | _ -> ldrop (tf ()) (n - 1)

(* Question 1 *)

let rec _lpow2 n = Cons (n, fun () -> _lpow2 (n * 2)) 

let l_pow2 = _lpow2 1

