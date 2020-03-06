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

(* Question 2 *)
let nth l n = lhd (ldrop l n)

(* Question 3 *)
exception Fuck

let rec repeat l =
        match l with
        [] -> raise Fuck
        | head :: tail -> Cons (head, fun () -> repeat (tail @ [head]))
(* !! this is totally ineficient !! *)
(* I should use an accumulator instead of doing all these concatenation *)
        
(* Question 4 *)
let rec _fibo a b = Cons (a, fun () -> _fibo b (a + b)) 

let fibo = _fibo 0 1

(* Question 5 *)
let rec one_on_two (Cons (head, tail)) =
        let Cons (_, tail) = tail () in
        Cons (head, fun () -> one_on_two (tail ()))

let unleave l = (one_on_two l, one_on_two (ldrop l 1))
