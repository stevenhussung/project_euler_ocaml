(*
The prime 41, can be written as the sum of six consecutive primes:
        41 = 2 + 3 + 5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?
*)


(* Generate primes up to 10_000 (Copying from old code) 
   Generating up to one million takes 10 minutes. Too long.
   But, since the longest has more than 21 terms, we can actually stop at 1,000,000 / 21 ~= 47,700
*)
let bound = 1_000_000 / 21 + 1;;

let int_sqrt i = i |> float_of_int |> sqrt |> ceil |> int_of_float;;

let rec gen_primes_to_tr acc i bound =
	if i = bound 
	then acc
	else    let sqrt_i = i |> float_of_int |> sqrt |> ceil |> int_of_float in
                if acc 
                |> List.filter (fun x -> x <= sqrt_i) 
                |> List.filter (fun x -> i mod x = 0) 
                |> List.is_empty
		then gen_primes_to_tr (i::acc) (i+1) bound
	else	gen_primes_to_tr acc (i+1) bound
;;

let gen_primes_to bound = gen_primes_to_tr [] 2 bound |> List.rev;;

let primes = gen_primes_to bound;;

let is_prime n = 
        if n < bound then primes |> List.exists(fun x -> x = n)
        else    List.init (int_sqrt n) (fun x -> x)
                |> List.filter (fun x -> n mod x = 0) 
                |> List.is_empty;;

let is_listsum_prime l =
        l |> List.fold_left (fun x y -> x + y) 0
        |> is_prime;;

let rec takewhiletotal_tr acc func l = 
        match l with
        h :: t -> if func (h::acc) then takewhiletotal_tr (h :: acc) func t
                else acc
        | [] -> acc;;

let takewhiletotal func l = takewhiletotal_tr [] func l |> List.rev;;


(* Next step is tricky. Need to get all viable sublists of the main list. Not easy! *)
