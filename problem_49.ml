(*

The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: 
        (i) each of the three terms are prime, and, 
        (ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this sequence?
*)


(* - - - First, auxiliary functions for testing candidates - - - *)
(* Need to be able to parse through a string. (unsure of how important duplicates are here) *)

(* Lets mimic the Enum.frequencies function from Elixir *)

let rec list_of_int_tr acc n = 
        match n with
        | 0 -> acc
        | n -> list_of_int_tr ((n mod 10)::acc) (n/10)
;;

let list_of_int n = list_of_int_tr [] n;;

let rec count a xs =
        xs |>
        List.filter (fun x -> x = a) |>
        List.length
;;

let frequencies xs = 
        xs
        |> List.map (fun a -> (a, count a xs))
        |> List.sort_uniq (fun a b -> match a with (a1, a2) -> (match b with (b1, b2) -> a1 - b1))
;;

let all_true xs = xs |> List.fold_left (fun x y -> x && y) true;;

let all_equal xs = xs |> List.map (fun x -> x = (List.hd xs)) |> all_true;;

let is_list_of_palindrome_ints candidates =
        candidates |> List.map list_of_int
                |> List.map frequencies
                |> all_equal 
;;
(* - - - Auxiliary functions are complete - - - *)
(* - - - Testing - - - *)

let candidates = [1487; 4817; 8147];;

print_endline "Checking original candidates:";;
candidates |> List.map (fun d -> Printf.printf "%d " d);;
print_endline "";;

print_string "Are these palindromes? " ;;
candidates |> is_list_of_palindrome_ints |> string_of_bool |> print_endline;;
        

let candidates = [1487; 4817; 9147];;

print_endline "Checking wrong candidates:";;
candidates |> List.map (fun d -> Printf.printf "%d " d);;
print_endline "";;

print_string "Are these palindromes? " ;;
candidates |> is_list_of_palindrome_ints |> string_of_bool |> print_endline;;

(* - - - Testing complete! - - - *)


(* Generate primes up to 10_000 (Copying from old code) *)
let bound = 10_000;;

let rec gen_primes_to_tr acc i bound =
	if i = bound 
	then acc
	else if acc |> List.filter (fun x -> i mod x = 0) |> List.is_empty
		then gen_primes_to_tr (i::acc) (i+1) bound
	else	gen_primes_to_tr acc (i+1) bound
;;

let gen_primes_to bound = gen_primes_to_tr [] 2 bound |> List.rev;;

let num_digits n = n |> string_of_int |> String.length;;

let primes = gen_primes_to bound
        |> List.filter (fun p -> num_digits p = 4)

let is_prime n = 
        primes |> List.exists(fun x -> x = n);;

(* Find arithmetic sequences *)

let find_pairwise_distance a primes = 
        let greater_primes = primes |> List.filter (fun p -> p > a) in
        greater_primes |> List.map (fun p -> (a, p - a))
;;

(* Next: test the length of a single pairwise distance tuple (or don't tuplize the arguments--your code) *)
let pairwise = primes 
        |> List.map(fun p -> find_pairwise_distance p primes) 
        |> List.flatten;;

let prime_sequence = pairwise
        |> List.map (fun (p, offset) -> (p, p + offset, p + 2 * offset))
        |> List.filter (fun (p1, p2, p3) -> num_digits p1 = 4 && num_digits p2 = 4 && num_digits p3 = 4)
        |> List.filter (fun (p1, p2, p3) -> [p1; p2; p3] |> is_list_of_palindrome_ints)
        |> List.filter (fun (p1, p2, p3) -> is_prime p1 && is_prime p2 && is_prime p3)
;;

prime_sequence 
        |> List.map (fun (p1, p2, p3) -> Printf.printf "%d, %d, %d\n" p1 p2 p3);;
