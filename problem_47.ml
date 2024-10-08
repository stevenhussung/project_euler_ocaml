print_endline "Hello again, friend of a friend";;


let bound = 10000;;

let rec gen_primes_to_tr acc i bound =
	if i = bound 
	then acc
	else if acc |> List.filter (fun x -> i mod x = 0) |> List.is_empty
		then gen_primes_to_tr (i::acc) (i+1) bound
	else	gen_primes_to_tr acc (i+1) bound
;;

let gen_primes_to bound = gen_primes_to_tr [] 2 bound |> List.rev;;

let primes = gen_primes_to bound;;

print_string "Number of primes: ";;
primes |> List.length |> string_of_int |> print_endline;;

let composites_div_counts = List.init bound (fun n ->
        (n, (primes |> List.filter (fun p -> (n mod p) = 0) |> List.length)));;

(*
composites_div_counts |> List.iter (fun tu -> match tu with (n, divisors) -> 
        Printf.printf "n = %d, with %d divisors\n" n divisors);;
*)


let rec consecutive_with_n_divisors n tuple_list =
        match tuple_list with
        | h1 :: h2 :: t -> 
                (match h1 with (c1, d1) ->
                (match h2 with (c2, d2) ->
                        if d1 = n && d2 = n
                                then c1
                                else consecutive_with_n_divisors n (h2 :: t)
                ))
        | h :: t -> 0
        | [] -> 0;;

composites_div_counts |> consecutive_with_n_divisors 2 |> string_of_int |> print_endline;;
