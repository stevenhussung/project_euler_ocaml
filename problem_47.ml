print_endline "Hello again, friend of a friend";;


let bound = 1000000;;

let rec gen_primes_to_tr acc i bound =
	if i = bound then acc
	else if acc |> List.filter (fun x -> i mod x = 0) |> List.is_empty
		then gen_primes_to_tr (i::acc) (i+1) bound
	else	gen_primes_to_tr acc (i+1) bound
;;

let sqrt_bound = bound |> float_of_int |> sqrt |> ceil |> int_of_float;;

let gen_primes_to bound = gen_primes_to_tr [] 2 sqrt_bound |> List.rev;;

let primes = gen_primes_to bound;;

print_string "Number of primes: ";;
primes |> List.length |> string_of_int |> print_endline;;

let composites_div_counts = List.init bound (fun n ->
        (n, (primes |> List.filter (fun p -> (n mod p) = 0) |> List.length)));;

(*
composites_div_counts |> List.iter (fun tu -> match tu with (n, divisors) -> 
        Printf.printf "n = %d, with %d divisors\n" n divisors);;
*)


let rec find_cons_full n cons_count current_first tuple_list =
        match tuple_list with
        | h :: t -> 
                (
                        match h with (c, d) ->
                        if d = n then
                                if cons_count + 1 = n then current_first
                                else if cons_count > 0 then find_cons_full n (cons_count + 1) current_first t
                                else (* cons_count = 0 *) find_cons_full n (cons_count + 1) c t
                        else find_cons_full n 0 0 t
                )
        | [] -> 0;;

let find_cons n tuple_list = 
        find_cons_full n 0 0 tuple_list;;

composites_div_counts |> find_cons 2 |> string_of_int |> print_endline;;
composites_div_counts |> find_cons 3 |> string_of_int |> print_endline;;
composites_div_counts |> find_cons 4 |> string_of_int |> print_endline;;
