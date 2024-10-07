print_endline "Hello World"

(* We begin by generating primes *)

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

let rec map_while_func_tr acc map_func bound_func source_list =
        match source_list with
        | h :: t -> if (bound_func h)
                then map_while_func_tr ((map_func h) :: acc) map_func bound_func t
                else acc
        | [] -> acc
        ;;

let map_while_func map_func bound_func source_list = 
        map_while_func_tr [] map_func bound_func source_list;;

let odds = List.init bound (fun x -> 1 + 2*x);;
let squares = List.init bound (fun x -> x*x);;

let expressible = primes 
        |> List.map 
                (fun p -> map_while_func (fun s -> p + 2*s) (fun x -> x < bound) squares) 
        |> List.flatten 
        |> List.filter (fun x -> (x mod 2) = 1)
        |> List.sort_uniq (fun x y -> x - y);;

let unexpressible_odds = odds 
        |> List.filter (fun o -> List.exists (fun x -> x = o) expressible |> not)
        |> List.filter (fun x -> x < bound);;

print_endline "Here are the odds that we cannot express as a prime plus the sum of twice a square.";;
List.iter (Printf.printf "%d ") unexpressible_odds;;
print_endline "";;
