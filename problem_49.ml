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







