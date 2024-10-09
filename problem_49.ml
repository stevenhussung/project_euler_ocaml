(*

The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: 
        (i) each of the three terms are prime, and, 
        (ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this sequence?
*)

(* Need to be able to parse through a string. (unsure of how important duplicates are here) *)

(* Lets mimic the Enum.frequencies function from Elixir *)

let rec list_of_int_tr acc n = 
        match n with
        | 0 -> acc
        | n -> list_of_int_tr ((n mod 10)::acc) (n/10)
;;

let list_of_int n = list_of_int_tr [] n;;


let rec count a list =
        list |>
        List.filter (fun x -> x = a) |>
        List.length
;;

12343434 
        |> list_of_int 
        |> count 4
        |> string_of_int
        |> print_endline
