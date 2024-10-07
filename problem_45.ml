(*
Problem 45: Find the second number that is triangular, pentagonal, and hexagonal.

*)

open Printf

let bound = 4_000_000_000
(* First solve! 1:11 *)

let tri n = n*(n+1)/2;;
let pent n = n*(3*n-1)/2;;
let hex n = n*(2*n-1);;

print_endline "Generating lists.";;

let greater_than bound x = bound < x;;
let less_than    bound x = bound > x;;

let rec list_init_while_tr acc low high func test = 
        if (low > high)
        then acc
        else
                if (test (func low)) 
                        then list_init_while_tr ((func low)::acc) (low+1) high func test
                else acc

let list_init_while low high func test = 
        list_init_while_tr [] low high func test;;

(* This is simpler, but requires more computation since List.filter won't take advantage of ordering.
let tri_list  = List.init bound tri  |> List.filter (less_than bound);;
*)
let tri_list  = list_init_while 0 bound tri  (fun x -> x < bound) |> List.rev
let pent_list = list_init_while 0 bound pent (fun x -> x < bound) |> List.rev
let hex_list  = list_init_while 0 bound hex  (fun x -> x < bound) |> List.rev

let rec skip_while func xs =
        match xs with
        | h :: t -> if (func h) then skip_while func t else t
        | [] -> []

let rec list_contains xs x = 
        match xs with 
        | h :: t -> 
                if h = x then true 
                else if h > x then false
                else list_contains t x
        | [] -> false;;

let rec ordered_intersection acc list_a list_b = 
        match list_a with 
        h :: t -> (match list_b with 
                hb :: tb -> if h = hb 
                        then ordered_intersection (h :: acc) t tb 
                        else if h > hb then (* advance list_b*) ordered_intersection acc list_a tb 
                        else (* h < hb *) (*advance list_a*) ordered_intersection acc t list_b 
                | [] -> acc) 
        | [] -> acc;;

let rec list_intersection_tr acc list_a list_b = 
        (*let _ = print_endline "Acc =" in
        let _ = List.iter (printf "%d ") acc in
        let _ = print_endline "list_b = " in
        let _ = List.iter (printf "%d ") list_b in
        let _ = print_endline "" in*)
        match list_a with 
        | h::t -> if list_contains list_b h
                then list_intersection_tr (h::acc) t list_b (*(list_b |> skip_while (less_than h)) *)
                else list_intersection_tr  acc     t list_b (*(list_b |> skip_while (less_than h)) *)
        | [] -> acc;;

let list_intersection list_a list_b = list_intersection_tr [] list_a list_b;;

print_endline "Computing first intersection";;
(*let tri_and_pent = list_intersection tri_list pent_list;;*)
let tri_and_pent = ordered_intersection [] tri_list pent_list;;

print_endline "Computing second intersection";;
let tri_pent_and_hex = list_intersection tri_and_pent hex_list;;

List.iter (printf "%d ") tri_pent_and_hex;;
print_endline "";;

(*Printouts for testing*)

(*
print_endline "Tri";;
List.iter (Printf.printf "%d ") tri_list;;
print_endline "\n";;

print_endline "Pent";;
List.iter (Printf.printf "%d ") pent_list;;
print_endline "\n";;

print_endline "Hex";;
List.iter (Printf.printf "%d ") hex_list;;
print_endline "\n";;
*)

