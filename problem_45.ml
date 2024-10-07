(*
Problem 45: Find the second number that is triangular, pentagonal, and hexagonal.

*)

open Printf

let bound = 30000

let tri n = n*(n+1)/2
let pent n = n*(3*n-1)/2
let hex n = n*(2*n-1)

let tri_list  = List.init bound tri;;
let pent_list = List.init bound pent;;
let hex_list  = List.init bound hex;;

let rec list_contains x list = 
        match list with 
        | h :: t -> if h = x then true else list_contains x t 
        | [] -> false;;

let rec list_intersection list_a list_b = 
        match list_a with 
        | h::t -> if list_contains h list_b 
                then h :: list_intersection t list_b 
                else list_intersection t list_b 
        | [] -> [];;

let tri_and_pent = list_intersection tri_list pent_list;;
let tri_pent_and_hex = list_intersection tri_and_pent hex_list;;

List.iter (printf "%d ") tri_pent_and_hex;;

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

(*
(* This isn't the dominant way we'll look for these *)
let rec is_form_search form a b n =
  (*let _ = print_endline(string_of_int (form a)) in
  let _ = print_endline(string_of_int (form b)) in*)
  if a = b || a+1 = b then false else
    let mid = (a + b)/2 in 
    let mid_form = form mid in
    if mid_form = n then true else
    if mid_form < n then is_form_search form mid b n
    else is_form_search form a mid n


let is_form form n = is_form_search form 0 n n

let is_tri n = is_form tri n;;
let is_pent n = is_form pent n;;
let is_hex n = is_form hex n;;

print_endline (string_of_bool (is_tri 40755));;
print_endline (string_of_bool (is_pent 40755));;
print_endline (string_of_bool (is_hex 40755));;
*)
