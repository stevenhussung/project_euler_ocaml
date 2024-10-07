
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
