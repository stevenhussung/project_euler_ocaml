(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button or [Ctrl-e]. *)

let a = 2;;

let tri n = n*(n+1)/2;;
let pent n = n*(3*n-1)/2;;
let hex n = n*(2*n-1);;

let rec is_form_search form a b n =
  let _ = print_endline(string_of_int (form a)) in
  let _ = print_endline(string_of_int (form b)) in
  if a = b || a+1 = b then false else
    let mid = (a + b)/2 in 
    let mid_form = form mid in
    if mid_form = n then true else
    if mid_form < n then is_form_search form mid b n
    else is_form_search form a mid n
;;

let is_form form n = is_form_search form 0 n n;;

let is_tri n = is_form tri n;;
let is_pent n = is_form pent n;;
let is_hex n = is_form hex n;;

is_tri(40755);;
is_pent(40755);;
is_hex(40755);;
