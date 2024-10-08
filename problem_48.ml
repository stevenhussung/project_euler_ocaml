(* Self Powers

1^1 + 2^2 + 3^3 ... + 10^10 = 10405071317

Find the last 10 digits of the sequence

1^1 + 2^2 + 3^3 ... + 1000^1000.
 *)

open Printf;;

print_endline "I knew you well";;


let rec power_mod power modulus base = 
        (*let _ = printf "taking %d to the %d power mod %d\n" base power modulus in*)
        (match power with
        | 0 -> 1
        | 1 -> base
        | n -> base * power_mod (power - 1) modulus base

                (* This is very efficient, but can cause integer overflows
                let b = power_mod (n/2) modulus base in
                b * b * (if n mod 2 = 0 then 1 else base)
                *)
        ) mod modulus
;;

1234 |> power_mod 2 100 |> string_of_int |> print_endline;;

let big_int = 1 lsl 61
let sum list_to_sum = List.fold_left (fun a b -> a + b) 0 list_to_sum;;

let init_inclusive n func = List.init n (fun n -> n + 1) |> List.map func;;

init_inclusive 10 (fun n -> power_mod n big_int n) |> sum |> string_of_int |> print_endline;


print_endline "Now the big one:";;
init_inclusive 1000 (fun n -> power_mod n 10_000_000_000 n) 
        |> sum 
        |> (fun n -> n mod 10_000_000_000)
        |> string_of_int 
        |> print_endline;
