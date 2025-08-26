open Core

(* fen begins from 8th rank and ends with 1st rank
for each rank, squares begin from the first file and go to the 8th
*)

(* let parse fen = () *)
let handle_char c = if Char.is_uppercase c then 1 else 0
