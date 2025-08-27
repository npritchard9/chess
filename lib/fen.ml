open Core

(* fen begins from 8th rank and ends with 1st rank
for each rank, squares begin from the first file and go to the 8th
*)

module CharMap = Map.Make (Char)

let create_map () =
  [
    ('k', 0L);
    ('q', 0L);
    ('r', 0L);
    ('b', 0L);
    ('n', 0L);
    ('p', 0L);
    ('K', 0L);
    ('Q', 0L);
    ('R', 0L);
    ('B', 0L);
    ('N', 0L);
    ('P', 0L);
    ('.', 0L);
  ]
  |> Map.of_alist_exn (module Char)

let parse_fen fen =
  let m = create_map () in
  let set_bit k idx m =
    Map.update m k
      ~f:
        (Option.value_map ~default:0L ~f:(fun piece ->
             Int64.(piece lxor (1L lsl idx))))
  in
  let rec read s rank file m =
    match s with
    | [] -> m
    | c :: rest -> (
        match c with
        | '/' -> read rest Int64.(rank - 1L) 1L m
        | n when Char.is_digit n ->
            read rest rank Int64.(file + (of_string @@ String.of_char c)) m
        | p when Char.is_alpha p ->
            let idx = Int64.((rank * 8L) - file) |> Int.of_int64_exn in
            set_bit p idx m |> set_bit '.' idx
            |> read rest rank Int64.(file + 1L)
        | _ -> failwith "unknown input")
  in
  read (String.to_list fen) 8L 1L m
