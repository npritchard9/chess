open Core

let bk_long_castle = 61
let bk_short_castle = 57
let wk_long_castle = 6
let wk_short_castle = 1

let parse_square sq =
  match String.to_array sq with
  | [| file; rank |] ->
      let file = 1 + (Char.to_int file - Char.to_int 'a') in
      let rank = Int.of_string @@ String.of_char rank in
      (rank * 8) - file
  | _ -> failwith "invalid square notation"

let parse_castle castle =
  (* needs work *)
  match String.count castle ~f:(Char.equal 'O') with
  | 2 -> bk_short_castle
  | 3 -> wk_long_castle
  | _ -> failwith "invalid castling notation"

let parse_move move =
  let piece_ids = "KQRBN" in
  match move with
  | _cap when String.exists move ~f:(Char.equal 'x') ->
      let first_char = String.get move 0 in
      if String.exists piece_ids ~f:(Char.equal first_char) then
        (first_char, parse_square move)
      else ('p', parse_square move)
      (*Nxd4 | bxc6 pawn uses file *)
  | _checkmate when String.exists move ~f:(Char.equal '#') ->
      let first_char = String.get move 0 in
      let get_rid_of_cm =
        String.take_while move ~f:(fun c -> not @@ Char.equal '#' c)
      in
      if String.exists piece_ids ~f:(Char.equal first_char) then
        (first_char, parse_square get_rid_of_cm)
      else ('p', parse_square get_rid_of_cm)
      (*Qh4#*)
  | _check when String.exists move ~f:(Char.equal '+') ->
      let first_char = String.get move 0 in
      let get_rid_of_ch =
        String.take_while move ~f:(fun c -> not @@ Char.equal '#' c)
      in
      if String.exists piece_ids ~f:(Char.equal first_char) then
        (first_char, parse_square get_rid_of_ch)
      else ('p', parse_square get_rid_of_ch)
      (*Bb5+*)
  | _castle when String.exists move ~f:(Char.equal 'O') ->
      ('k', parse_castle) (*O-O*)
  | "1-0" -> ('w', 0 (* white wins *))
  | "0-1" -> ('b', 0 (* black wins *))
  | "1/2-1/2" -> (' ', 0 (* draw *))
  | _pawn when String.length move = 2 -> ('p', parse_square move (*e4*))
  | _same_piece_diff_file when Char.is_alpha @@ String.get move 1 ->
      (String.get move 0, parse_square move) (*Nbd2*)
  | _same_piece_diff_rank when Char.is_digit @@ String.get move 1 ->
      (String.get move 0, parse_square move) (*N1d2*)
  | _move -> (String.get move 0, parse_square move)

(* let parse_capture cap =
  match String.split cap ~on:'x' with
  | [start; end] -> *)
