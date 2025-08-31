open Core

(*
need to have a way to handle the starting pos for a move so i can clear it out when a piece moves
also need to handle valid moves
*)

let default_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
let bk_long_castle = 61
let bk_short_castle = 57
let wk_long_castle = 6
let wk_short_castle = 1

type color = [ `White | `Black ]
type board_map = (char, int64, Base.Char.comparator_witness) Map_intf.Map.t

type t = {
  bitboards : board_map;
  castling_rights : bool * bool;
  turn : color;
  winner : color option;
}

let create ?(fen = default_fen) () =
  {
    bitboards = Fen.parse_fen fen;
    castling_rights = (true, true);
    turn = `White;
    winner = None;
  }

let get_pawn_type game = match game.turn with `White -> 'P' | `Black -> 'p'

let parse_square sq =
  match String.to_array sq with
  | [| file; rank |] ->
      let file = 1 + (Char.to_int file - Char.to_int 'a') in
      let rank = Int.of_string @@ String.of_char rank in
      (rank * 8) - file
  | _ -> failwith "invalid square notation"

let parse_castle game castle =
  match (game.turn, String.count castle ~f:(Char.equal 'O')) with
  | `White, 2 -> wk_short_castle
  | `White, 3 -> wk_long_castle
  | `Black, 2 -> bk_short_castle
  | `Black, 3 -> bk_long_castle
  | _ -> failwith "invalid castling notation"

let parse_move game move =
  let piece_ids = "KQRBN" in
  let square = String.suffix move 2 in
  match move with
  | _cap when String.exists move ~f:(Char.equal 'x') ->
      let first_char = String.get move 0 in
      if String.exists piece_ids ~f:(Char.equal first_char) then
        (first_char, parse_square square)
      else (get_pawn_type game, parse_square square)
      (*Nxd4 | bxc6 pawn uses file *)
  | _checkmate when String.exists move ~f:(Char.equal '#') ->
      let first_char = String.get move 0 in
      let get_rid_of_cm =
        String.take_while move ~f:(fun c -> not @@ Char.equal '#' c)
      in
      let square = String.suffix get_rid_of_cm 2 in
      if String.exists piece_ids ~f:(Char.equal first_char) then
        (first_char, parse_square square)
      else (get_pawn_type game, parse_square square)
      (*Qh4#*)
  | _check when String.exists move ~f:(Char.equal '+') ->
      let first_char = String.get move 0 in
      let get_rid_of_ch =
        String.take_while move ~f:(fun c -> not @@ Char.equal '+' c)
      in
      printf "%s\n" get_rid_of_ch;
      let square = String.suffix get_rid_of_ch 2 in
      if String.exists piece_ids ~f:(Char.equal first_char) then
        (first_char, parse_square square)
      else (get_pawn_type game, parse_square square)
      (*Bb5+*)
  | _castle when String.exists move ~f:(Char.equal 'O') ->
      ('k', parse_castle game move) (*O-O*)
  | "1-0" -> ('w', 0 (* white wins *))
  | "0-1" -> ('b', 0 (* black wins *))
  | "1/2-1/2" -> (' ', 0 (* draw *))
  | _pawn when String.length move = 2 -> (get_pawn_type game, parse_square move)
  (*e4*)
  | _same_piece_diff_file
    when String.length move = 4 && (Char.is_alpha @@ String.get move 1) ->
      (String.get move 0, parse_square square) (*Nbd2*)
  | _same_piece_diff_rank
    when String.length move = 4 && (Char.is_digit @@ String.get move 1) ->
      (String.get move 0, parse_square square) (*N1d2*)
  | _move ->
      printf "making normal move\n";
      (String.get move 0, parse_square square)

let make_move game move =
  let piece, square = parse_move game move in
  printf "%c, %d\n" piece square;
  let set_bit k idx m =
    Map.update m k
      ~f:
        (Option.value_map ~default:0L ~f:(fun piece ->
             Int64.(piece lxor (1L lsl idx))))
  in
  let bitboards = set_bit piece square game.bitboards in
  { game with bitboards }
