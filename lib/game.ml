open Core

(*
need to have a way to handle the starting pos for a move so i can clear it out when a piece moves
also need to handle valid moves
*)

let default_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
let rank1 = 0x00000000000000FFL
let rank2 = 0x000000000000FF00L
let rank7 = 0x00FF000000000000L
let rank8 = 0xFF00000000000000L
let not_rank_1 = Int64.lnot rank1
let not_rank_2 = Int64.lnot rank2
let not_rank_7 = Int64.lnot rank7
let not_rank_8 = Int64.lnot rank8
let a_file = 0x8080808080808080L
let b_file = 0x4040404040404040L
let g_file = 0x0202020202020202L
let h_file = 0x0101010101010101L
let not_a_file = Int64.lnot a_file
let not_h_file = Int64.lnot h_file
let not_ab_file = Int64.(lnot (a_file lor b_file))
let not_gh_file = Int64.(lnot (g_file lor h_file))

(* need to handle rook moving on castles too *)
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
let is_bit_set bitboard square = Int64.((1L lsl square) land bitboard <> 0L)

let int64_to_board n =
  let rec loop i s =
    if i < 0 then s
    else
      loop (i - 1)
        (s
        ^
        match (is_bit_set n i, i mod 8 = 0) with
        | true, true -> "1\n"
        | true, false -> "1"
        | false, true -> "0\n"
        | _ -> "0")
  in
  loop 63 "\n"

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
  | _move -> (String.get move 0, parse_square square)

let rec slide src shift shift_fn mask acc own_pieces opp_pieces =
  let shifted = Int64.(shift_fn (src land mask) shift) in
  if Int64.(shifted = 0L) then acc
  else
    let acc = Int64.(acc lor shifted) in
    if Int64.(shifted land own_pieces <> 0L) then acc
    else if Int64.(shifted land opp_pieces <> 0L) then acc
    else slide shifted shift shift_fn mask acc own_pieces opp_pieces

let gen_pawn_moves game =
  let pawn, opp_key, starting_rank =
    match game.turn with
    | `White -> ('P', '0', rank2)
    | `Black -> ('p', '1', rank7)
  in
  let board = Map.find_exn game.bitboards '.' in
  let pawns = Map.find_exn game.bitboards pawn in
  let opp = Map.find_exn game.bitboards opp_key in
  let empty = Int64.lnot board in

  let single = Int64.((pawns lsl 8) land empty) in
  let on_starting_rank = Int64.(pawns land starting_rank) in
  let single_targets = Int64.((on_starting_rank lsl 8) land empty) in
  let double = Int64.((single_targets lsl 8) land empty) in
  let can_cap_left = Int64.(pawns land not_a_file) in
  let cap_left = Int64.((can_cap_left lsl 9) land opp) in
  let can_cap_right = Int64.(pawns land not_h_file) in
  let cap_right = Int64.((can_cap_right lsl 7) land opp) in
  Int64.(single lor double lor cap_left lor cap_right)

let gen_knight_moves game =
  let knight, color =
    match game.turn with `White -> ('N', '1') | `Black -> ('n', '0')
  in
  let knights = Map.find_exn game.bitboards knight in
  let pieces = Map.find_exn game.bitboards color in
  let open_squares = Int64.lnot pieces in
  let m1 = Int64.((knights land not_gh_file) lsl 6) in
  let m2 = Int64.((knights land not_ab_file) lsl 10) in
  let m3 = Int64.((knights land not_h_file) lsl 15) in
  let m4 = Int64.((knights land not_a_file) lsl 17) in
  let m5 = Int64.((knights land not_ab_file) lsr 6) in
  let m6 = Int64.((knights land not_gh_file) lsr 10) in
  let m7 = Int64.((knights land not_a_file) lsr 15) in
  let m8 = Int64.((knights land not_h_file) lsr 17) in
  let all_moves = Int64.(m1 lor m2 lor m3 lor m4 lor m5 lor m6 lor m7 lor m8) in
  Int64.(all_moves land open_squares)

let gen_bishop_moves game =
  let bishop, color, opp_color =
    match game.turn with `White -> ('B', '1', '0') | `Black -> ('b', '0', '1')
  in
  let own_pieces = Map.find_exn game.bitboards color in
  let opp_pieces = Map.find_exn game.bitboards opp_color in
  let bishops = Map.find_exn game.bitboards bishop in
  let ne = slide bishops 7 Int64.( lsl ) not_h_file 0L own_pieces opp_pieces in
  let nw = slide bishops 9 Int64.( lsl ) not_a_file 0L own_pieces opp_pieces in
  let se = slide bishops 9 Int64.( lsr ) not_h_file 0L own_pieces opp_pieces in
  let sw = slide bishops 7 Int64.( lsr ) not_a_file 0L own_pieces opp_pieces in
  Int64.(ne lor nw lor se lor sw)

let gen_rook_moves game =
  let rook, color, opp_color =
    match game.turn with `White -> ('R', '1', '0') | `Black -> ('r', '0', '1')
  in
  let own_pieces = Map.find_exn game.bitboards color in
  let opp_pieces = Map.find_exn game.bitboards opp_color in
  let rooks = Map.find_exn game.bitboards rook in
  let n = slide rooks 8 Int64.( lsl ) not_rank_8 0L own_pieces opp_pieces in
  let s = slide rooks 8 Int64.( lsr ) not_rank_1 0L own_pieces opp_pieces in
  let e = slide rooks 1 Int64.( lsl ) not_a_file 0L own_pieces opp_pieces in
  let w = slide rooks 1 Int64.( lsr ) not_h_file 0L own_pieces opp_pieces in
  Int64.(n lor s lor e lor w)

let gen_queen_moves game =
  let queen, color, opp_color =
    match game.turn with `White -> ('Q', '1', '0') | `Black -> ('q', '0', '1')
  in
  let own_pieces = Map.find_exn game.bitboards color in
  let opp_pieces = Map.find_exn game.bitboards opp_color in
  let queen = Map.find_exn game.bitboards queen in
  let n = slide queen 8 Int64.( lsl ) not_rank_8 0L own_pieces opp_pieces in
  let s = slide queen 8 Int64.( lsr ) not_rank_1 0L own_pieces opp_pieces in
  let e = slide queen 1 Int64.( lsl ) not_a_file 0L own_pieces opp_pieces in
  let w = slide queen 1 Int64.( lsr ) not_h_file 0L own_pieces opp_pieces in
  let ne = slide queen 7 Int64.( lsl ) not_h_file 0L own_pieces opp_pieces in
  let nw = slide queen 9 Int64.( lsl ) not_a_file 0L own_pieces opp_pieces in
  let se = slide queen 9 Int64.( lsr ) not_h_file 0L own_pieces opp_pieces in
  let sw = slide queen 7 Int64.( lsr ) not_a_file 0L own_pieces opp_pieces in
  Int64.(n lor s lor e lor w lor ne lor nw lor se lor sw)

let gen_king_moves game =
  printf "%b\n" @@ snd game.castling_rights;
  Int64.zero

let get_gen_move_fn piece =
  match Char.lowercase piece with
  | 'k' -> gen_king_moves
  | 'q' -> gen_queen_moves
  | 'r' -> gen_rook_moves
  | 'b' -> gen_bishop_moves
  | 'n' -> gen_knight_moves
  | 'p' -> gen_pawn_moves
  | _ -> failwith "invalid piece"

let make_move game move =
  let piece, square = parse_move game move in
  let valid_moves = game |> get_gen_move_fn piece in
  let is_valid = is_bit_set valid_moves square in
  match is_valid with
  | false -> game
  | true ->
      printf "valid: %b\n" is_valid;
      printf "%c, %d\n" piece square;
      let set_bit k idx m =
        Map.update m k
          ~f:
            (Option.value_map ~default:0L ~f:(fun piece ->
                 Int64.(piece lxor (1L lsl idx))))
      in
      let color = match game.turn with `White -> '1' | `Black -> '0' in
      let bitboards =
        set_bit piece square game.bitboards
        |> set_bit color square |> set_bit '.' square
      in
      { game with bitboards }
