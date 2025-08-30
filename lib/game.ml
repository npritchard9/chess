open Core

let default_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

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

let make_move move =
  let _parsed_move = Moves.parse_move move in
  ()
