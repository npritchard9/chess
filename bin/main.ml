open Core
open Chess

let () =
  let fen = "r1bk3r/p2pBpNp/n4n2/1p1NP2P/6P1/3P4/P1P1K3/q5b1" in
  let game = Game.create ~fen () in
  printf "%b\n" @@ fst game.castling_rights

(* printf "%a\n" Game.show game *)
(* let board = Map.find_exn m '.' in
  printf "%Ld\n" board; *)
(* printf "%d\n" @@ Moves.parse_square "d4" *)
