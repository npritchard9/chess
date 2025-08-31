(* open Core *)
open Chess

let () =
  let fen = "r1bk3r/p2pBpNp/n4n2/1p1NP2P/6P1/3P4/P1P1K3/q5b1" in
  let game = Game.create ~fen () in
  Game.make_move game "dxe4"
