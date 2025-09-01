open Core
open Chess

let () =
  (* let fen = "r1bk3r/p2pBpNp/n4n2/1p1NP2P/6P1/3P4/P1P1K3/q5b1" in *)
  (* let fen = "8/8/8/4Q3/8/8/8/8" in *)
  let game = Game.create () in
  let board = Map.find_exn game.bitboards '.' in
  printf "board: %Ld\n" board;
  printf "board as string: %s\n" @@ Game.int64_to_board board;
  let game = Game.make_move game "Nc4" in
  let board = Map.find_exn game.bitboards '.' in
  printf "board: %Ld\n" board;
  printf "board as string: %s\n" @@ Game.int64_to_board board;
  ()
(* let moves = Game.gen_queen_moves game in *)
(* printf "moves: %Ld\n" moves;
  printf "moves as string: %s\n" @@ Game.int64_to_board moves *)
