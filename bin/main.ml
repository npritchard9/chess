open Core
open Chess

let () =
  let m = Fen.parse_fen "r1bk3r/p2pBpNp/n4n2/1p1NP2P/6P1/3P4/P1P1K3/q5b1" in
  let board = Map.find_exn m '.' in
  printf "%Ld\n" board
