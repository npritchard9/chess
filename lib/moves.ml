(* open Core *)

type board = int64

(* let init_board () = (255 lsl 56) lor (255 lsl 48) lor (255 lsl 8) lor 255 *)
let init_board () = 0xFFFF00000000FFFFL
