open Gocaml.Board
open Gocaml.Game_logic

let () =
  let board = get_9x9 () in
  let _ = board |> fun b -> place b (Place (Black, {x= 4; y= 4})) in
  print_string (string_of_board board)
