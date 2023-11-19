open Gocaml.Board
open Gocaml.Game_logic

let place_print color x y board =
  place board (Place (color, {x; y})) ;
  print_endline (string_of_board board ^ "\n==========================") ;
  board

let () =
  let board = get_9x9 () in
  let _ =
    board |> place_print Black 4 4 |> place_print White 4 5
    |> place_print White 3 4 |> place_print White 4 3 |> place_print White 5 4
  in
  ()
