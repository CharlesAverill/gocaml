open Gocaml.Board
open Gocaml.Game_logic

let place_print color x y board =
  place board (Place (color, {x; y})) ;
  print_endline (string_of_board board ^ "\n==========================")

(* let () =
   let board = get_9x9 () in
   let _ =
     board |> place_print Black 4 4 |> place_print White 4 5
     |> place_print White 3 4 |> place_print White 4 3 |> place_print White 5 4
   in
   () *)

let () =
  let size = ref None in
  while !size = None do
    print_string "Board size? (integer) " ;
    size := read_int_opt ()
  done ;
  let board = get_board (match !size with Some x -> x | None -> 19) in
  let last_passed = ref false in
  let game_over = ref false in
  let turn = ref Black in
  while not !game_over do
    print_endline (string_of_color !turn ^ "'s turn") ;
    print_string "Pass or place? " ;
    match String.lowercase_ascii (read_line ()) with
    | "pass" ->
        game_over := !last_passed ;
        last_passed := true ;
        turn := opposite !turn
    | "place" -> (
        let move_x, move_y = (ref None, ref None) in
        print_string "Move X Coordinate: " ;
        move_x := read_int_opt () ;
        print_string "Move Y Coordinate: " ;
        move_y := read_int_opt () ;
        if !move_x = None || !move_y = None then print_endline "Bad coordinates"
        else
          let x, y =
            match (!move_x, !move_y) with
            | Some x, Some y ->
                (x, y)
            | _ ->
                (0, 0)
          in
          print_string (Printf.sprintf "Confirm move (%d, %d)? (y/n) " x y) ;
          match Char.lowercase_ascii (read_line ()).[0] with
          | 'y' ->
              place_print !turn x y board ;
              turn := opposite !turn ;
              last_passed := false
          | _ ->
              () )
    | _ ->
        print_endline "I didn't understand"
  done
