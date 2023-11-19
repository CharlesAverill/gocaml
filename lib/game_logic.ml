open Board
open Logging

(** The kinds of moves a player can make *)
type move =
  | Place of color * point  (** Placing a stone at a point on the board*)
  | Pass  (** Passing *)

type illegal_move =
  | Legal
  | EmptyMove
  | OutOfBoundsMove
  | OccupiedMove
  | KoMove
  | SuicideMove

(** Get liberties at point *)
let rec get_liberties board point =
  let color = read board point in
  let flood = opposite color in
  write board point flood ;
  List.fold_left
    (fun liberties neighbor ->
      match read board neighbor with
      | c when c = color ->
          liberties @ get_liberties board neighbor
      | Empty ->
          neighbor :: liberties
      | _ ->
          liberties )
    [] (neighbors board point)

(** A connected group of stones and their liberties *)
type group = {points: point list; liberties: point list}

(** Get the list of points making up a group connected at a given point *)
let rec get_group_points board point : point list =
  let stones = ref [] in
  let my_color = read board point in
  if my_color = Empty then []
  else
    let floodfill_color = opposite my_color in
    write board point floodfill_color ;
    let process_neighbor stones neighbor =
      if read board neighbor = my_color && not (List.mem neighbor !stones) then
        stones := !stones @ get_group_points board neighbor
      else stones := !stones
    in
    let neighbors = neighbors board point in
    List.iter (process_neighbor stones) neighbors ;
    point :: !stones

(** Get a group *)
let get_group board point =
  let points = get_group_points (copy board) point in
  { points
  ; liberties=
      List.fold_left
        (fun libs stone ->
          libs
          @ List.filter (fun x -> read board x = Empty) (neighbors board stone)
          )
        [] points }

(** Determine if a point is in atari *)
let in_atari board point = List.length (get_liberties (copy board) point) = 1

(** Determine if a group is in atari *)
let group_in_atari group = List.length group.liberties = 1

(** Determine if a move would be a suicide *)
let is_suicide board point =
  let color = read board point in
  List.fold_left
    (fun s neighbor ->
      match read board neighbor with
      | Empty ->
          false
      | c when c = color && not (in_atari board neighbor) ->
          false
      | c when c = opposite color && in_atari board neighbor ->
          false
      | _ ->
          s )
    true (neighbors board point)

(** Assert the legality of a move
  @return (bool * illegal_move) The legality of the move, and the reason for an illegal move
 *)
let assert_legality board move =
  match move with
  | Place (Empty, _) ->
      (false, EmptyMove)
  | Place (_, point) when not (in_bounds board point) ->
      (false, OutOfBoundsMove)
  | Place (_, point) when read board point != Empty ->
      (false, OccupiedMove)
  | Place (_, point) when Some point = !(board.ko) ->
      (false, KoMove)
  | Place (_, point) when is_suicide board point ->
      (false, SuicideMove)
  | _ ->
      (true, Legal)

let place board move =
  let color, point =
    match move with
    | Place (c, p) ->
        (c, p)
    | _ ->
        fatal rc_Error "Tried to place a Pass"
  in
  ( match assert_legality board move with
  | false, s -> (
    match s with
    | EmptyMove ->
        fatal rc_IllegalMove "Attempted to place an Empty"
    | OutOfBoundsMove ->
        fatal rc_IllegalMove
          (Printf.sprintf "%s is out-of-bounds for board of dimension %s"
             (string_of_point point)
             (string_of_point board.shape) )
    | OccupiedMove ->
        fatal rc_IllegalMove "Can't place on a non-empty position"
    | KoMove ->
        fatal rc_IllegalMove "Rule of Ko"
    | SuicideMove ->
        fatal rc_IllegalMove "Suicide is illegal"
    | Legal ->
        () )
  | _ ->
      () ) ;
  let captured =
    List.fold_left
      (fun acc neighbor ->
        let enemy = get_group board neighbor in
        if read board neighbor = opposite color && group_in_atari enemy then (
          List.iter (fun e -> write board e Empty) enemy.points ;
          acc @ enemy.points )
        else acc )
      [] (neighbors board point)
  in
  board.ko := if List.length captured = 1 then Some (List.hd captured) else None ;
  write board point color
