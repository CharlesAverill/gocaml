(** 
  2-dimensional point
  *)
type point = {x: int; y: int}

(** String conversion for the point type *)
let string_of_point point = Printf.sprintf "(%d, %d)" point.x point.y

(**
  The color of a space on the board
  *)
type color = Black | White | Empty

let string_of_color = function
  | Black ->
      "Black"
  | White ->
      "White"
  | Empty ->
      "Empty"

(** Get the opposite of a color *)
let opposite color =
  match color with Black -> White | White -> Black | Empty -> Empty

(**
  The game board
  *)
type board =
  { shape: point  (** The (width, height) of the game board *)
  ; grid: color array
        (** A flattened matrix of (x, y) points representing the game board grid *)
  ; ko: point option ref
        (** Optionally, the point on the board illegal to play due to the rule of Ko *)
  }

(** Get an empty board of a given size *)
let get_board size =
  {shape= {x= size; y= size}; grid= Array.make (size * size) Empty; ko= ref None}

let get_19x19 () = get_board 19

let get_13x13 () = get_board 13

let get_9x9 () = get_board 9

(** Make a copy of a board *)
let copy board = {shape= board.shape; grid= Array.copy board.grid; ko= board.ko}

(** Convert an (x, y) point to an index *)
let xy_to_i board x y = (x * board.shape.x) + y

(** Check if a point is in the bounds of the board *)
let in_bounds board point =
  point.x >= 0 && point.y >= 0 && point.x < board.shape.x
  && point.y < board.shape.y

(** Get the color at a point on a board *)
let read board point = board.grid.(xy_to_i board point.x point.y)

(** Set the color at a point on a board *)
let write board point color =
  board.grid.(xy_to_i board point.x point.y) <- color

(** Get a string representation of the board *)
let string_of_board (b : board) : string =
  let row_to_string y =
    let col_to_string x =
      match read b {x; y} with White -> "O" | Black -> "X" | Empty -> "."
    in
    String.concat "" (List.init b.shape.x col_to_string)
  in
  String.concat "\n" (List.init b.shape.y row_to_string)

(** Get neighboring points *)
let neighbors (board : board) (point : point) =
  List.fold_left
    (fun acc offset ->
      let n = {x= point.x + offset.x; y= point.y + offset.y} in
      if in_bounds board n then n :: acc else acc )
    []
    [{x= 0; y= -1}; {x= -1; y= 0}; {x= 0; y= 1}; {x= 1; y= 0}]
