# gocaml

`gocaml` is a lightweight [go](https://en.wikipedia.org/wiki/Go_(game)) engine

## Usage

Requires Dune

If you'd like to play a game:
```bash
make run
```

If you'd like to use the API:
- `Gocaml.Board.board` - The data structure for representing board data
- `Gocaml.Game_logic.move` - The type used to submit moves to the engine
- `Gocaml.Game_logic.get_liberties board point` - Returns a list of liberties around the given point
- `Gocaml.Game_logic.assert_legality board move` - Determine if a move is illegal, and why
- `Gocaml.Game_logic.place board move` - Places a piece on the board, **does** check for legality and will remove captured pieces

## Navigation

- [game_logic.ml](lib/game_logic.ml) - The main engine code
- [board.ml](lib/board.ml) - Game board definitions
- [main.ml](bin/main.ml) - Driver code

## Credits

I depended on [wikibooks](https://en.wikibooks.org/wiki/Computer_Go) as my primary source of literature on Computer Go
