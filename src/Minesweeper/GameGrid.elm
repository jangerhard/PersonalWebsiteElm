module Minesweeper.GameGrid exposing (Content(..), Tile, TileState(..), autoSolve, flagTile, gridGenerator, isInWinState, showAllFlagged, showTile, toggleBlank, toggleHidden)

import Grid exposing (Grid)
import Random
import RandomHelpers


type Content
    = Mine
    | Number Int
    | Blank


type TileState
    = Hidden
    | Shown
    | Flagged


type alias UnplacedTile =
    { content : Content, state : TileState }


type alias Tile =
    { content : Content, state : TileState, x : Int, y : Int }


dummyGrid : Int -> Grid UnplacedTile
dummyGrid size =
    Grid.repeat size size { content = Number -1, state = Shown }


gridGenerator : Int -> Random.Generator ( Grid Tile, Maybe Tile )
gridGenerator size =
    let
        probabilityForMine =
            0.18

        withRandom : Grid Tile -> Random.Generator ( Grid Tile, Maybe Tile )
        withRandom grid =
            Grid.foldl (::) [] grid
                |> List.filter (\tile -> tile.content == Blank)
                |> RandomHelpers.chooseRandomGenerator
                |> Random.map (\( maybe, _ ) -> ( grid, maybe ))

        tileGenerator : Random.Generator UnplacedTile
        tileGenerator =
            Random.float 0 1
                |> Random.map
                    (\probability ->
                        { content =
                            if probability < probabilityForMine then
                                Mine

                            else
                                Blank
                        , state = Shown
                        }
                    )

        placeTile : Int -> Int -> UnplacedTile -> Tile
        placeTile rI cI tile =
            Tile tile.content tile.state rI cI
    in
    List.repeat size tileGenerator
        |> RandomHelpers.sequence
        |> List.repeat size
        |> RandomHelpers.sequence
        |> Random.map
            (\gen ->
                Maybe.withDefault (dummyGrid size) (Grid.fromList gen)
                    |> Grid.indexedMap placeTile
                    |> prepareGrid
            )
        |> Random.andThen withRandom


toggleHidden : Grid Tile -> Grid Tile
toggleHidden grid =
    Grid.map
        (\tile ->
            { tile
                | state =
                    case tile.state of
                        Hidden ->
                            Shown

                        Shown ->
                            Hidden

                        Flagged ->
                            Flagged
            }
        )
        grid


surroundingTiles : Tile -> Grid Tile -> List Tile
surroundingTiles { x, y } grid =
    [ Grid.get ( x - 1, y - 1 ) grid
    , Grid.get ( x - 1, y ) grid
    , Grid.get ( x - 1, y + 1 ) grid
    , Grid.get ( x, y - 1 ) grid
    , Grid.get ( x, y + 1 ) grid
    , Grid.get ( x + 1, y - 1 ) grid
    , Grid.get ( x + 1, y ) grid
    , Grid.get ( x + 1, y + 1 ) grid
    ]
        |> List.filterMap identity


isInWinState : Grid Tile -> Bool
isInWinState grid =
    let
        check : Tile -> Bool -> Bool
        check tile bool =
            bool
                && (case ( tile.content, tile.state ) of
                        ( Number _, Hidden ) ->
                            False

                        _ ->
                            True
                   )
    in
    Grid.foldl check True grid


showAllFlagged : Grid Tile -> Grid Tile
showAllFlagged =
    Grid.map
        (\tile ->
            if tile.state == Flagged then
                { tile | state = Shown }

            else
                tile
        )


updateGrid : Tile -> Grid Tile -> Grid Tile
updateGrid tile =
    Grid.set ( tile.x, tile.y ) tile


showTile : Tile -> Grid Tile -> Grid Tile
showTile tile =
    { tile | state = Shown }
        |> updateGrid


flagTile : Tile -> Grid Tile -> Grid Tile
flagTile tile =
    { tile
        | state =
            case tile.state of
                Hidden ->
                    Flagged

                Shown ->
                    tile.state

                Flagged ->
                    Hidden
    }
        |> updateGrid


toggleBlank : Tile -> Grid Tile -> Grid Tile
toggleBlank currentTile initialGrid =
    case ( currentTile.state, currentTile.content ) of
        ( Hidden, Blank ) ->
            showTile currentTile initialGrid
                |> (\updatedGrid ->
                        surroundingTiles currentTile updatedGrid
                            |> List.filter (\tile -> tile.state == Hidden)
                            |> List.foldl toggleBlank updatedGrid
                   )

        ( Hidden, Number _ ) ->
            showTile currentTile initialGrid

        _ ->
            initialGrid


autoSolve : Grid Tile -> Grid Tile
autoSolve grid =
    Grid.map (\tile -> { tile | state = Shown }) grid


numberOfSurroundingMines : Tile -> Grid Tile -> Int
numberOfSurroundingMines tile grid =
    surroundingTiles tile grid
        |> List.filter (\neighbour -> neighbour.content == Mine)
        |> List.length


prepareGrid : Grid Tile -> Grid Tile
prepareGrid oldGrid =
    let
        placeNumbers : Tile -> Tile
        placeNumbers tile =
            case tile.content of
                Blank ->
                    case numberOfSurroundingMines tile oldGrid of
                        0 ->
                            tile

                        n ->
                            { tile | content = Number n }

                _ ->
                    tile

        turnBlank : Tile -> Tile
        turnBlank tile =
            { tile | state = Hidden }
    in
    Grid.map placeNumbers oldGrid
        |> Grid.map turnBlank
