module BoardTests (..) where

import ElmTest exposing (..)
import Board exposing (tileAt, placeTile)
import TileEdge exposing (TileEdge(..))
import Tile exposing (Tile, TilePlacement(..))
import TileType exposing (TileType(..))


tileAtReturnsJustTileAssertion =
    let
        tile1 =
            { left = Road
            , right = Road
            , bottom = Grass
            , top = Grass
            , tileType = Generic
            , x = Just 0
            , y = Just 0
            }

        board = [ tile1 ]
    in
        test
            "the board can fetch the tile at coords"
            (assertEqual (Just tile1) (tileAt board ( 0, 0 )))


tileAtReturnsNothingForNoTileAssertion =
    test
        "returns Nothing when there is no tile"
        (assertEqual Nothing (tileAt [] ( 0, 0 )))


placeTileOnEmptyBoardAssertion =
    let
        tile1 =
            { left = Road
            , right = Road
            , bottom = Grass
            , top = Grass
            , tileType = Generic
            , x = Nothing
            , y = Nothing
            }

        tile2 = { tile1 | x = Just 0, y = Just 0 }
    in
        test
            "I can place a tile on an empty board and have its coords update"
            (assertEqual [ tile2 ] (placeTile [] tile1 ( 0, 0 )))


placeTileInSamePlaceAsExistingAssertion =
    let
        tile1 =
            { left = Road
            , right = Road
            , bottom = Grass
            , top = Grass
            , tileType = Generic
            , x = Just 0
            , y = Just 0
            }

        board = [ tile1 ]

        tile2 =
            { left = Road
            , right = Grass
            , bottom = Grass
            , top = Grass
            , tileType = RoadEnding
            , x = Nothing
            , y = Nothing
            }
    in
        test
            "I can't place a tile where there is already one"
            (assertEqual [ tile1 ] (placeTile [ tile1 ] tile2 ( 0, 0 )))


placeTilePreventsIllegalMoveAssertion =
    let
        tile1 =
            { left = Road
            , right = Road
            , bottom = Grass
            , top = Grass
            , tileType = Generic
            , x = Just 0
            , y = Just 0
            }

        -- try to place tile2 to the right of tile1
        --  should fail as edges are not the saem
        tile2 =
            { left = Grass
            , right = Grass
            , bottom = Castle
            , top = Grass
            , tileType = Generic
            , x = Nothing
            , y = Nothing
            }
    in
        test
            "Can't place a tile where the edges don't line up"
            (assertEqual [ tile1 ] (placeTile [ tile1 ] tile2 ( 1, 0 )))


tests : Test
tests =
    suite
        "BoardTests"
        [ tileAtReturnsJustTileAssertion
        , tileAtReturnsNothingForNoTileAssertion
        , placeTileOnEmptyBoardAssertion
        , placeTileInSamePlaceAsExistingAssertion
        , placeTilePreventsIllegalMoveAssertion
        ]
