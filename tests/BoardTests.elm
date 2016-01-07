module BoardTests (..) where

import ElmTest exposing (..)
import Board exposing (tileAt, placeTile)
import TileEdge exposing (TileEdge(..))
import Tile exposing (Tile, TilePlacement(..))
import TileType exposing (TileType(..))


tileAtAssertionOne =
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


tileAtAssertionTwo =
    test
        "returns Nothing when there is no tile"
        (assertEqual Nothing (tileAt [] ( 0, 0 )))


placeTileAssertionOne =
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


placeTileAssertionTwo =
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


tests : Test
tests =
    suite
        "BoardTests"
        [ tileAtAssertionOne
        , tileAtAssertionTwo
        , placeTileAssertionOne
        , placeTileAssertionTwo
        ]
