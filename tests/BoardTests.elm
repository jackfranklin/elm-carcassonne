module BoardTests (..) where

import ElmTest exposing (..)
import Board exposing (tileAt)
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


tests : Test
tests =
    suite
        "BoardTests"
        [ tileAtAssertionOne, tileAtAssertionTwo ]
