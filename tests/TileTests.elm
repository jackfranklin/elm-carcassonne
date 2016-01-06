module TileTests (..) where

import ElmTest exposing (..)
import TileEdge exposing (TileEdge(..))
import Tile exposing (Tile, TilePlacement(..), canPlaceTileNextTo)
import TileType exposing (TileType(..))


canPlaceAssertionOne =
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

        tile2 =
            { left = Road
            , right = Grass
            , bottom = Road
            , top = Grass
            , tileType = Generic
            , x = Nothing
            , y = Nothing
            }

        -- can we put tile2 to the right of tile 1?
        canPlace = canPlaceTileNextTo tile1 Right tile2
    in
        test "two roads can join up" (assert canPlace)


canPlaceAssertionTwo =
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

        tile2 =
            { left = Grass
            , right = Grass
            , bottom = Road
            , top = Grass
            , tileType = Generic
            , x = Nothing
            , y = Nothing
            }

        -- can we put tile2 to the right of tile 1?
        canPlace = canPlaceTileNextTo tile1 Right tile2
    in
        test "a road and a grass cannot" (assert (not canPlace))


tests : Test
tests =
    suite
        "TileTests"
        [ canPlaceAssertionOne, canPlaceAssertionTwo ]
