module TileTests (..) where

import ElmTest exposing (..)
import TileEdge exposing (TileEdge(..))
import Tile exposing (Tile, canPlaceTileNextTo, TilePlacement(..))
import TileType exposing (TileType(..))


canPlaceTileNextToWhenEdgesMatchAssertion =
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
        canPlace = canPlaceTileNextTo tile1 tile2 Right
    in
        test "two roads can join up" (assert canPlace)


cannotPlaceTilesNextToWhenEdgesDiffer =
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
        canPlace = canPlaceTileNextTo tile1 tile2 Right
    in
        test "a road and a grass cannot" (assert (not canPlace))


tests : Test
tests =
    suite
        "TileTests"
        [ canPlaceTileNextToWhenEdgesMatchAssertion
        , cannotPlaceTilesNextToWhenEdgesDiffer
        ]
