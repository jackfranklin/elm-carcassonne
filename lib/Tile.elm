module Tile (Tile, canPlaceTileNextTo, isPlaced, zipCoordsWithEdgesAroundTile, TilePlacement(..)) where

import TileType exposing (..)
import TileEdge exposing (..)
import Maybe.Extra exposing (..)
import Coord exposing (Coord)
import Util exposing (unsafeMaybe, zip)
import Debug


type TilePlacement
    = Left
    | Right
    | Above
    | Below


type alias Tile =
    { left : TileEdge
    , right : TileEdge
    , top : TileEdge
    , bottom : TileEdge
    , tileType : TileType
    , x : Maybe Int
    , y : Maybe Int
    }


coordsAroundTile : Tile -> List Coord
coordsAroundTile tile =
    case isPlaced tile of
        False ->
            []

        True ->
            let
                x = unsafeMaybe tile.x

                y = unsafeMaybe tile.y
            in
                [ ( x - 1, y ), ( x + 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]


zipCoordsWithEdgesAroundTile : Tile -> List ( Coord, TilePlacement )
zipCoordsWithEdgesAroundTile tile =
    case isPlaced tile of
        False ->
            []

        True ->
            let
                tileX = unsafeMaybe tile.x

                tileY = unsafeMaybe tile.y

                coords = coordsAroundTile tile
            in
                List.map
                    (\( x, y ) ->
                        if x < tileX && y == tileY then
                            ( ( x, y ), Left )
                        else if x > tileX && y == tileY then
                            ( ( x, y ), Right )
                        else if x == tileX && y < tileY then
                            ( ( x, y ), Below )
                        else
                            ( ( x, y ), Above )
                    )
                    coords


canPlaceTileNextTo : Tile -> Tile -> TilePlacement -> Bool
canPlaceTileNextTo placedTile newTile placement =
    case placement of
        Right ->
            placedTile.right == newTile.left

        Left ->
            placedTile.right == newTile.left

        Above ->
            placedTile.top == newTile.bottom

        Below ->
            placedTile.bottom == newTile.top


isPlaced : Tile -> Bool
isPlaced tile =
    tile.x /= Nothing && tile.y /= Nothing
