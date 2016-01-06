module Tile (Tile, canPlaceTileNextTo, TilePlacement(..)) where

import TileType exposing (..)
import TileEdge exposing (..)


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


canPlaceTileNextTo : Tile -> TilePlacement -> Tile -> Bool
canPlaceTileNextTo placedTile placement newTile =
    case placement of
        Right ->
            placedTile.right == newTile.left

        Left ->
            placedTile.right == newTile.left

        Above ->
            placedTile.top == newTile.bottom

        Below ->
            placedTile.bottom == newTile.top
