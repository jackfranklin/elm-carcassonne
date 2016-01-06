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
    True
