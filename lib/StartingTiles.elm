module StartingTiles (..) where

import TileType exposing (..)
import TileEdge exposing (..)
import Tile exposing (Tile)


makeTile : TileEdge -> TileEdge -> TileEdge -> TileEdge -> TileType -> Tile
makeTile l r t b ty =
    { x = Nothing
    , y = Nothing
    , left = l
    , right = r
    , top = t
    , bottom = b
    , tileType = ty
    }


starterTile : Tile
starterTile =
    { left = Road
    , right = Road
    , top = Castle
    , bottom = Grass
    , tileType = Generic
    , x = Just 0
    , y = Just 0
    }


tiles : List Tile
tiles =
    -- http://russcon.org/RussCon/carcassonne/tiles.html
    -- TODO: add all the tiles and make this random
    [ makeTile Grass Road Castle Road Generic
    , makeTile Grass Grass Road Road Generic
    , makeTile Grass Grass Road Road Generic
    ]
