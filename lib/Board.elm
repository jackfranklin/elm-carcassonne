module Board (tileAt) where

import Tile exposing (Tile)
import Maybe exposing (withDefault)
import List
import Maybe.Extra exposing (..)


type alias Board =
    List Tile


type alias Coord =
    ( Int, Int )


tileAt : Board -> Coord -> Maybe Tile
tileAt board ( x, y ) =
    board
        |> List.filter (((/=) Nothing) << .x)
        |> List.filter (\t -> t.y ? 0 == y && t.x ? 0 == x)
        |> List.head
