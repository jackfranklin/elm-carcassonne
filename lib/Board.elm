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
        |> List.filter (\t -> t.y == Just y && t.x == Just x)
        |> List.head
