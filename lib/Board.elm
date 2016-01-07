module Board (tileAt, placeTile) where

import Tile exposing (Tile)
import Maybe exposing (withDefault)
import List
import Maybe.Extra exposing (..)


type alias Board =
    List Tile


type alias Coord =
    ( Int, Int )


placeTile : Board -> Tile -> Coord -> Board
placeTile board newTile ( newX, newY ) =
    case tileAt board ( newX, newY ) of
        -- noop if there is already a board there
        Just _ ->
            board

        Nothing ->
            { newTile | x = Just newX, y = Just newY } :: board


tileAt : Board -> Coord -> Maybe Tile
tileAt board ( x, y ) =
    board
        |> List.filter (\t -> t.y == Just y && t.x == Just x)
        |> List.head
