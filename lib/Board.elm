module Board (tileAt, placeTile) where

import Tile exposing (Tile, zipCoordsWithEdgesAroundTile, canPlaceTileNextTo, TilePlacement(..))
import Coord exposing (Coord)
import Maybe exposing (withDefault)
import Util exposing (zip, unsafeMaybe)
import List


type alias Board =
    List Tile


placeTile : Board -> Tile -> Coord -> Board
placeTile board newTile ( newX, newY ) =
    case tileAt board ( newX, newY ) of
        -- noop if there is already a cell there
        Just _ ->
            board

        Nothing ->
            let
                tileWithCoords = { newTile | x = Just newX, y = Just newY }

                placedTilesAround =
                    zipCoordsWithEdgesAroundTile tileWithCoords
                        |> List.map (\( coords, place ) -> ( (tileAt board coords), place ))
                        |> List.filter (\( tile, _ ) -> tile /= Nothing)
                        |> List.map (\( tile, place ) -> ( (unsafeMaybe tile), place ))

                canPlace =
                    List.all
                        (\( tile, place ) ->
                            canPlaceTileNextTo tileWithCoords place tile
                        )
                        placedTilesAround
            in
                if canPlace then
                    tileWithCoords :: board
                else
                    board


tileAt : Board -> Coord -> Maybe Tile
tileAt board ( x, y ) =
    board
        |> List.filter (\t -> t.y == Just y && t.x == Just x)
        |> List.head
