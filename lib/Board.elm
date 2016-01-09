module Board (tileAt, placeTile, canPlaceTileAt, getTilesAroundTile, potentialTileCoords, Board) where

import Tile exposing (Tile, zipCoordsWithEdgesAroundTile, canPlaceTileNextTo, TilePlacement(..), coordsAroundTile)
import Coord exposing (Coord)
import Maybe exposing (withDefault)
import Util exposing (zip, unsafeMaybe)
import List
import Debug


type alias Board =
    List Tile


potentialTileCoords : Board -> List Coord
potentialTileCoords board =
    List.concatMap coordsAroundTile board


placeTile : Board -> Tile -> Coord -> Board
placeTile board newTile ( newX, newY ) =
    case tileAt board ( newX, newY ) of
        Just _ ->
            board

        Nothing ->
            if canPlaceTileAt board newTile ( newX, newY ) then
                { newTile | x = Just newX, y = Just newY } :: board
            else
                board


canPlaceTileAt : Board -> Tile -> Coord -> Bool
canPlaceTileAt board newTile ( x, y ) =
    case tileAt board ( x, y ) of
        Just _ ->
            False

        Nothing ->
            let
                tileWithCoords = { newTile | x = Just x, y = Just y }
            in
                List.all
                    (\( tile, place ) ->
                        canPlaceTileNextTo newTile tile place
                    )
                    (getTilesAroundTile board tileWithCoords)


getTilesAroundTile : Board -> Tile -> List ( Tile, TilePlacement )
getTilesAroundTile board tile =
    zipCoordsWithEdgesAroundTile tile
        |> List.map (\( coords, place ) -> ( (tileAt board coords), place ))
        |> List.filter (\( tile, _ ) -> tile /= Nothing)
        |> List.map (\( tile, place ) -> ( (unsafeMaybe tile), place ))


tileAt : Board -> Coord -> Maybe Tile
tileAt board ( x, y ) =
    board
        |> List.filter (\t -> t.y == Just y && t.x == Just x)
        |> List.head
