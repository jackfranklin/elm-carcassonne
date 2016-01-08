module MyApp (..) where

import Graphics.Element exposing (..)
import Window
import Maybe.Extra exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Tile exposing (..)
import Board exposing (..)
import Signal
import String
import TileEdge exposing (TileEdge)
import TileType exposing (TileType)
import Coord exposing (Coord)
import StartingTiles


type alias Model =
    { availableTiles : List Tile
    , board : Board
    }


initialModel : Model
initialModel =
    { availableTiles = StartingTiles.tiles
    , board = [ StartingTiles.starterTile ]
    }


type Action
    = NoOp
    | RotateTile Tile
    | PlaceTile Coord
    | PlacePerson Coord


actions : Signal.Mailbox Action
actions =
    Signal.mailbox NoOp


update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        RotateTile tile ->
            -- TODO
            model

        PlaceTile ( x, y ) ->
            model

        PlacePerson ( x, y ) ->
            -- TODO
            model


valToText : a -> Html
valToText =
    text << toString


renderTileEdge : TileEdge -> String -> Html
renderTileEdge edge className =
    div
        [ class ("tile-edge tile-" ++ (edge |> toString |> String.toLower) ++ " tile-" ++ className) ]
        [ edge
            |> toString
            |> String.left 1
            |> text
        ]


renderTileType : TileType -> Html
renderTileType tileType =
    div
        [ class ("tile-type tile-type-" ++ (tileType |> toString |> String.toLower))
        ]
        [ text <| String.left 1 <| toString <| tileType ]


topCssForTile : Int -> Tile -> String
topCssForTile winY tile =
    (toString ((winY // 2) - ((tile.y ? 0) * 60))) ++ "px"


leftCssForTile : Int -> Tile -> String
leftCssForTile winX tile =
    (toString ((winX // 2) - ((tile.y ? 0) * 60))) ++ "px"


tileStyle : ( Int, Int ) -> Tile -> Html.Attribute
tileStyle ( winX, winY ) tile =
    style
        [ ( "top", topCssForTile winY tile )
        , ( "left", leftCssForTile winX tile )
        , ( "backgroundColor", "red" )
        ]


renderTile : ( Int, Int ) -> Tile -> Html
renderTile dimensions tile =
    div
        [ class "tile"
        , tileStyle dimensions tile
        ]
        [ renderTileEdge tile.top "top"
        , renderTileEdge tile.left "left"
        , renderTileEdge tile.right "right"
        , renderTileEdge tile.bottom "bottom"
        , renderTileType tile.tileType
        ]


renderBoard : Signal.Address Action -> ( Int, Int ) -> Model -> Html
renderBoard address dimensions model =
    div [] (List.map (renderTile dimensions) model.board)


view : Signal.Address Action -> ( Int, Int ) -> Model -> Html
view address dimensions model =
    div
        []
        [ renderBoard address dimensions model ]


model : Signal Model
model =
    Signal.foldp update initialModel actions.signal


main : Signal Html
main =
    Signal.map2 (view actions.address) Window.dimensions model



-- Signal.map (view actions.address) model
