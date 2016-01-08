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
    , nextTile : Tile
    , board : Board
    }


initialModel : Model
initialModel =
    { availableTiles = (List.tail StartingTiles.tiles) ? []
    , nextTile = (List.head StartingTiles.tiles) ? StartingTiles.fakeTile
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


calculateCssPositionForCoord : Int -> Int -> String
calculateCssPositionForCoord window tile =
    (toString ((window // 2) - (tile * 60))) ++ "px"


topCssForTile : Int -> Tile -> String
topCssForTile winY tile =
    calculateCssPositionForCoord winY (tile.y ? 0)


leftCssForTile : Int -> Tile -> String
leftCssForTile winX tile =
    calculateCssPositionForCoord winX (tile.x ? 0)


tileStyle : ( Int, Int ) -> Tile -> Html.Attribute
tileStyle ( winX, winY ) tile =
    style
        [ ( "top", topCssForTile winY tile )
        , ( "left", leftCssForTile winX tile )
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


renderTileEdgesAndType : Tile -> List Html
renderTileEdgesAndType tile =
    [ renderTileEdge tile.top "top"
    , renderTileEdge tile.left "left"
    , renderTileEdge tile.right "right"
    , renderTileEdge tile.bottom "bottom"
    , renderTileType tile.tileType
    ]


potentialTileStyle : ( Int, Int ) -> Coord -> Html.Attribute
potentialTileStyle ( winX, winY ) ( x, y ) =
    style
        [ ( "top", calculateCssPositionForCoord winY y )
        , ( "left", calculateCssPositionForCoord winX x )
        ]


renderPlacementTile : Signal.Address Action -> ( Int, Int ) -> Coord -> Html
renderPlacementTile address dimensions coords =
    div
        [ class "potential-tile"
        , potentialTileStyle dimensions coords
        , onClick address (PlaceTile coords)
        ]
        []


renderBoard : Signal.Address Action -> ( Int, Int ) -> Model -> Html
renderBoard address dimensions model =
    let
        actualTiles = List.map (renderTile dimensions) model.board

        placementTiles =
            potentialTileCoords model.board
                |> List.map (renderPlacementTile address dimensions)
    in
        div [] (List.append actualTiles placementTiles)


renderNextTile : Tile -> Html
renderNextTile tile =
    div
        [ class "next-tile tile" ]
        (renderTileEdgesAndType tile)


view : Signal.Address Action -> ( Int, Int ) -> Model -> Html
view address dimensions model =
    div
        []
        [ renderNextTile model.nextTile
        , renderBoard address dimensions model
        ]


model : Signal Model
model =
    Signal.foldp update initialModel actions.signal


main : Signal Html
main =
    Signal.map2 (view actions.address) Window.dimensions model



-- Signal.map (view actions.address) model
