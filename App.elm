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
import Player exposing (Player, makePlayer)
import Colour exposing (Colour(..))
import Statey exposing (..)
import TurnStateMachine exposing (Turn, placePersonState, transitionSilently, makeTurn, placeTileState)
import Person exposing (PersonType(..))
import Debug


type alias Model =
    { availableTiles : List Tile
    , nextTile : Maybe Tile
    , board : Board
    , players : List Player
    , turn : Turn
    }


firstPlayer : Player
firstPlayer =
    makePlayer Red


initialPlayers : List Player
initialPlayers =
    [ firstPlayer
    , makePlayer Blue
    ]


initialModel : Model
initialModel =
    { availableTiles = (List.tail StartingTiles.tiles) ? []
    , nextTile = List.head StartingTiles.tiles
    , board = [ StartingTiles.starterTile ]
    , players = initialPlayers
    , turn = makeTurn firstPlayer
    }


type Action
    = NoOp
    | RotateTile
    | PlaceTile Coord
    | PlacePerson PersonType


actions : Signal.Mailbox Action
actions =
    Signal.mailbox NoOp


update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        RotateTile ->
            -- TODO
            case model.nextTile of
                Just tile ->
                    { model | nextTile = Just (rotateTile tile) }

                Nothing ->
                    model

        PlaceTile coords ->
            -- ensure we have a tile to place
            -- we should always because a user should not be able to click and
            -- end up here unless there is
            case model.nextTile of
                Nothing ->
                    -- TODO: should update some state here to trigger end of game
                    model

                Just tileToPlace ->
                    case List.head model.availableTiles of
                        Just nextTile ->
                            { model
                                | board = placeTile model.board tileToPlace coords
                                , nextTile = Just nextTile
                                , availableTiles =
                                    (List.tail model.availableTiles) ? []
                                    -- TODO: should deal with this failing?
                                , turn = (transitionSilently placePersonState model.turn)
                            }

                        Nothing ->
                            -- TODO: should update some state here to trigger end of game
                            { model
                                | board = placeTile model.board tileToPlace coords
                                , nextTile = Nothing
                                , availableTiles = []
                            }

        PlacePerson personType ->
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


calculateLeftCssPositionForCoord : Int -> Int -> String
calculateLeftCssPositionForCoord winX tileX =
    (toString ((winX // 2) + (tileX * 60))) ++ "px"


calculateTopCssPositionForCoord : Int -> Int -> String
calculateTopCssPositionForCoord winY tileY =
    (toString ((winY // 2) - (tileY * 60))) ++ "px"


topCssForTile : Int -> Tile -> String
topCssForTile winY tile =
    calculateTopCssPositionForCoord winY (tile.y ? 0)


leftCssForTile : Int -> Tile -> String
leftCssForTile winX tile =
    calculateLeftCssPositionForCoord winX (tile.x ? 0)


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
        [ ( "top", calculateTopCssPositionForCoord winY y )
        , ( "left", calculateLeftCssPositionForCoord winX x )
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
            case model.nextTile of
                Just nextTile ->
                    potentialTileCoords model.board
                        |> List.filter (canPlaceTileAt model.board nextTile)
                        |> List.map (renderPlacementTile address dimensions)

                Nothing ->
                    []
    in
        div [] (List.append actualTiles placementTiles)


renderNextTile : Turn -> Maybe Tile -> Html
renderNextTile turn tile =
    if getState turn == placeTileState then
        case tile of
            Just t ->
                div
                    [ class "next-tile tile" ]
                    (renderTileEdgesAndType t)

            Nothing ->
                div
                    [ class "next-tile tile" ]
                    [ text "No tiles left" ]
    else
        div [] []


renderCurrentTurn : Turn -> Html
renderCurrentTurn turn =
    div
        [ class "current-turn" ]
        [ text (toString turn.state) ]


renderRotateButton : Signal.Address Action -> Turn -> Html
renderRotateButton address turn =
    if getState turn == placeTileState then
        div
            [ class "rotate-button" ]
            [ button [ onClick address RotateTile ] [ text "Rotate" ] ]
    else
        div [] []


renderPlacePersonButton : Signal.Address Action -> Turn -> Html
renderPlacePersonButton address turn =
    if getState turn == placePersonState then
        case Player.freePeople turn.player of
            [] ->
                div [] []

            players ->
                div
                    [ class "press-player-button" ]
                    [ -- TODO: farmers are way more complex because they have to go on a segment
                      -- of a tile
                      button [ onClick address (PlacePerson Farm) ] [ text "Add Farmer" ]
                    ]
    else
        div [] []


view : Signal.Address Action -> ( Int, Int ) -> Model -> Html
view address dimensions model =
    div
        []
        [ renderNextTile model.turn model.nextTile
        , renderRotateButton address model.turn
        , renderPlacePersonButton address model.turn
        , renderCurrentTurn model.turn
        , renderBoard address dimensions model
        ]


model : Signal Model
model =
    Signal.foldp update initialModel actions.signal


main : Signal Html
main =
    Signal.map2 (view actions.address) Window.dimensions model
