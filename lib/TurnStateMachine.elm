module TurnStateMachine (..) where

import Statey exposing (makeState, StateMachine, StateRecord, State)
import Player exposing (Player)


type alias Turn =
    StateRecord { player : Player }


placeTileState =
    makeState "placeTile"


placePersonState =
    makeState "placePerson"


pickUpPeopleState =
    makeState "pickUpPeople"


endTurnState =
    makeState "endTurn"


stateMachine : StateMachine Turn
stateMachine =
    { states =
        [ placeTileState
        , placePersonState
        , pickUpPeopleState
        , endTurnState
        ]
    , transitions =
        [ ( placeTileState, placePersonState )
        , ( placeTileState, pickUpPeopleState )
        , ( placePersonState, pickUpPeopleState )
        , ( pickUpPeopleState, endTurnState )
        , ( placeTileState, endTurnState )
        , ( placePersonState, endTurnState )
        ]
    , guards = []
    }


makeTurn : Player -> Turn
makeTurn player =
    { state = placeTileState, player = player }


transitionSilently : State -> StateRecord Turn -> StateRecord Turn
transitionSilently newState record =
    case Statey.transition stateMachine newState record of
        Ok newRecord ->
            newRecord

        Err _ ->
            record
