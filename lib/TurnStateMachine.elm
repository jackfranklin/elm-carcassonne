module TurnStateMachine (..) where

import Statey exposing (makeState, StateMachine, StateRecord, State)
import Player exposing (Player)


type alias Turn =
    StateRecord { player : Player }


placeTile =
    makeState "placeTile"


placePerson =
    makeState "placePerson"


pickUpPeople =
    makeState "pickUpPeople"


endTurn =
    makeState "endTurn"


stateMachine : StateMachine Turn
stateMachine =
    { states =
        [ placeTile
        , placePerson
        , pickUpPeople
        , endTurn
        ]
    , transitions =
        [ ( placeTile, placePerson )
        , ( placeTile, pickUpPeople )
        , ( placePerson, pickUpPeople )
        , ( pickUpPeople, endTurn )
        , ( placeTile, endTurn )
        , ( placePerson, endTurn )
        ]
    , guards = []
    }


makeTurn : Player -> Turn
makeTurn player =
    { state = placeTile, player = player }


transitionSilently : State -> StateRecord Turn -> StateRecord Turn
transitionSilently newState record =
    case Statey.transition stateMachine newState record of
        Ok newRecord ->
            newRecord

        Err _ ->
            record
