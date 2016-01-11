module TurnStateMachine (..) where

import Statey exposing (makeState, StateMachine, StateRecord)
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
