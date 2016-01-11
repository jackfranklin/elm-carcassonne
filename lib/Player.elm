module Player (..) where

import Person exposing (Person, initialPeople)
import Colour exposing (Colour)


type alias Player =
    { score : Int
    , people : List Person
    , colour : Colour
    }


makePlayer : Colour -> Player
makePlayer colour =
    { score = 0
    , people = initialPeople
    , colour = colour
    }
