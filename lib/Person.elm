module Person (..) where


type PersonType
    = Farm
    | Road
    | Castle
    | Monastery
    | NotPlaced


type alias Person =
    { x : Maybe Int
    , y : Maybe Int
    , personType : PersonType
    }


makePerson : Person
makePerson =
    { x = Nothing
    , y = Nothing
    , personType = NotPlaced
    }


initialPeople : List Person
initialPeople =
    List.map (\_ -> makePerson) [0..7]
