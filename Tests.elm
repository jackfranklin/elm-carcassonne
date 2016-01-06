module Tests (..) where

import ElmTest exposing (..)
import TileTests
import BoardTests


all : Test
all =
    suite
        "Carcassonne Tests Suite"
        [ TileTests.tests
        , BoardTests.tests
        ]
