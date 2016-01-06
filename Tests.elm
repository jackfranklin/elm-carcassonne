module Tests (..) where

import ElmTest exposing (..)
import String
import TileTests


all : Test
all =
    suite
        "Carcassonne Tests Suite"
        [ TileTests.tests
        ]
