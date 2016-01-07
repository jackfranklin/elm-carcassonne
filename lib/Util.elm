module Util (unsafeMaybe, zip) where


unsafeMaybe : Maybe a -> a
unsafeMaybe mayb =
    case mayb of
        Nothing ->
            Debug.crash "Unsafe maybe went wrong"

        Just x ->
            x


zip =
    List.map2 (,)
