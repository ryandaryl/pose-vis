module Utils exposing (..)

{-| Take elements in order as long as the predicate evaluates to `True`
-}


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate =
    let
        takeWhileMemo memo list =
            case list of
                [] ->
                    List.reverse memo

                x :: xs ->
                    if (predicate x) then
                        takeWhileMemo (x :: memo) xs
                    else
                        List.reverse memo
    in
        takeWhileMemo []
