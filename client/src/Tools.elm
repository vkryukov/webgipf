module Tools exposing (..)

import Dict exposing (Dict)


dictSlice : Dict comparable v -> List comparable -> List (Maybe v)
dictSlice dict keys =
    List.map (\key -> Dict.get key dict) keys


anyNothing : List (Maybe a) -> Bool
anyNothing list =
    List.any (\item -> item == Nothing) list


anyKeyMissing : Dict comparable v -> List comparable -> Bool
anyKeyMissing dict keys =
    anyNothing (dictSlice dict keys)


largestPrefixWithoutNothing : List (Maybe a) -> List a
largestPrefixWithoutNothing list =
    case list of
        [] ->
            []

        x :: xs ->
            case x of
                Nothing ->
                    []

                Just xx ->
                    xx :: largestPrefixWithoutNothing xs


sublistsOfFour : List a -> List ( Int, List a )
sublistsOfFour list =
    List.map
        (\i ->
            ( i, List.take 4 (List.drop i list) )
        )
        (List.range 0 (List.length list - 4))


extendSublistWithJustItems : List (Maybe a) -> Int -> List a
extendSublistWithJustItems list start =
    let
        prefix =
            List.take start list
    in
    List.reverse (largestPrefixWithoutNothing (List.reverse prefix)) ++ largestPrefixWithoutNothing (List.drop start list)
