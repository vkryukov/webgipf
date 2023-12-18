module Tools exposing (..)

import Dict exposing (Dict)
import Http


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


maybeList : List (Maybe a) -> Maybe (List a)
maybeList list =
    List.foldr
        (\ma acc ->
            Maybe.andThen
                (\a ->
                    Maybe.map
                        (\aas -> a :: aas)
                        acc
                )
                ma
        )
        (Just [])
        list


maybeFoldr : (a -> b -> Maybe b) -> b -> List a -> Maybe b
maybeFoldr f acc list =
    List.foldr
        (\a accumulator ->
            Maybe.andThen
                (\b ->
                    f a b
                )
                accumulator
        )
        (Just acc)
        list


splitByPredicate : (a -> Bool) -> List a -> ( List a, List a )
splitByPredicate predicate list =
    let
        matching =
            List.filter predicate list

        nonMatching =
            List.filter (not << predicate) list
    in
    ( matching, nonMatching )


isSubsetOf : List a -> List a -> Bool
isSubsetOf subset list =
    List.all (\element -> List.member element list) subset


isSubsetOfAny : List a -> List (List a) -> Bool
isSubsetOfAny subset lists =
    List.any (\l -> isSubsetOf subset l) lists


removeElements : List a -> List a -> List a
removeElements toRemove list =
    List.filter (\element -> not (List.member element toRemove)) list


removeElementsFromOneOfSupersets : List (List a) -> List a -> List a
removeElementsFromOneOfSupersets supersets list =
    -- find the first superset that contains all elements of the list,
    -- and remove those elements from that superset, returning them.
    -- if no superset contains all elements of the list, return the empty list.
    let
        matchingSuperset =
            List.filter (\superset -> isSubsetOf list superset) supersets
    in
    case matchingSuperset of
        [] ->
            []

        x :: _ ->
            removeElements list x


count : List a -> (a -> Bool) -> Int
count list pred =
    List.length (List.filter pred list)


symmetricalDifference : List (List a) -> List a
symmetricalDifference lists =
    {-
       symmetricalDifference takes a list of lists and returns a list of all elements that are in exactly one of the lists.
    -}
    let
        allElements =
            List.concat lists

        counts =
            List.map
                (\element ->
                    ( element, count allElements (\e -> e == element) )
                )
                allElements

        elementsInExactlyOneList =
            List.map
                (\( element, c ) ->
                    if c == 1 then
                        Just element

                    else
                        Nothing
                )
                counts
    in
    -- remove all the Nothings, and return the list of Justs
    List.filterMap identity elementsInExactlyOneList


findSubsetContaining : List (List a) -> a -> Maybe (List a)
findSubsetContaining lists element =
    let
        matchingSubsets =
            List.filter (\subset -> List.member element subset) lists
    in
    case matchingSubsets of
        [] ->
            Nothing

        x :: _ ->
            Just x


boolToString : Bool -> String
boolToString bool =
    if bool then
        "True"

    else
        "False"


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus statusCode ->
            "Bad status: " ++ String.fromInt statusCode

        Http.BadBody message ->
            "Bad body: " ++ message
