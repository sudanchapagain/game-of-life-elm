module Game exposing (nextGeneration)

import Grid exposing (Grid)


countNeighbors : Grid -> Int -> Int -> Int
countNeighbors grid x y =
    let
        positions =
            [ ( -1, -1 )
            , ( -1, 0 )
            , ( -1, 1 )
            , ( 0, -1 )
            , ( 0, 1 )
            , ( 1, -1 )
            , ( 1, 0 )
            , ( 1, 1 )
            ]

        getCell ( dx, dy ) =
            case List.drop (y + dy) grid |> List.head of
                Just row ->
                    case List.drop (x + dx) row |> List.head of
                        Just alive ->
                            if alive then
                                1

                            else
                                0

                        Nothing ->
                            0

                Nothing ->
                    0
    in
    List.sum (List.map getCell positions)


applyRules : Bool -> Int -> Bool
applyRules alive neighbors =
    case ( alive, neighbors ) of
        ( True, 2 ) ->
            True

        ( True, 3 ) ->
            True

        ( False, 3 ) ->
            True

        _ ->
            False


nextGeneration : Grid -> Grid
nextGeneration grid =
    List.indexedMap (\y row -> List.indexedMap (\x cell -> applyRules cell (countNeighbors grid x y)) row) grid
