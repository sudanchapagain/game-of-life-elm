module Grid exposing (Grid, initGrid, toggleCell, viewGrid)

import Html exposing (Html, div, table, td, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type alias Grid =
    List (List Bool)


initGrid : Int -> Int -> Grid
initGrid width height =
    List.repeat height (List.repeat width False)


toggleCell : Int -> Int -> Grid -> Grid
toggleCell x y grid =
    List.indexedMap
        (\rowIdx row ->
            if rowIdx == y then
                toggleRow x row

            else
                row
        )
        grid


toggleRow : Int -> List Bool -> List Bool
toggleRow x row =
    List.indexedMap
        (\colIdx cell ->
            if colIdx == x then
                not cell

            else
                cell
        )
        row


viewGrid : Grid -> (Int -> Int -> msg) -> Html msg
viewGrid grid toggleMsg =
    table []
        (List.indexedMap (\y row -> viewRow y row toggleMsg) grid)


viewRow : Int -> List Bool -> (Int -> Int -> msg) -> Html msg
viewRow y row toggleMsg =
    tr [] (List.indexedMap (\x cell -> viewCell x y cell toggleMsg) row)


viewCell : Int -> Int -> Bool -> (Int -> Int -> msg) -> Html msg
viewCell x y alive toggleMsg =
    td
        [ onClick (toggleMsg x y)
        , style "width" "20px"
        , style "height" "20px"
        , style "border" "1px solid black"
        , style "background-color"
            (if alive then
                "black"

             else
                "white"
            )
        ]
        []
