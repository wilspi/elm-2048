module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events as BE
import Debug
import Html exposing (Html, div, table, td, text, tr)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Random



-- MODEL


type Cell
    = Tile Int
    | EmptyCell


type alias Grid =
    Array (Array Cell)


type alias Model =
    { size : Int
    , grid : Grid
    }



-- INIT


gridSize : Int
gridSize =
    4


init : ( Model, Cmd Msg )
init =
    ( { size = gridSize
      , grid =
            EmptyCell
                |> Array.repeat gridSize
                |> Array.repeat gridSize
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "outer-2048" ]
        [ div [] [ text "2048", text "Use 'w', 'a', 's', 'd' to play" ]
        , div [ class "grid-2048" ]
            [ table []
                (model.grid
                    |> Array.map
                        (\l ->
                            tr []
                                (l
                                    |> Array.map
                                        (\c ->
                                            case c of
                                                Tile v ->
                                                    td [] [ text (String.fromInt v) ]

                                                EmptyCell ->
                                                    td [] [ text "0" ]
                                        )
                                    |> Array.toList
                                )
                        )
                    |> Array.toList
                )
            ]
        ]



-- UPDATE


type Msg
    = Left
    | Right
    | Up
    | Down
    | Reset
    | Add Int



-- Returns `Add` msg with a random number between 1 to number of available cells


randomPickCell : Model -> Cmd Msg
randomPickCell model =
    Random.generate Add
        (Random.int 1
            ((model.grid
                |> getAvailableCells
                |> List.length
             )
                - 1
            )
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        dummy =
            Debug.log "message: " msg
    in
    case msg of
        Left ->
            ( { model
                | grid =
                    model.grid
                        |> Array.map
                            (\x ->
                                x
                                    |> Array.toList
                                    |> mergeAndFillRow
                                    |> Array.fromList
                            )
              }
            , randomPickCell model
            )

        Right ->
            ( { model
                | grid =
                    model.grid
                        |> Array.map
                            (\x ->
                                x
                                    |> Array.toList
                                    |> List.reverse
                                    |> mergeAndFillRow
                                    |> List.reverse
                                    |> Array.fromList
                            )
              }
            , randomPickCell model
            )

        Up ->
            ( { model
                | grid =
                    model.grid
                        |> transposeMap
                            (EmptyCell
                                |> Array.repeat 0
                                |> Array.repeat 0
                            )
                        |> Array.map
                            (\x ->
                                x
                                    |> Array.toList
                                    |> mergeAndFillRow
                                    |> Array.fromList
                            )
                        |> transposeMap
                            (EmptyCell
                                |> Array.repeat 0
                                |> Array.repeat 0
                            )
              }
            , randomPickCell model
            )

        Down ->
            ( { model
                | grid =
                    model.grid
                        |> transposeMap
                            (EmptyCell
                                |> Array.repeat 0
                                |> Array.repeat 0
                            )
                        |> Array.map
                            (\x ->
                                x
                                    |> Array.toList
                                    |> List.reverse
                                    |> mergeAndFillRow
                                    |> List.reverse
                                    |> Array.fromList
                            )
                        |> transposeMap
                            (EmptyCell
                                |> Array.repeat 0
                                |> Array.repeat 0
                            )
              }
            , randomPickCell model
            )

        Reset ->
            init

        -- Adds a tile of `2` in the i-th available cell
        Add i ->
            ( addTile
                (case
                    model.grid
                        |> getAvailableCells
                        |> Array.fromList
                        |> Array.get i
                 of
                    Just t ->
                        t

                    -- Array.set handles out of range
                    Nothing ->
                        ( -1, -1 )
                )
                2
                model
            , Cmd.none
            )


transposeMap : Grid -> Grid -> Grid
transposeMap grid2 grid1 =
    case Array.get 0 grid1 of
        Just e ->
            transposeMap (updateForTranspose 0 e grid2) (Array.slice 1 (Array.length grid1) grid1)

        Nothing ->
            grid2


updateForTranspose : Int -> Array Cell -> Grid -> Grid
updateForTranspose idx list grid2 =
    case Array.get 0 list of
        Just e ->
            updateForTranspose (idx + 1)
                (Array.slice 1 (Array.length list) list)
                (case Array.get idx grid2 of
                    Just l ->
                        Array.set idx (Array.push e l) grid2

                    Nothing ->
                        Array.push (Array.fromList [ e ]) grid2
                )

        Nothing ->
            grid2


mergeAndFillRow : List Cell -> List Cell
mergeAndFillRow list =
    let
        updatedRow =
            mergeRow list
    in
    List.concat
        (updatedRow
            :: [ EmptyCell
                    |> List.repeat
                        (gridSize
                            - List.length updatedRow
                        )
               ]
        )


mergeRow : List Cell -> List Cell
mergeRow list =
    case list of
        [] ->
            []

        x1 :: xs ->
            if x1 == EmptyCell then
                mergeRow xs

            else
                case xs of
                    [] ->
                        [ x1 ]

                    x2 :: xs2 ->
                        if x1 == x2 then
                            mergeCell ( x1, x2 ) :: mergeRow xs2

                        else
                            x1 :: mergeRow (x2 :: xs2)



-- HELPERS


type alias Position =
    ( Int, Int )


getAvailableCells : Grid -> List Position
getAvailableCells grid =
    List.concat
        (grid
            |> Array.indexedMap
                (\i x ->
                    x
                        |> Array.indexedMap
                            (\j y ->
                                case y of
                                    Tile t ->
                                        { x = i, y = j, v = t }

                                    EmptyCell ->
                                        { x = i, y = j, v = 0 }
                            )
                        |> Array.toList
                )
            |> Array.toList
        )
        |> List.filter (\{ x, y, v } -> v == 0)
        |> List.map (\t -> ( t.x, t.y ))


addTile : Position -> Int -> Model -> Model
addTile ( x, y ) value model =
    { model
        | grid =
            model.grid
                |> Array.set x
                    (Array.set y
                        (case value > 0 of
                            True ->
                                Tile value

                            _ ->
                                EmptyCell
                        )
                        (case Array.get x model.grid of
                            Just r ->
                                r

                            Nothing ->
                                Array.repeat model.size EmptyCell
                        )
                    )
    }


swap : ( Cell, Cell ) -> ( Cell, Cell )
swap ( cell1, cell2 ) =
    ( cell2, cell1 )


mergeCell : ( Cell, Cell ) -> Cell
mergeCell ( cell1, cell2 ) =
    case ( cell1, cell2 ) of
        ( Tile val1, Tile val2 ) ->
            Tile (val1 + val2)

        ( Tile val1, EmptyCell ) ->
            Tile val1

        _ ->
            mergeCell (swap ( cell1, cell2 ))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ BE.onKeyPress (Decode.map toDirection keyDecoder)
        ]


toDirection : String -> Msg
toDirection string =
    let
        dummy =
            Debug.log "input key: " string
    in
    case string of
        "a" ->
            Left

        "d" ->
            Right

        "w" ->
            Up

        "s" ->
            Down

        _ ->
            Reset


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \() -> init
        , subscriptions = subscriptions
        , update = update
        }
