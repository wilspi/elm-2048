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
    , cells : Grid
    }



-- INIT


gridSize : Int
gridSize =
    4


init : ( Model, Cmd Msg )
init =
    ( { size = gridSize, cells = EmptyCell |> Array.repeat gridSize |> Array.repeat gridSize }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "outer-2048" ]
        [ div [] [ text "2048" ]
        , div [ class "grid-2048" ]
            [ table []
                (model.cells
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        dummy =
            Debug.log "message: " msg
    in
    case msg of
        Left ->
            ( { model | cells = Array.map (\x -> Array.fromList (mergeRows (Array.toList x))) model.cells }
              -- Sends `Add` msg with a random number between 1 to number of available cells
            , Random.generate Add (Random.int 1 (List.length (model.cells |> getAvailableCells) - 1))
            )

        Right ->
            ( model, Cmd.none )

        Up ->
            ( model, Cmd.none )

        Down ->
            ( model, Cmd.none )

        Reset ->
            init

        -- Adds a tile of `2` in the i-th available cell
        Add i ->
            ( addTile
                (case model.cells |> getAvailableCells |> Array.fromList |> Array.get i of
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


mergeRows : List Cell -> List Cell
mergeRows list =
    let
        updatedRow =
            mergeRow list
    in
    List.concat (updatedRow :: [ List.repeat (gridSize - List.length updatedRow) EmptyCell ])


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
        | cells =
            Array.set x
                (Array.set y
                    (Tile value)
                    (case Array.get x model.cells of
                        Just r ->
                            r

                        Nothing ->
                            Array.repeat model.size EmptyCell
                    )
                )
                model.cells
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
