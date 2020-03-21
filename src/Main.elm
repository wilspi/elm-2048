module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events as BE
import Debug
import Html exposing (Html, a, div, h1, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
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
    , EmptyCell
        |> Array.repeat gridSize
        |> Array.repeat gridSize
        |> randomPickCell
    )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "heading" ]
            [ h1 [ class "title" ]
                [ text "2048" ]
            , div
                [ class "scores-container" ]
                []
            ]
        , div [ class "above-grid" ]
            [ p [ class "game-intro" ] [ text "Join the numbers and get to the '2048' tile!" ]
            , a [ class "restart-button", onClick Reset ] [ text "New Game" ]
            ]
        , div [ class "grid" ]
            (model.grid
                |> Array.map
                    (\l ->
                        div [ class "row" ]
                            (l
                                |> Array.map
                                    (\c ->
                                        case c of
                                            Tile v ->
                                                div [ class ("cell" ++ " tile" ++ String.fromInt v) ] [ text (String.fromInt v) ]

                                            EmptyCell ->
                                                div [ class "cell emptycell" ] [ text "0" ]
                                    )
                                |> Array.toList
                            )
                    )
                |> Array.toList
            )
        , div [ class "below-grid" ] [ text "Use 'w', 'a', 's', 'd' to play" ]
        ]



-- UPDATE


type Msg
    = LeftMove
    | RightMove
    | UpMove
    | DownMove
    | AddTile Int
    | Reset
    | Invalid


randomPickCell : Grid -> Cmd Msg
randomPickCell grid =
    -- Returns `AddTile` msg with a random number between 1 to number of available cells
    Random.generate AddTile
        (Random.int 1
            ((grid
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
        LeftMove ->
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
            , model.grid |> randomPickCell
            )

        RightMove ->
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
            , model.grid |> randomPickCell
            )

        UpMove ->
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
            , model.grid |> randomPickCell
            )

        DownMove ->
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
            , model.grid |> randomPickCell
            )

        -- Adds a tile of `2` in the i-th available cell
        AddTile i ->
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

        Reset ->
            init

        Invalid ->
            ( model, Cmd.none )


transposeMap : Grid -> Grid -> Grid
transposeMap grid2 grid1 =
    case Array.get 0 grid1 of
        Just e ->
            transposeMap (grid2 |> transposeForIdx 0 e) (grid1 |> Array.slice 1 (grid1 |> Array.length))

        Nothing ->
            grid2


transposeForIdx : Int -> Array Cell -> Grid -> Grid
transposeForIdx idx list grid2 =
    case Array.get 0 list of
        Just e ->
            transposeForIdx (idx + 1)
                (list |> Array.slice 1 (list |> Array.length))
                (case grid2 |> Array.get idx of
                    Just l ->
                        grid2 |> Array.set idx (l |> Array.push e)

                    Nothing ->
                        grid2 |> Array.push (Array.fromList [ e ])
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
                        ((list |> List.length) - (updatedRow |> List.length))
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
                        (case model.grid |> Array.get x of
                            Just r ->
                                r

                            Nothing ->
                                EmptyCell |> Array.repeat model.size
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
            LeftMove

        "d" ->
            RightMove

        "w" ->
            UpMove

        "s" ->
            DownMove

        "r" ->
            Reset

        _ ->
            Invalid


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
