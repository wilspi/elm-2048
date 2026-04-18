port module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events as BE
import Html exposing (Html, a, button, div, h1, p, span, text)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick)
import Html.Events.Extra.Touch as Touch
import Json.Decode as Decode
import Random



-- PORTS


port saveBestScore : Int -> Cmd msg


port onShake : (String -> msg) -> Sub msg



-- MODEL


type Cell
    = Tile Int
    | EmptyCell


type alias Grid =
    Array (Array Cell)


type alias Model =
    { size : Int
    , grid : Grid
    , swipeCoordinate : ( Maybe Float, Maybe Float )
    , score : Int
    , bestScore : Int
    , shakeEnabled : Bool
    }



-- INIT


gridSize : Int
gridSize =
    4


init : Int -> ( Model, Cmd Msg )
init bestScore =
    ( { size = gridSize
      , grid =
            EmptyCell
                |> Array.repeat gridSize
                |> Array.repeat gridSize
      , swipeCoordinate = ( Nothing, Nothing )
      , score = 0
      , bestScore = bestScore
      , shakeEnabled = True
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
            [ h1 [ class "title" ] [ text "2048" ]
            , div [ class "scores-container" ]
                [ scoreBox "SCORE" model.score
                , scoreBox "BEST" model.bestScore
                ]
            ]
        , div [ class "above-grid" ]
            [ p [ class "game-intro" ] [ text "Join the numbers and get to the 2048 tile!" ]
            , div [ class "buttons" ]
                [ button
                    [ class "share-button"
                    , attribute "data-score" (String.fromInt model.score)
                    , attribute "data-best" (String.fromInt model.bestScore)
                    ]
                    [ text "Share" ]
                , a [ class "restart-button", onClick Reset ] [ text "New Game" ]
                ]
            , div [ class "shake-row" ]
                [ button
                    [ class
                        ("shake-toggle"
                            ++ (if model.shakeEnabled then
                                    " shake-toggle--on"

                                else
                                    " shake-toggle--off"
                               )
                        )
                    , onClick ToggleShake
                    ]
                    [ text
                        (if model.shakeEnabled then
                            "🫨 Shake Gestures ON"

                         else
                            "🫨 Shake Gestures OFF"
                        )
                    ]
                ]
            ]
        , div
            [ class "grid"
            , Touch.onStart (SwipeStart << touchCoordinates)
            , Touch.onEnd (SwipeEnd << touchCoordinates)
            ]
            ((model.grid
                |> Array.map
                    (\l ->
                        div [ class "row" ]
                            (l
                                |> Array.map
                                    (\c ->
                                        case c of
                                            Tile v ->
                                                div [ class ("cell tile" ++ String.fromInt v) ] [ text (String.fromInt v) ]

                                            EmptyCell ->
                                                div [ class "cell emptycell" ] [ text "" ]
                                    )
                                |> Array.toList
                            )
                    )
                |> Array.toList
             )
                ++ (if isGameOver model.grid then
                        [ div [ class "gameover" ] [ text "Game Over!" ] ]

                    else
                        []
                   )
            )
        , div [ class "below-grid" ] [ text "Use Arrow keys / WASD · Swipe · Shake to play" ]
        ]


scoreBox : String -> Int -> Html Msg
scoreBox label value =
    div [ class "score-box" ]
        [ span [ class "score-label" ] [ text label ]
        , span [ class "score-value" ] [ text (String.fromInt value) ]
        ]


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )



-- UPDATE


type Msg
    = LeftMove
    | RightMove
    | UpMove
    | DownMove
    | SwipeStart ( Float, Float )
    | SwipeEnd ( Float, Float )
    | AddTile Int
    | Reset
    | ToggleShake
    | Invalid


randomPickCell : Grid -> Cmd Msg
randomPickCell grid =
    Random.generate AddTile
        (Random.int 0
            ((grid
                |> getAvailableCells
                |> List.length
             )
                - 1
            )
        )


emptyGrid : Grid
emptyGrid =
    EmptyCell |> Array.repeat 0 |> Array.repeat 0


applyMoveToRows : (List Cell -> List Cell) -> Grid -> ( Grid, Int )
applyMoveToRows rowFn grid =
    let
        results =
            grid |> Array.map (\row -> row |> Array.toList |> mergeAndFillRow rowFn)

        newGrid =
            results |> Array.map (Tuple.first >> Array.fromList)

        gained =
            results |> Array.toList |> List.map Tuple.second |> List.sum
    in
    ( newGrid, gained )


commitMove : Grid -> Int -> Model -> ( Model, Cmd Msg )
commitMove newGrid gained model =
    if newGrid == model.grid then
        ( model, Cmd.none )

    else
        let
            newScore =
                model.score + gained

            newBest =
                max model.bestScore newScore
        in
        ( { model | grid = newGrid, score = newScore, bestScore = newBest }
        , Cmd.batch
            [ randomPickCell newGrid
            , if newBest > model.bestScore then
                saveBestScore newBest

              else
                Cmd.none
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LeftMove ->
            let
                ( newGrid, gained ) =
                    applyMoveToRows identity model.grid
            in
            commitMove newGrid gained model

        RightMove ->
            let
                ( newGrid, gained ) =
                    applyMoveToRows List.reverse model.grid
            in
            commitMove newGrid gained model

        UpMove ->
            let
                ( newGrid, gained ) =
                    model.grid
                        |> transposeMap emptyGrid
                        |> applyMoveToRows identity
                        |> Tuple.mapFirst (transposeMap emptyGrid)
            in
            commitMove newGrid gained model

        DownMove ->
            let
                ( newGrid, gained ) =
                    model.grid
                        |> transposeMap emptyGrid
                        |> applyMoveToRows List.reverse
                        |> Tuple.mapFirst (transposeMap emptyGrid)
            in
            commitMove newGrid gained model

        SwipeStart ( x, y ) ->
            ( { model | swipeCoordinate = ( Just x, Just y ) }, Cmd.none )

        SwipeEnd ( x, y ) ->
            model |> identifySwipeDirectionAndUpdate ( x, y )

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

                    Nothing ->
                        ( -1, -1 )
                )
                2
                model
            , Cmd.none
            )

        Reset ->
            let
                ( newModel, cmd ) =
                    init model.bestScore
            in
            ( { newModel | shakeEnabled = model.shakeEnabled }, cmd )

        ToggleShake ->
            ( { model | shakeEnabled = not model.shakeEnabled }, Cmd.none )

        Invalid ->
            ( model, Cmd.none )


identifySwipeDirectionAndUpdate : ( Float, Float ) -> Model -> ( Model, Cmd Msg )
identifySwipeDirectionAndUpdate ( x2, y2 ) model =
    let
        initialCoordinates =
            model.swipeCoordinate

        newModel =
            { model | swipeCoordinate = ( Nothing, Nothing ) }
    in
    case initialCoordinates of
        ( Just x1, Just y1 ) ->
            if (abs (x2 - x1) > abs (y2 - y1)) && (abs (x2 - x1) > 50) then
                if x2 > x1 then
                    update RightMove newModel

                else
                    update LeftMove newModel

            else if abs (y2 - y1) > 50 then
                if y2 > y1 then
                    update DownMove newModel

                else
                    update UpMove newModel

            else
                ( newModel, Cmd.none )

        _ ->
            ( newModel, Cmd.none )


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


mergeAndFillRow : (List Cell -> List Cell) -> List Cell -> ( List Cell, Int )
mergeAndFillRow orderFn list =
    let
        ( merged, gained ) =
            list |> orderFn |> mergeCompacted

        filled =
            merged ++ List.repeat (List.length list - List.length merged) EmptyCell
    in
    ( orderFn filled, gained )


mergeCompacted : List Cell -> ( List Cell, Int )
mergeCompacted list =
    case List.filter (\c -> c /= EmptyCell) list of
        [] ->
            ( [], 0 )

        [ x ] ->
            ( [ x ], 0 )

        x1 :: x2 :: rest ->
            if x1 == x2 then
                let
                    merged =
                        mergeCell ( x1, x2 )

                    points =
                        case merged of
                            Tile v ->
                                v

                            EmptyCell ->
                                0

                    ( restCells, restPoints ) =
                        mergeCompacted rest
                in
                ( merged :: restCells, points + restPoints )

            else
                let
                    ( restCells, restPoints ) =
                        mergeCompacted (x2 :: rest)
                in
                ( x1 :: restCells, restPoints )



-- HELPERS


type alias Position =
    ( Int, Int )


isGameOver : Grid -> Bool
isGameOver grid =
    let
        hasEmpty =
            grid
                |> Array.toList
                |> List.any (\row -> row |> Array.toList |> List.any (\c -> c == EmptyCell))

        canMergeInRow row =
            List.map2 Tuple.pair (Array.toList row) (row |> Array.toList |> List.drop 1)
                |> List.any (\( a, b ) -> a /= EmptyCell && a == b)

        canMergeHorizontally =
            grid |> Array.toList |> List.any canMergeInRow

        canMergeVertically =
            grid
                |> transposeMap emptyGrid
                |> Array.toList
                |> List.any canMergeInRow
    in
    not hasEmpty && not canMergeHorizontally && not canMergeVertically


getAvailableCells : Grid -> List Position
getAvailableCells grid =
    grid
        |> Array.indexedMap
            (\i row ->
                row
                    |> Array.indexedMap
                        (\j cell ->
                            case cell of
                                EmptyCell ->
                                    Just ( i, j )

                                Tile _ ->
                                    Nothing
                        )
                    |> Array.toList
            )
        |> Array.toList
        |> List.concat
        |> List.filterMap identity


addTile : Position -> Int -> Model -> Model
addTile ( x, y ) value model =
    { model
        | grid =
            model.grid
                |> Array.set x
                    (Array.set y
                        (if value > 0 then
                            Tile value

                         else
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


mergeCell : ( Cell, Cell ) -> Cell
mergeCell ( cell1, cell2 ) =
    case ( cell1, cell2 ) of
        ( Tile val1, Tile val2 ) ->
            Tile (val1 + val2)

        ( Tile val1, EmptyCell ) ->
            Tile val1

        ( EmptyCell, Tile val2 ) ->
            Tile val2

        ( EmptyCell, EmptyCell ) ->
            EmptyCell



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ BE.onKeyDown (Decode.map toDirection keyDecoder)
        , if model.shakeEnabled then
            onShake toDirection

          else
            Sub.none
        ]


toDirection : String -> Msg
toDirection string =
    case string of
        "a" ->
            LeftMove

        "ArrowLeft" ->
            LeftMove

        "shake-left" ->
            LeftMove

        "d" ->
            RightMove

        "ArrowRight" ->
            RightMove

        "shake-right" ->
            RightMove

        "w" ->
            UpMove

        "ArrowUp" ->
            UpMove

        "shake-up" ->
            UpMove

        "s" ->
            DownMove

        "ArrowDown" ->
            DownMove

        "shake-down" ->
            DownMove

        "r" ->
            Reset

        _ ->
            Invalid


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string



-- MAIN


main : Program Int Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , subscriptions = subscriptions
        , update = update
        }
