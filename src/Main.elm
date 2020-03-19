module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events as BE
import Debug
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Random



-- MODEL


gridSize : Int
gridSize =
    4


type Msg
    = Left
    | Right
    | Up
    | Down
    | Reset
    | Add Int


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


init : ( Model, Cmd Msg )
init =
    ( { size = gridSize, cells = Array.repeat gridSize (Array.repeat gridSize EmptyCell) }, Cmd.none )



-- VIEWS


viewRow : Array Cell -> Html Msg
viewRow list =
    tr [] (Array.toList (Array.map viewCell list))


viewCell : Cell -> Html Msg
viewCell cell =
    case cell of
        Tile value ->
            td [] [ text (String.fromInt value) ]

        EmptyCell ->
            td [] [ text "0" ]


view : Model -> Html Msg
view model =
    div [ class "outer-2048" ]
        [ div [] [ text "2048" ]
        , div [ class "grid-2048" ]
            [ table []
                (Array.toList
                    (Array.map viewRow model.cells)
                )
            ]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        dummy =
            Debug.log "dump tuple" msg
    in
    case msg of
        Reset ->
            init

        Left ->
            ( model, Random.generate Add (Random.int 1 (List.length (getAvailableCells model.cells) - 1)) )

        Right ->
            ( model, Cmd.none )

        Up ->
            ( model, Cmd.none )

        Down ->
            ( model, Cmd.none )

        Add i ->
            ( addCell (getPosition i (getAvailableCells model.cells)) model, Cmd.none )


type alias Position =
    { x : Int
    , y : Int
    , v : Int
    }


getAvailableCells : Grid -> List Position
getAvailableCells grid =
    let
        positions =
            List.concat
                (Array.toList
                    (Array.indexedMap
                        (\i x ->
                            Array.toList
                                (Array.indexedMap
                                    (\j y ->
                                        case y of
                                            Tile t ->
                                                { x = i, y = j, v = t }

                                            EmptyCell ->
                                                { x = i, y = j, v = 0 }
                                    )
                                    x
                                )
                        )
                        grid
                    )
                )
    in
    List.filter (\{ x, y, v } -> v == 0) positions


getPosition : Int -> List Position -> ( Int, Int )
getPosition idx positions =
    let
        position =
            case Array.get idx (Array.fromList positions) of
                Just t ->
                    t

                Nothing ->
                    { x = -1, y = -1, v = -1 }
    in
    ( position.x, position.y )



--
--getRandomAvailableCell : Grid -> Position
--getRandomAvailableCell grid =
--    let
--        availablePositions =
--            getAvailableCells grid
--
--        dummy =
--            Debug.log "tiles" availablePositions
--    in
--    Array.get (Random.int 0 (List.length availablePositions - 1)) (Array.fromList availablePositions)
--roll : Random.Generator Int
--roll =
--  Random.int 0


addCell : ( Int, Int ) -> Model -> Model
addCell ( x, y ) model =
    let
        row =
            case Array.get x model.cells of
                Just r ->
                    r

                Nothing ->
                    Array.repeat model.size EmptyCell
    in
    { size = model.size
    , cells = Array.set x (Array.set y (Tile 2) row) model.cells
    }



--addRandomCell : Model -> Model
--addRandomCell model =
--    let
--        randomPosition =
--            getRandomAvailableCell model.cells
--
--        row =
--            case Array.get randomPosition.x model.cells of
--                Just x ->
--                    x
--
--                Nothing ->
--                    Array.repeat model.size EmptyCell
--    in
--    { size = model.size
--    , cells = Array.set randomPosition.x (Array.set randomPosition.y (Tile 2) row) model.cells
--    }
--List.concat (Array.toList (Array.indexedMap (\i, x -> List.filter (\g -> g == EmptyCell) (Array.toList x)) grid))
--List.concat (Array.toList (Array.indexedMap (\i, x -> Array.foldl (\g -> if g == EmptyCell then ()) [] x) grid))
--
--m : Model -> Model
--m model =
--    let
--        g =
--            getAvailableCells model.cells
--    in
--    model


swap : Cell -> Cell -> ( Cell, Cell )
swap cell1 cell2 =
    ( cell2, cell1 )


merge : Cell -> Cell -> ( Cell, Cell )
merge cell1 cell2 =
    case ( cell1, cell2 ) of
        ( Tile val1, Tile val2 ) ->
            ( Tile (val1 + val2), EmptyCell )

        ( Tile val1, EmptyCell ) ->
            ( Tile val1, EmptyCell )

        _ ->
            swap cell1 cell2



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
            Debug.log "key tuple" string
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
