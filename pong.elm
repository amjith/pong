module Pong exposing (..)

import Element exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Keyboard exposing (KeyCode)
import AnimationFrame
import Time exposing (Time)
import Html exposing (Html, text)
import Html.App as Html
import Window
import Focus exposing (..)
import Debug exposing (..)


-- Model


type alias Size =
    { width : Int, height : Int }


type alias Position =
    { x : Int, y : Int }


type alias Model =
    { gameSize : Size
    , paddle : { size : Size, position : Position }
    }


initialModel : Model
initialModel =
    { gameSize = Size 0 0
    , paddle = { size = Size 50 200, position = Position 0 0 }
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- Update


type Msg
    = NoOp
    | SizeChange Size
    | Up
    | Down


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        NoOp ->
            model

        SizeChange size ->
            { model | gameSize = size }

        Up ->
            model
                |> update (paddle => position => y) (\y -> y + 10)

        Down ->
            --Focus.update (.paddle .position .y) (\y -> y - 1) model
            update (paddle => position => y) (\y -> y - 10) model


x =
    Focus.create .x (\f position -> { position | x = f position.x })


y =
    Focus.create .y (\f position -> { position | y = f position.y })


position =
    Focus.create .position (\f paddle -> { paddle | position = f paddle.position })


paddle =
    Focus.create .paddle (\f r -> { r | paddle = f r.paddle })



-- View


view : Model -> Html Msg
view model =
    let
        ( w, h ) =
            ( model.gameSize.width, model.gameSize.height )
    in
        collage w
            h
            [ drawGame (toFloat w) (toFloat h)
            , drawPaddle model.paddle
            , toForm (show model)
            ]
            |> toHtml


drawGame : Float -> Float -> Form
drawGame w h =
    rect w h
        |> filled black



--drawPaddle : paddle -> Form


drawPaddle paddle =
    rect (toFloat paddle.size.width) (toFloat paddle.size.height)
        |> filled gray
        |> move ( toFloat paddle.position.x, toFloat paddle.position.y )



-- Subscriptions


keyPress : Sub Msg
keyPress =
    let
        toMsg n =
            case n of
                38 ->
                    Up

                40 ->
                    Down

                _ ->
                    NoOp
    in
        Keyboard.downs toMsg


input : Model -> Sub Msg
input _ =
    Sub.batch [ Window.resizes SizeChange, keyPress ]


main =
    Html.program
        { init = init
        , view = view
        , update = \msg model -> ( updateModel msg model, Cmd.none )
        , subscriptions = input
        }
