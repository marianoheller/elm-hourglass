module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Color
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Platform exposing (..)
import Time
import Particle as P


type alias Model = {
    frameCount : Float,
    fps : Float,
    particles: List P.Particle
  }


type Msg
    = Frame Float | Tick Time.Posix


main : Program () Model Msg
main = Browser.element {
      init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
  }

init _ = (
    { frameCount = 0, fps = 0, particles = [] },
    Cmd.none
  )

update msg model = 
  case msg of
    Frame _ ->
        ( { model | frameCount = model.frameCount + 1 }, Cmd.none )
    Tick _ -> ({ model | frameCount = 0, fps = model.frameCount }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
      onAnimationFrameDelta Frame,
      Time.every 1000 Tick
    ]

width : number
width = 400
height : number
height = 400


centerX = width / 2
centerY = height / 2

view : Model -> Html Msg
view { frameCount, fps } =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( width, height )
            [ style "border" "10px solid rgba(0,0,0,0.1)" ]
            [ clearScreen
            , render frameCount
            ]
        , text <| String.fromFloat fps
        ]


clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


render frameCount =
    let
        size =
            width / 3
        x =
            -(size / 2)
        y =
            -(size / 2)
    in
    shapes
        [ transform
            [ translate centerX centerY
            , rotate (degrees (frameCount * 3))
            ]
        , fill (Color.hsl (degrees (frameCount / 4)) 0.3 0.7)
        ]
        [ rect ( x, y ) size size ]