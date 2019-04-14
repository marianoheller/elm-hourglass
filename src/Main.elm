module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Canvas exposing (..)
import Color
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Particle as P
import Platform exposing (..)
import Task
import Time


type alias ViewportInfo =
    { width : Float
    , height : Float
    }


toViewPortInfo : Viewport -> ViewportInfo
toViewPortInfo v =
    { width = v.viewport.width, height = v.viewport.height }


type alias MetaInfo =
    { frameCount : Float
    , frameCountAcc : Float
    , fps : Float
    , viewport : ViewportInfo
    }


type alias Model =
    { metaInfo : MetaInfo
    , particles : List P.Particle
    }


type Msg
    = Frame Float
    | Tick Time.Posix
    | ViewportInfoUpdate ViewportInfo


setMetaInfo : MetaInfo -> Model -> Model
setMetaInfo f m =
    { m | metaInfo = f }


setFrameCount : Float -> MetaInfo -> MetaInfo
setFrameCount c f =
    { f | frameCount = c }

increaseFrameCount : MetaInfo -> MetaInfo
increaseFrameCount f =
    { f | frameCount = f.frameCount + 1, frameCountAcc = f.frameCount + 1 }


setViewportInfo : ViewportInfo -> MetaInfo -> MetaInfo
setViewportInfo vInfo m =
    { m | viewport = vInfo }


calcFps : MetaInfo -> MetaInfo
calcFps f =
    { f | frameCount = 0, fps = f.frameCount }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { metaInfo =
            { frameCount = 0
            , frameCountAcc = 0
            , fps = 50
            , viewport =
                { width = 0
                , height = 0
                }
            }
      , particles = []
      }
    , getViewport |> Task.perform (\v -> ViewportInfoUpdate (toViewPortInfo v))
    )


update msg model =
    case msg of
        ViewportInfoUpdate viewport ->
            let
                newModel =
                    model
                        |> setMetaInfo (setViewportInfo viewport model.metaInfo)
            in
            ( newModel, Cmd.none )

        Frame _ ->
            let
                newModel =
                    model
                        |> setMetaInfo (increaseFrameCount model.metaInfo)
            in
            ( newModel, Cmd.none )

        Tick _ ->
            let
                newModel =
                    model
                        |> setMetaInfo (calcFps model.metaInfo)
            in
            ( newModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Frame
        , onResize (\w h -> ViewportInfoUpdate { width = toFloat w, height = toFloat h })
        , Time.every 1000 Tick
        ]


view : Model -> Html Msg
view { metaInfo } =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( round metaInfo.viewport.width, round metaInfo.viewport.height )
            [ style "border" "5px solid rgba(0,0,0,0.1)" ]
            [ clearScreen metaInfo.viewport
            , render metaInfo.frameCountAcc metaInfo.viewport
            ]
        , viewFPS metaInfo.fps
        ]

viewFPS fps =
    div
        [ style "position" "absolute"
        , style "top" "1rem"
        , style "right" "1rem"
        , style "z-index" "10"
        ]
        [ text <| String.fromFloat fps ]


clearScreen : ViewportInfo -> Renderable
clearScreen v =
    shapes [ fill Color.black ] [ rect ( 0, 0 ) v.width v.height ]


render : Float -> ViewportInfo -> Renderable
render count v =
    let
        size =
            v.height / 3
        
        centerX =
            v.width / 2
        
        centerY =
            v.height / 2

        x =
            centerX - (size / 2)

        y =
            centerY - (size / 2)
            
        rotation =
            degrees (count * 3)

        hue =
            toFloat (count / 4 |> floor |> modBy 100) / 100
    in
    shapes
        [ transform
            [ translate centerX centerY
            , rotate rotation
            ]
        , fill (Color.hsl hue 0.3 0.7)
        ]
        [ rect ( x, y ) size size ]
