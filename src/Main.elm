module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Color
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import List exposing (..)
import Math.Vector2 exposing (..)
import Particle as P
import Platform exposing (..)
import Random
import Task
import Time



{- GRAVITY -}


g : Vec2
g =
    vec2 0 -0.00001



{- PARTICLE MASS -}


mass : Float
mass =
    5



{- DRAG COEFF TOTAL -}
{- Drag = Cd * .5 * r * V^2 * A -}
{- Cd: drag coeff, r: air density -}
{- Cd * .5 * r -}


d : Float
d =
    0.01



{- BASE PARTICLES -}


pBase : List Float
pBase =
    map toFloat (range 1 10)


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
    , startTime : Int
    , viewport : ViewportInfo
    }


type alias Model =
    { metaInfo : MetaInfo
    , particles : List P.Particle
    }


type Msg
    = Frame Float
    | Tick1s Time.Posix
    | Tick20ms Time.Posix
    | ViewportInfoUpdate ViewportInfo
    | InitParticlePositions ViewportInfo
    | InitStartTime Time.Posix


setParticles : List P.Particle -> Model -> Model
setParticles ps m =
    { m | particles = ps }


setMetaInfo : MetaInfo -> Model -> Model
setMetaInfo f m =
    { m | metaInfo = f }


setFrameCount : Float -> MetaInfo -> MetaInfo
setFrameCount c f =
    { f | frameCount = c }


setStartTime : Int -> MetaInfo -> MetaInfo
setStartTime st f =
    { f | startTime = st }


increaseFrameCount : MetaInfo -> MetaInfo
increaseFrameCount f =
    { f | frameCount = f.frameCount + 1, frameCountAcc = f.frameCountAcc + 1 }


setViewportInfo : ViewportInfo -> MetaInfo -> MetaInfo
setViewportInfo vInfo m =
    { m | viewport = vInfo }


calcFps : MetaInfo -> MetaInfo
calcFps f =
    { f | frameCount = 0, fps = f.frameCount }



{- ------------------------------------------------------------------------------------ -}
{- MAIN -}


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



{- ------------------------------------------------------------------------------------ -}
{- INIT -}


init : () -> ( Model, Cmd Msg )
init _ =
    ( { metaInfo =
            { frameCount = 0
            , frameCountAcc = 0
            , fps = 60
            , startTime = 0
            , viewport =
                { width = 0
                , height = 0
                }
            }
      , particles =
            map
                (\x ->
                    P.createParticle
                        (vec2 x 0)
                        (vec2 -(-5 + x) 0.000001)
                        x
                )
                pBase
      }
    , Cmd.batch
        [ getViewport |> Task.perform (\v -> ViewportInfoUpdate (toViewPortInfo v))
        , getViewport |> Task.perform (\v -> InitParticlePositions (toViewPortInfo v))
        , Time.now |> Task.perform (\t -> InitStartTime t)
        ]
    )



{- ------------------------------------------------------------------------------------ -}
{- UPDATE -}


update msg model =
    case msg of
        ViewportInfoUpdate viewport ->
            let
                newModel =
                    model
                        |> setMetaInfo (setViewportInfo viewport model.metaInfo)
            in
            ( newModel, Cmd.none )

        InitParticlePositions viewport ->
            let
                widthP =
                    viewport.width / (toFloat <| List.length pBase)

                heightP =
                    viewport.height / 10

                newModel =
                    model
                        |> setParticles
                            (map
                                (\p -> P.setPos (vec2 (getX p.p * widthP) heightP) p)
                                model.particles
                            )
            in
            ( newModel, Cmd.none )

        Frame _ ->
            let
                newModel =
                    model
                        |> setMetaInfo (increaseFrameCount model.metaInfo)
            in
            ( newModel, Cmd.none )

        InitStartTime pTime ->
            let
                newModel =
                    model
                        |> setMetaInfo (setStartTime (Time.posixToMillis pTime) model.metaInfo)
            in
            ( newModel, Cmd.none )

        Tick1s _ ->
            let
                newModel =
                    model
                        |> setMetaInfo (calcFps model.metaInfo)
            in
            ( newModel, Cmd.none )

        Tick20ms pTime ->
            let
                newModel =
                    model
                        |> setParticles
                            (P.updateWorld
                                g
                                d
                                (Time.posixToMillis pTime - model.metaInfo.startTime)
                                model.particles
                            )
            in
            ( newModel, Cmd.none )



{- ------------------------------------------------------------------------------------ -}
{- SUBSCRIPTIONS -}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Frame
        , onResize (\w h -> ViewportInfoUpdate { width = toFloat w, height = toFloat h })
        , Time.every 1000 Tick1s
        , Time.every 20 Tick20ms
        ]



{- ------------------------------------------------------------------------------------ -}
{- VIEW -}


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ viewSvg model
        , viewFPS model.metaInfo.fps
        ]


viewFPS fps =
    div
        [ style "position" "absolute"
        , style "top" "1rem"
        , style "right" "1rem"
        , style "z-index" "10"
        ]
        [ text <| String.fromFloat fps ]


viewSvg : Model -> Html Msg
viewSvg model =
    impose
        {- (renderSquare model.metaInfo.frameCountAcc model.metaInfo.viewport) -} (renderParticles model.particles)
        (renderBackground model.metaInfo |> align topLeft)
        |> svg


renderBackground : MetaInfo -> Collage msg
renderBackground m =
    let
        hue =
            toFloat (m.frameCountAcc / 8 |> floor |> modBy 100) / 100
    in
    rectangle m.viewport.width m.viewport.height
        |> filled (uniform <| Color.hsl hue 0.1 0.9)


drawParticle : P.Particle -> Collage msg
drawParticle p =
    circle p.radius
        |> filled (uniform Color.red)
        |> shift ( getX p.p, -1 * getY p.p )


renderParticles : List P.Particle -> Collage msg
renderParticles ps =
    group <| map drawParticle ps


renderSquare : Float -> ViewportInfo -> Collage msg
renderSquare count v =
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
            degrees count

        hue =
            toFloat (count / 4 |> floor |> modBy 100) / 100
    in
    rectangle size size
        |> filled (uniform <| Color.hsl hue 0.45 0.7)
        |> rotate rotation
