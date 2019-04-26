module Particle exposing (Particle, createParticle, nextTick, setPos)

import Math.Vector2 exposing (..)


type alias Tick =
    Int


type alias Particle =
    { p : Vec2
    , v : Vec2
    , a : Vec2
    , radius : Float
    , mass : Float
    , restitution : Float
    }


setPos : Vec2 -> Particle -> Particle
setPos p e =
    { e | p = p }


setVel : Particle -> Vec2 -> Particle
setVel e v =
    { e | v = v }


setAccel : Particle -> Vec2 -> Particle
setAccel e a =
    { e | a = a }


createParticle : Vec2 -> Vec2 -> Float -> Particle
createParticle pos vel mass =
    { p = pos
    , v = vel
    , a = vec2 0.00000001 0.00000001
    , radius = 5
    , mass = mass
    , restitution = 5
    }

{- 
nextTick : Vec2 -> Float -> Tick -> Particle -> Particle
nextTick g dCoeff t e =
    let
        surface =
            e.radius * 2 * 3.14

        drag =
            scale (dCoeff * surface) (scale (dot e.v e.v) (normalize e.v))
            
        weight =
            scale e.mass (add g e.a)

        diff = sub drag weight

        wd =
            if getX diff < 0 || getY diff < 0 then
                vec2 0.0 0.0

            else
                diff

        a = scale (1 / e.mass) wd
    in
    { e
        | p = nextPosition a t e
        , v = nextVelocity a t e
        , a = sub a g
    } -}

nextTick : Vec2 -> Float -> Tick -> Particle -> Particle
nextTick g dCoeff t e =
    { e
        | p = nextPosition (add g e.a) t e
        , v = nextVelocity (add g e.a) t e
        , a = (sub e.a g)
    }


nextPosition : Vec2 -> Tick -> Particle -> Vec2
nextPosition a t e =
    scale (((toFloat t / 1000) ^ 2) / 2) a
        |> add (scale (toFloat t / 1000) e.v)
        |> add e.p


nextVelocity : Vec2 -> Tick -> Particle -> Vec2
nextVelocity a t e =
    scale (toFloat t / 1000) a
        |> add e.v


detectCollision : Particle -> Particle -> Bool
detectCollision a b =
    let
        r =
            a.radius + b.radius
    in
    r < (getX a.p + getX b.p) ^ 2 + (getY a.p + getY b.p) ^ 2
{- 

resolveCollision : Particle -> Particle -> Particle
resolveCollision a b =
    let
        vRelX =
            b.v.x - a.v.x

        vRelY =
            b.v.y - a.v.y

        velAlongNormal =
            vRelX - vRelY

        e =
            a.restitution

        j =
            (-(1 + e) * velAlongNormal) / (a.mass + 1 / b.mass)

        impulseX =
            j * a.v.x

        impulseY =
            j * a.v.y
    in
    if velAlongNormal < 0 then
        a

    else
        setVel a
            { x = a.v.x - (1 / a.mass * impulseX)
            , y = a.v.y - (1 / a.mass * impulseY)
            }
 -}