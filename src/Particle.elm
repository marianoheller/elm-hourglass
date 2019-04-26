module Particle exposing (Particle, createParticle, moveEm, setPos, updateWorld)

import List exposing (..)
import Math.Vector2 exposing (..)
import Tuple exposing (..)


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



updateWorld : Vec2 -> Float -> Tick -> List Particle -> List Particle
updateWorld g d t ps =
    map (moveEm g d t) ps
        |> collideEm

{- 
moveEm : Vec2 -> Float -> Tick -> Particle -> Particle
moveEm g dCoeff t e =
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
                vec2 0.00001 0.00001

            else
                diff

        a = scale (1 / e.mass) wd
    in
    { e
        | p = nextPosition a t e
        , v = nextVelocity a t e
        , a = sub a g
    }

 -}


moveEm : Vec2 -> Float -> Tick -> Particle -> Particle
moveEm g dCoeff t e =
    { e
        | p = nextPosition (add g e.a) t e
        , v = nextVelocity (add g e.a) t e
        , a = sub e.a g
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


collideEm : List Particle -> List Particle
collideEm ps =
    let
        detected = map (\p -> ( p, detectCollisions ps p )) ps
    in
        {- resolveCollisions detected -}
        ps


detectCollisions : List Particle -> Particle -> List Particle
detectCollisions xs y =
    foldl
        (\x acc ->
            if collided x y == True then
                x :: acc

            else
                acc
        )
        []
        xs


collided : Particle -> Particle -> Bool
collided a b =
    let
        r =
            a.radius + b.radius

        d = (distance a.p b.p)
            
    in
    r > d


resolveCollisions : List ( Particle, List Particle ) -> List Particle
resolveCollisions ps =
    map
        (\( x, xs ) -> resolveCollision x xs)
        ps


resolveCollision : Particle -> List Particle -> Particle
resolveCollision a bs =
    let
        b =
            { mass = foldl (\c acc -> c.mass + acc) 0 bs
            , v = foldl (\c acc -> add c.v acc) (vec2 0 0) bs
            }

        rv =
            sub b.v a.v

        normal =
            normalize rv

        velAlongNormal =
            dot rv normal

        e =
            a.restitution

        j =
            (-(1 + e) * velAlongNormal) / (a.mass + 1 / b.mass)

        impulse =
            scale j normal
    in
    if velAlongNormal > 0 then
        a

    else
        setVel a (sub a.v (scale (1 / a.mass) impulse))
