module Particle exposing (Particle, createParticle, nextTick, setPos)


type alias TupleXY =
    { x : Float, y : Float }


type alias Tick =
    Int


type alias Particle =
    { p : TupleXY
    , v : TupleXY
    , a : TupleXY
    , radius : Float
    , mass : Float
    , restitution : Float
    }


setPos : TupleXY -> Particle -> Particle
setPos p e =
    { e | p = p }


setVel : Particle -> TupleXY -> Particle
setVel e v =
    { e | v = v }


setAccel : Particle -> TupleXY -> Particle
setAccel e a =
    { e | a = a }


createParticle : TupleXY -> Float -> Particle
createParticle pos mass =
    { p = pos
    , a =
        { x = 0.0
        , y = 0.0
        }
    , v =
        { x = 0.0
        , y = 0.0
        }
    , radius = 5
    , mass = mass
    , restitution = 5
    }


nextTick : TupleXY -> Float -> Tick -> Particle -> Particle
nextTick g dCoeff t e =
    let
        surface =
            e.radius * 2 * 3.14

        dragX =
            dCoeff * (e.v.x ^ 2) * surface

        dragY =
            dCoeff * (e.v.y ^ 2) * surface

        weightX =
            e.mass * (g.x + e.a.x)

        weightY =
            e.mass * (g.y + e.a.y)

        wxdx =
            if weightX - dragX < 0 then
                0

            else
                dragX - weightX

        wydy =
            if abs weightY - abs dragY < 0 then
                0

            else
                dragY - weightY

        a =
            { x = wxdx / e.mass
            , y = wydy / e.mass
            }
    in
    { e
        | p = nextPosition t e
        , v = nextVelocity a t e
        , a =
            { x = a.x - g.x
            , y = a.y - g.y
            }
    }


nextPosition : Tick -> Particle -> TupleXY
nextPosition t e =
    { x = e.v.x * (toFloat t / 1000) + e.p.x
    , y = e.v.y * (toFloat t / 1000) + e.p.y
    }


nextVelocity : TupleXY -> Tick -> Particle -> TupleXY
nextVelocity a t e =
    { x = a.x * (toFloat t / 1000) + e.v.x
    , y = a.y * (toFloat t / 1000) + e.v.y
    }


detectCollision : Particle -> Particle -> Bool
detectCollision a b =
    let
        r =
            a.radius + b.radius
    in
    r < (a.p.x + b.p.x) ^ 2 + (a.p.y + b.p.y) ^ 2

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

