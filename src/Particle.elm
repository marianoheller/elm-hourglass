module Particle exposing (Particle, createParticle, nextTick, setPos)


type alias TupleXY =
    { x : Float, y : Float }


type alias Tick =
    Int


type alias Particle =
    { p : TupleXY
    , v : TupleXY
    , a : TupleXY
    , collisionRadius : Float
    , particleRadius : Float
    , mass : Float
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
    , collisionRadius = 5
    , particleRadius = 5
    , mass = mass
    }


nextTick : TupleXY -> Float -> Tick -> Particle -> Particle
nextTick g dCoeff t e =
    let
        surface =
            e.particleRadius * 2 * 3.14

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



{-

   detectCollision : List Particle -> Particle -> Boolean
   detectCollision (a:as) b = collide a b || detectCollision as b


   collide : Particle -> Particle -> Boolean
   collide e1 e2 = d > sqrt ((e1.p.x * e2.p.x) + (e1.p.y * e2.p.y))
     where d = e1.collisionRadius + e2.collisionRadius
-}
