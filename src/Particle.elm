module Particle exposing (Particle, createParticle)

type alias TupleXY = { x : Float, y : Float }

type alias Tick = Float

type alias Particle = {
    v : TupleXY,
    p : TupleXY,
    collisionRadius : Float,
    particleRadius : Float
  }


{- Lenses??? -}
setPos : Particle -> TupleXY -> Particle 
setPos e p = { e | p = p }

setVel : Particle -> TupleXY -> Particle 
setVel e v = { e | v = v }


createParticle : TupleXY -> Particle
createParticle pos = {
    p = pos,
    v = {
      x = 0.0,
      y = 0.0
    },
    collisionRadius = 5,
    particleRadius = 5
  }

nextTick : Particle -> TupleXY -> Tick -> Particle
nextTick e a t = { e |
    p = nextPosition e t,
    v = nextVelocity e a t
  }

nextPosition : Particle -> Tick -> TupleXY
nextPosition e t = {
    x = (e.v.x * t + e.p.x),
    y = (e.v.y * t + e.p.y)
  }


nextVelocity : Particle -> TupleXY -> Tick -> TupleXY
nextVelocity e a t = {
    x = (a.x * t + e.v.x),
    y = (a.y * t + e.v.y)
  }

{- 

detectCollision : List Particle -> Particle -> Boolean
detectCollision (a:as) b = collide a b || detectCollision as b


collide : Particle -> Particle -> Boolean
collide e1 e2 = d > sqrt ((e1.p.x * e2.p.x) + (e1.p.y * e2.p.y))
  where d = e1.collisionRadius + e2.collisionRadius -}
