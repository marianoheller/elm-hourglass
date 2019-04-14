module Particle where

import Prelude
import Types
import Math (sqrt, pow, sin, cos, atan, pi)
import Data.List
import Effect (Effect)
import Graphics.Canvas as C
import Config as Conf

type Tick = Number

type Accel = TupleXY

type Particle = {
  v :: TupleXY,
  p :: TupleXY,
  collisionRadius :: Number
}

drawP :: C.Context2D -> Particle -> Effect Unit
drawP ctx p = do
  C.setFillStyle ctx "red"
  C.setStrokeStyle ctx "black"
  C.fillPath ctx $ C.arc ctx
    { x: p.p.x
    , y: p.p.y
    , radius: Conf.particleRadius
    , start: 0.0
    , end: 2.0 * pi
    }
  pure unit

initP :: TupleXY -> Particle
initP pos = {
  p: pos,
  v: {
    x: 0.0,
    y: 0.0
  },
  collisionRadius: Conf.particleRadius
}

nextTick :: Particle -> Accel -> Tick -> Particle
nextTick e a t = e {
    p = nextPosition e t,
    v = nextVelocity e a t
  }


nextPosition :: Particle -> Tick -> TupleXY
nextPosition e t = {
    x: newX,
    y: newY
  }
  where newX = (e.v.x * t + e.p.x) :: Number
        newY = (e.v.y * t + e.p.y) :: Number


nextVelocity :: Particle -> Accel -> Tick -> TupleXY
nextVelocity e a t = {
    x: newX,
    y: newY
  }
  where newX = (a.x * t + e.v.x) :: Number
        newY = (a.y * t + e.v.y) :: Number


detectCollision :: List Particle -> Particle -> Boolean
detectCollision Nil b = false
detectCollision (a:as) b = collide a b || detectCollision as b


collide :: Particle -> Particle -> Boolean
collide e1 e2 = d > sqrt ((e1.p.x * e2.p.x) + (e1.p.y * e2.p.y))
  where d = e1.collisionRadius + e2.collisionRadius

{- 
calcCollition :: Particle -> Particle -> Particle
calcCollition a c = a { p = {
    x: a.p.x + drx,
    y: a.p.y + dry
  }, v = {
    x: 
  }}
  where dx = (a.p.x - c.p.x)
        dy = (a.p.y - c.p.y)
        dr = sqrt (pow dx 2 + pow dy 2)
        drx = dr * cos $ atan (dy / dx)
        dry = dr * sin $ atan (dy / dx)
        dvx =  -}