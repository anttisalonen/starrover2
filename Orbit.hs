module Orbit
where

type PointF = (Float, Float)

type Orbit = Float -> PointF

combineOrbits :: Orbit -> Orbit -> Orbit
combineOrbits f1 f2 = \a ->
  let (x0, y0) = f1 a
      (x1, y1) = f2 a
  in (x0 + x1, y0 + y1)

unitCircle :: Float -> PointF
unitCircle a = (cos a, sin a)

circleR :: Float -> Float -> PointF
circleR r a = 
  let (x, y) = unitCircle a
  in (r * x, r * y)

circleVel :: Float -> Float -> Float -> PointF
circleVel v r a = circleR r (a * v * pi * 2)

ellipse :: Float -> Float -> Float -> PointF
ellipse a b t = (a * cos t, b * sin t)

ellipseVel :: Float -> Float -> Float -> Float -> PointF
ellipseVel v a b t = (a * cos (v * t), b * sin (v * t))

shiftedEllipseVel :: Float -> Float -> Float -> Float -> Float -> Float -> PointF
shiftedEllipseVel x0 y0 v a b t = 
  let (x, y) = ellipseVel v a b t
  in (x + x0, y + y0)

