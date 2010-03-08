module OpenGLUtils
where

import Graphics.Rendering.OpenGL as OpenGL

type GLvector2 = (GLdouble, GLdouble)

type GLvector3 = (GLdouble, GLdouble, GLdouble)

(*+*) :: GLvector3 -> GLvector3 -> GLvector3
(*+*) (x0, y0, z0) (x1, y1, z1) = (x0 + x1, y0 + y1, z0 + z1)

(*-*) :: GLvector3 -> GLvector3 -> GLvector3
(*-*) (x0, y0, z0) (x1, y1, z1) = (x0 - x1, y0 - y1, z0 - z1)

(***) :: GLvector3 -> Double -> GLvector3
(***) (x0, y0, z0) s = (x0 * s, y0 * s, z0 * s)

length2 :: GLvector3 -> GLdouble
length2 (x0, y0, z0) = x0 ** 2 + y0 ** 2 + z0 ** 2

length :: GLvector3 -> GLdouble
length = sqrt . length2

glVector3Null :: GLvector3
glVector3Null = (0, 0, 0)

glVector3AllUnit :: GLvector3
glVector3AllUnit = (1, 1, 1)

circlePoints :: Int -> [GLvector3]
circlePoints n = 
  let xs = map (sin . (2 * pi *) . (/(fromIntegral n)) . fromIntegral) [0..(n - 1)]
      ys = map (cos . (2 * pi *) . (/(fromIntegral n)) . fromIntegral) [0..(n - 1)]
      zs = repeat 0
  in zip3 xs ys zs

