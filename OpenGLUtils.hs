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

glVector3Null :: GLvector3
glVector3Null = (0, 0, 0)

