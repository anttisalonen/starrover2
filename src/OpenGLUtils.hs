module OpenGLUtils
where

import Control.Monad.State

import Graphics.Rendering.OpenGL as OpenGL

type GLvector2 = (GLdouble, GLdouble)

type GLvector3 = (GLdouble, GLdouble, GLdouble)

(*+*) :: GLvector3 -> GLvector3 -> GLvector3
(*+*) (x0, y0, z0) (x1, y1, z1) = (x0 + x1, y0 + y1, z0 + z1)

(*-*) :: GLvector3 -> GLvector3 -> GLvector3
(*-*) (x0, y0, z0) (x1, y1, z1) = (x0 - x1, y0 - y1, z0 - z1)

(***) :: GLvector3 -> GLdouble -> GLvector3
(***) (x0, y0, z0) s = (x0 * s, y0 * s, z0 * s)

length2 :: GLvector3 -> GLdouble
length2 (x0, y0, z0) = x0 ** 2 + y0 ** 2 + z0 ** 2

length :: GLvector3 -> GLdouble
length = sqrt . length2

normalize :: GLvector3 -> GLvector3
normalize (0, 0, 0) = (0, 0, 0)
normalize v@(x, y, z) = 
  let l = OpenGLUtils.length v
  in (x / l, y / l, z / l)

glVector3Null :: GLvector3
glVector3Null = (0, 0, 0)

glVector3AllUnit :: GLvector3
glVector3AllUnit = (1, 1, 1)

glVector3UnitX :: GLvector3
glVector3UnitX = (1, 0, 0)

glVector3UnitY :: GLvector3
glVector3UnitY = (0, 1, 0)

glVector3UnitZ :: GLvector3
glVector3UnitZ = (0, 0, 1)

circlePoints :: Int -> [GLvector3]
circlePoints n = 
  let xs = map (sin . (2 * pi *) . (/(fromIntegral n)) . fromIntegral) [0..(n - 1)]
      ys = map (cos . (2 * pi *) . (/(fromIntegral n)) . fromIntegral) [0..(n - 1)]
      zs = repeat 0
  in zip3 xs ys zs

uniformScale :: (MatrixComponent c) => c -> IO ()
uniformScale x = OpenGL.scale x x x

inOrthoBoxDraw :: (MonadIO m) => GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> m () -> m ()
inOrthoBoxDraw minx maxx miny maxy minz maxz f = do
  liftIO $ matrixMode $= Projection
  liftIO $ loadIdentity
  liftIO $ ortho minx maxx miny maxy minz maxz
  liftIO $ matrixMode $= Modelview 0
  f

vecToVec :: GLvector3 -> Vector3 GLdouble
vecToVec (x, y, z) = Vector3 x y z
