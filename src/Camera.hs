module Camera
where

import Graphics.Rendering.OpenGL as OpenGL

import OpenGLUtils

type Camera = ((GLdouble, GLdouble), (GLdouble, GLdouble))

setCamera :: Camera -> IO ()
setCamera ((minx, miny), (diffx, diffy)) = do
  matrixMode $= Projection
  loadIdentity
  ortho minx (minx + diffx) miny (miny + diffy) (-10) 10
  matrixMode $= Modelview 0

setZoom :: GLdouble -> Camera -> Camera
setZoom z ((minx, miny), (diffx, diffy)) =
  let ndiffx = z
      ndiffy = z * (diffy / diffx)
      ocent = (minx + diffx / 2, miny + diffy / 2, 0)
  in setCentre ocent ((0, 0), (ndiffx, ndiffy))

setCentre :: GLvector3 -> Camera -> Camera
setCentre (nx, ny, _) ((_, _), (diffx, diffy)) =
  ((nx - diffx / 2, ny - diffy / 2), (diffx, diffy))


