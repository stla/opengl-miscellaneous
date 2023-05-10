module HopfTorus
  (main)
  where
import           Control.Monad                     (forM_, when)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Data.Tuple.Extra                  (fst3, snd3, thd3)
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Linear                            (V3 (..))
import           Text.Printf
import           Utils.Hopf
import           Utils.TorusTransformationMatrix

type Point = (GLfloat,GLfloat,GLfloat)

pointToV3 :: Point -> V3 GLfloat
pointToV3 (x,y,z) = V3 x y z

tripletOnCircle :: Int -> GLfloat -> (Point, Point, Point)
tripletOnCircle m u = -- m is the number of lobes
  ( stereoCircHinv'' x y z 0
  , stereoCircHinv'' x y z 2
  , stereoCircHinv'' x y z 4)
  where
    a = 0.44
    nlobes = fromIntegral m
    b = pi/2 - (pi/2 - a)*cos(u*nlobes)
    c = u + a*sin(2*u*nlobes)
    x = cos b
    sinb = sin b
    y = sinb * cos c
    z = sinb * sin c

myTriplets :: Int -> Int -> [(Point, Point, Point)]
myTriplets m n = map (tripletOnCircle m) u_ -- n: number of circles
  where
    u_ = [-pi + 2 * pi * frac i n | i <- [0 .. n-1]]
    frac :: Int -> Int -> GLfloat
    frac p q = realToFrac p / realToFrac q

data Context = Context
    {
      contextRot1 :: IORef GLfloat
    , contextRot2 :: IORef GLfloat
    , contextRot3 :: IORef GLfloat
    , contextZoom :: IORef Double
    }

white,black,blue :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
blue       = Color4    0    0    1    1

tmatsAndRadii :: Int -> Int -> [([GLfloat], GLdouble)]
tmatsAndRadii m n = map (\triplet -> torusTransformationMatrix
                                     (pointToV3 $ fst3 triplet)
                                     (pointToV3 $ snd3 triplet)
                                     (pointToV3 $ thd3 triplet))
                        (myTriplets m n)

display :: Context -> IORef GLfloat -> IORef Int -> IORef Int -> DisplayCallback
display context alpha m n = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  zoom <- get (contextZoom context)
  alpha' <- get alpha
  m' <- get m
  n' <- get n
  (_, size) <- get viewport
  loadIdentity
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  rotate alpha' $ Vector3 1 1 1
  forM_ (tmatsAndRadii m' n') $ \tmatAndRadius ->
    preservingMatrix $ do
      tmat <- newMatrix RowMajor (fst tmatAndRadius) :: IO (GLmatrix GLfloat)
      multMatrix tmat
      materialDiffuse Front $= blue
      renderObject Solid $ Torus 0.2 (snd tmatAndRadius) 30 30
  swapBuffers

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-35+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef GLdouble -- zoom
         -> IORef Bool -- animation
         -> IORef Int -- number of lobes
         -> IORef Int -- number of circles
         -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom anim m n c _ = do
  case c of
    'a' -> writeIORef anim True
    'g' -> m $~! (+1)
    'b' -> m $~! \x -> if x>1 then x-1 else x
    'h' -> n $~! (+50)
    'n' -> n $~! \x -> if x>100 then x-50 else x
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+2)
    'm' -> zoom $~! (+1)
    'l' -> zoom $~! subtract 1
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing

idle :: IORef Bool -> IORef Int -> IORef GLfloat -> IdleCallback
idle anim snapshots alpha = do
    a <- get anim
    s <- get snapshots
    when a $ do
      when (s < 360) $ do
        let ppm = printf "ppm/htorus%04d.ppm" s
        (>>=) capturePPM (B.writeFile ppm)
      snapshots $~! (+1)
      alpha $~! (+1)
      postRedisplay Nothing
    return ()

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Hopf torus"
  windowSize $= Size 512 512
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= black
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= black
  depthFunc $= Just Less
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  alpha <- newIORef 0.0
  m <- newIORef 3
  n <- newIORef 150
  anim <- newIORef False
  snapshots <- newIORef 0
  displayCallback $= display Context { contextRot1 = rot1,
                                       contextRot2 = rot2,
                                       contextRot3 = rot3,
                                       contextZoom = zoom }
                             alpha m n
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom anim m n)
  idleCallback $= Just (idle anim snapshots alpha)
  putStrLn "*** Hopf torus ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease number of lobes: g,b\n\
        \    Increase/decrease number of circles: h,n\n\
        \    Animation: a\n\
        \"
  mainLoop
