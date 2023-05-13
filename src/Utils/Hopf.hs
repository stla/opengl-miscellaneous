module Utils.Hopf
  (stereoCircHinv'', stereoCircHinv)
  where

hopfinverse :: Floating a => a -> a -> a -> a -> (a,a,a,a)
hopfinverse q0 q1 q2 t =
  let h = 1 / sqrt (2 * (1+q0)) in
  (h * (cos t * q2 + sin t * q1),
   h * (cos t * q1 - sin t * q2),
   h * sin t * (1+q0),
   h * cos t * (1+q0))

-- hopfinverse' :: (Num a, Floating a) => a -> a -> a -> (a,a,a,a)
-- hopfinverse' theta phi =
--   hopfinverse (cos theta * cos phi) (sin theta * cos phi) (sin phi) 

stereoProj :: Floating a => (a,a,a,a) -> (a,a,a)
stereoProj (x0,x1,x2,x3) =
  let h = 1/(1-x3) in
  (h * x0, h * x1, h * x2)

stereoCircHinv :: Floating a => a -> a -> a -> (a, a, a)
stereoCircHinv theta phi xi =
  stereoProj (hopfinverse q0 q1 q2 xi)
  where
    q0 = cos theta * cos phi
    q1 = sin theta * cos phi
    q2 = sin phi

-- stereoCircHinv' :: (Num a, Floating a) => a -> a -> a -> (a, a, a)
-- stereoCircHinv' theta phi xi =
--   stereoProj (hopfinverse q0 q1 q2 xi)
--   where
--     q0 = cos theta * sin phi
--     q1 = sin theta * sin phi
--     q2 = cos phi

stereoCircHinv'' :: Floating a => a -> a -> a -> a -> (a, a, a)
stereoCircHinv'' q0 q1 q2 xi =
  stereoProj (hopfinverse q0 q1 q2 xi)
