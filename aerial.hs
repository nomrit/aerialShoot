module Aerial (Frame(..), ident,tppry, module Quaternion) where
import Quaternion

---- 航空機の座標系
data Frame = FR [Double] [Double] [Double]
  deriving(Show)

ident = FR [1,0,0] [0,1,0] [0,0,1]

toList :: Frame -> [[Double]]
toList (FR x y z) = [x,y,z]

-- 直交かどうかはとりあえず気にしない
toFrame :: [[Double]] -> Frame
toFrame [x,y,z] = (FR x y z)

-- 座標系をX軸で回転。機体の場合はロール。
-- rotX <座標系> <角度>
rotX :: Double -> Frame -> Frame
rotX th (FR x y z) = (FR x (rot r y) (rot r z))   where r = rotator x th

-- 座標系をY軸で回転。機体の場合はピッチ。カメラの場合はチルト。
-- rotY <座標系> <角度> 
rotY :: Double -> Frame -> Frame
rotY th (FR x y z) = (FR (rot r x) y (rot r z))  where r = rotator y th

-- 座標系をZ軸で回転。機体の場合は方位角(ヨー)。カメラの場合はパン。
rotZ :: Double -> Frame -> Frame
rotZ th (FR x y z) = (FR (rot r x) (rot r y) z)  where r = rotator z th

-- 航空機の回転。
yaw :: Double -> Frame -> Frame
yaw azi c = rotZ azi c

-- 航空機のロール。
roll :: Double -> Frame -> Frame
roll r c = rotX r c

-- 航空機のピッチ。
pitch :: Double -> Frame -> Frame
pitch p c = rotY p c

-- カメラのパン。
pan :: Double -> Frame -> Frame
pan p c = rotZ p c

-- カメラのチルト。
tilt :: Double -> Frame -> Frame
tilt t c = rotY t c

-- 全部の回転を組み合わせる。
tppry :: Double -> Double -> Double -> Double -> Double -> Frame -> Frame
tppry y r pt pa t c = (tilt t).(pan pa).(pitch pt).(roll r).(yaw y) $ c

