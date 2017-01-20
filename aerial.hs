module Aerial where
import Quaternion

---- 航空機の回転
data Coord = CD [Double] [Double] [Double]
  deriving(Show)

ident = CD [1,0,0] [0,1,0] [0,0,1]

toList :: Coord -> [[Double]]
toList (CD x y z) = [x,y,z]

-- 直交かどうかはとりあえず気にしない
toCoord :: [[Double]] -> Coord
toCoord [x,y,z] = (CD x y z)

-- 座標系をX軸で回転。機体の場合はロール。
-- rotX <座標系> <角度>
rotX :: Double -> Coord -> Coord
rotX th (CD x y z) = (CD x (rot r y) (rot r z))   where r = rotator x th

-- 座標系をY軸で回転。機体の場合はピッチ。カメラの場合はチルト。
-- rotY <座標系> <角度> 
rotY :: Double -> Coord -> Coord
rotY th (CD x y z) = (CD (rot r x) y (rot r z))  where r = rotator y th

-- 座標系をZ軸で回転。機体の場合は方位角(ヨー)。カメラの場合はパン。
rotZ :: Double -> Coord -> Coord
rotZ th (CD x y z) = (CD (rot r x) (rot r y) z)  where r = rotator z th

-- 航空機の回転。
yaw :: Double -> Coord -> Coord
yaw azi c = rotZ azi c

-- 航空機のロール。
roll :: Double -> Coord -> Coord
roll r c = rotX r c

-- 航空機のピッチ。
pitch :: Double -> Coord -> Coord
pitch p c = rotY p c

-- カメラのパン。
pan :: Double -> Coord -> Coord
pan p c = rotZ p c

-- カメラのチルト。
tilt :: Double -> Coord -> Coord
tilt t c = rotY t c

-- 全部の回転を組み合わせる。
tppry :: Double -> Double -> Double -> Double -> Double -> Coord -> Coord
tppry y r pt pa t c = (tilt t).(pan pa).(pitch pt).(roll r).(yaw y) $ c

-- 光軸
los :: Coord -> [Double]
los (CD x _ _) = x
