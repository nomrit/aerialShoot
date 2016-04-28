module Aerial where
---- 一般
toR::Double -> Double
toR d = (d/180*pi)

toD::Double->Double
toD r = (r/pi*180)

---- 行列演算の定義

-- 内積
dot :: [Double] -> [Double] -> Double
dot x y = sum $ zipWith (*) x y

(|.|) :: [Double] -> [Double] -> Double
x |.| y = dot x y

-- 外積
cross :: [Double] -> [Double] -> [Double]
cross [a1,a2,a3] [b1,b2,b3] = [a2*b3 - a3*b2,a3*b1 - a1*b3, a1*b2 - a2*b1]

(|*|) :: [Double] -> [Double] -> [Double]
x |*| y = cross x y

-- 和
(|+|) :: [Double] -> [Double] -> [Double]
x |+| y = zipWith (+) x y

-- スカラー積
sc :: Double -> [Double] -> [Double]
sc x y = map (* x) y

(/*/) :: Double -> [Double] -> [Double]
x /*/ y = sc x y

---- クォータニオンの定義

data Quaternion = QT Double [Double]
  deriving (Show)

-- 実部
re :: Quaternion -> Double
re (QT r i) = r

-- 虚部
im :: Quaternion -> [Double]
im (QT r i) = i

-- クォータニオンの積
qm :: Quaternion -> Quaternion -> Quaternion
qm (QT r1 i1) (QT r2 i2) =
  QT (r1*r2-(i1 |.| i2))
  ((r1 /*/ i2) |+| (r2 /*/ i1) |+| (i1 |*| i2))

(¥**¥) :: Quaternion -> Quaternion -> Quaternion
q1 ¥**¥ q2 = qm q1 q2

-- 共役
conj :: Quaternion -> Quaternion
conj x = QT (re x) ((-1) /*/ (im x))

---- クォータニオンでの回転

-- 軸と角度を渡すと、その回転を計算するクォータニオンを返す。
rotator :: [Double] -> Double -> Quaternion
rotator [x,y,z] th = QT (cos (th/2)) [(x/l)*v, (y/l)*v, (z/l)*v]
  where l = sqrt(x**2+y**2+z**2)
        v = sin (th/2)

-- rotatorでPointを回転する
rot :: Quaternion -> [Double] -> [Double]
rot r p = q where (QT _ q) = (r ¥**¥ (QT 0 p) ¥**¥ (conj r))

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
optAxis :: Coord -> [Double]
optAxis (CD x _ _) = x
