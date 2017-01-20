module Quaternion
       (Quaternion(..),rotator,rot)
       where

import Matrix

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

