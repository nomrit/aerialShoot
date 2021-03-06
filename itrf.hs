--
-- ITRF 三次元直交座標系
--  (xy平面を赤道とし、z軸は北極。x軸方向は経度0度。y軸方向は統計90度)

-- 座標は[x,y,z]で表す。
module ITRF (wgs2itrf,itrf2wgs)
  where
import Earth
import Matrix

-- 補助関数
toR :: [Double] -> [Double]
toR [lat,lon,h] = [Matrix.toR lat, Matrix.toR lon,h]

k :: [Double] -> Double
k [x,y,_] = sqrt(x*x+y*y)

-- 極座標系WGS84 -> ITRF座標
-- 極座標系のパラメータは、Degreeにしましょう。
wgs2itrf :: [Double] -> [Double]
wgs2itrf [lat, lon, h] = [(n+h)*cos(p)*cos(l),
                      (n+h)*cos(p)*sin(l),
                      (n+h-e2*n)*sin(p)]
  where p = Matrix.toR lat
        l = Matrix.toR lon
        n = Earth.n p

-- ITRF->WGSへの補助関数
lx_0 :: Double -> Double
lx_0 y
  | y>0 = pi/2
  | y<0 = -pi/2
  | otherwise = 0

lx_neg x y
  | y>=0 = atan (y/x) + pi
  | otherwise = atan (y/x) - pi

lambda :: [Double] -> Double
lambda [x,y,_]
  | x==0 = lx_0 y
  | x>0  = atan (y/x)
  | otherwise = lx_neg x y

height :: [Double] -> Double -> Double
height i phi = (k i) / (cos phi) - (Earth.n phi)

phi' :: [Double] -> Double
phi' [x,y,z] = lambda [k [x,y,z] ,z,0]

delta :: Double -> Double -> Double -> Double
delta z k phi =
  (z*cp - k*sp + Earth.e2*(Earth.n phi)*sp*cp) /
  (z*sp + k*cp + Earth.e2*((Earth.m phi)*sp*sp - (Earth.n phi)*cp*cp))
    where cp = cos phi
          sp = sin phi

eps = 1e-7

i2w :: Double -> Double -> Double -> Double -> Double
i2w z k p d
  | abs(d) < eps = (p+d)
  | otherwise = i2w z k (p+d) d' where d' = delta z k (p+d)
  
-- ITRF -> WGS84 変換
-- リターンの極座標系は、Degreeに戻すで良いか？
itrf2wgs :: [Double] -> [Double]
itrf2wgs i = [Matrix.toD(p),Matrix.toD(l),h]
  where l = lambda i
        p = i2w z (k i) phip d
          where [x,y,z] = i
                phip = phi' i
                d = delta z (k i) phip
        h = height i p

-- ITRF -> LCF(Local Cartesian Frame) 変換
itrf2lcf :: [Double] -> [Double] -> [Double]
itrf2lcf [lat,lon,h] [x,y,z] = (f [x,y,z]) |-| (f (wgs2itrf [lat,lon,h])) 
  where f = wgs2rot [lat,lon,h]

-- WGS84 -> LCF 
wgs2rot :: [Double] -> [Double] -> [Double]
wgs2rot d1 d2 = w2r (ITRF.toR d1) d2

-- 内部関数
-- itrfをzを軸にl回転し、yを軸にpi/4-pだけ回転させたものになる。
w2r :: [Double] -> [Double] -> [Double]
w2r [lat,lon,_] [x,y,z] = [ sp*cl*x+sp*sl*y+((-cp)*z), (-sl)*x+cl*y, cp*cl*x+cp*sl*y+sp*z ]
  where sp = sin lat
        sl = sin lon
        cp = cos lat
        cl = cos lon

-- LCF -> ITRF 変換
lcf2itrf :: [Double] -> [Double] -> [Double]
lcf2itrf d [x,y,z] = w2r' (ITRF.toR d) ([x,y,z] |+| (wgs2rot d (wgs2itrf d)))

-- 内部関数
w2r' [lat,lon,_] [x,y,z] = [ sp*cl*x+(-sl)*y+cp*cl*z, sp*sl*x+cl*y+cp*sl*z, (-cp)*x+sp*z ]
  where sp = sin lat
        sl = sin lon
        cp = cos lat
        cl = cos lon


-- 試験用関数 mapf
mapf :: [([Double]->a)] -> [Double] -> [a]
mapf [] _ = []
mapf (f:fs) x = f x:mapf fs x
