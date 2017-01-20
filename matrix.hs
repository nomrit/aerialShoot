module Matrix
       ( toR, toD, dot, (|.|), cross, (|*|), (|+|), sc, (/*/) )
       where
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
