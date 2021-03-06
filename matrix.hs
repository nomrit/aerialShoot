module Matrix
       ( toR, toD, dot, (|.|), cross, (|*|), (|+|), (|-|), sc, (/*/), det, cramer )
       where
---- 一般
round::Double -> Int
round x = floor (x+1/2)

toR::Double -> Double
toR d = d*pi/180

toD::Double->Double
toD r = (fromIntegral (Matrix.round((r/pi*180)*(10^7)))::Double)/(10^7)

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

-- 差
(|-|) :: [Double] -> [Double] -> [Double]
x |-| y = zipWith (-) x y

-- スカラー積
sc :: Double -> [Double] -> [Double]
sc x y = map (* x) y

(/*/) :: Double -> [Double] -> [Double]
x /*/ y = sc x y

-- 行列式(3x3まで)
det :: [[Double]] -> Double
det [[a]] = a
det [[a,c],[b,d]] = a*d - b*c
det [[a1,a4,a7],[a2,a5,a8],[a3,a6,a9]] = a1*a5*a9+a2*a6*a7+a3*a4*a8-a3*a5*a7-a2*a4*a9-a1*a6*a8

-- クラメルの式による方程式の解
cramer :: [[Double]] -> [Double] -> [Double]
cramer [a1,a2] b = [(det [b,a2])/a,(det [a1,b])/a]
                    where a = det [a1,a2]
cramer [a1,a2,a3] b = [(det [b,a2,a3])/a,(det [a1,b,a3])/a,(det [a1,a2,b])/a]
                    where a = det [a1,a2,a3]