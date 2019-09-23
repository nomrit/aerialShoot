-- ellipsoidal model of the earth
module Earth where
import Matrix

equatorialRadius = 6378137.0 -- 長半径(m)
flattening = 1 / 298.257222101 -- 扁平率

{- 本番用
a = equatorialRadius
f = flattening
-}

{- 試験用 半径1000mの真球 -}

a = 1000
f = 0

polarRadius = a * (1-f) -- 短半径
b = polarRadius

radiusOfCurvature = a^2 / b
c = radiusOfCurvature

e2 = (a^2 - b^2) / (a^2) -- 第一離心率
e'2 = (a^2 - b^2) / (b^2) -- 第二離心率

-- 卯酉線曲率半径。latはRadian。
cPrimeVertical :: Double -> Double
cPrimeVertical lat = c / (sqrt (1+e'2*(cos(lat))^2))
n = cPrimeVertical

-- 子午線曲率半径。latはRadian。
cMeridian :: Double -> Double
cMeridian lat = n / (1+e'2*(cos(lat))^2)
  where n = cPrimeVertical lat
m = cMeridian
