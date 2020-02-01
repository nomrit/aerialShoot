-- ellipsoidal model of the earth
module Earth where
import Matrix

equatorialRadius = 6378137.0 -- 長半径(m)
flattening = 1 / 298.257222101 -- 扁平率

{- 本番用-}

a = equatorialRadius
f = flattening

{- 試験用 半径1000mの真球

a = 1000
f = 0
 -}

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

-- 子午線弧長。latはRadian。
arcMeridian :: Double -> Double
arcMeridian lat = a * (1-e2)*(a'*lat - (b'/2)*(sin(2*lat)) + (c'/4)*(sin(4*lat)) - (d'/6)*(sin(6*lat)) + (e'/8)*(sin(8*lat)) - (f'/10)*(sin(10*lat)))
  where a' = (((((43659/65536)*e2+(11025/16384))*e2+(175/256))*e2+(45/64))*e2+(3/4))*e2+1
        b' = (((((72765/65536)*e2+(2205/2048))*e2+(525/512))*e2+(15/16))*e2+(3/4))*e2
        c' = ((((10395/16384)*e2+(2205/4096))*e2+(105/256))*e2+(15/64))*(e2^2)
        d' = (((31185/131072)*e2+(315/2048))*e2+(35/512))*(e2^3)
        e' = ((3465/65536)*e2+(315/16384))*(e2^4)
        f' = (693/131072)*(e2^5)
s = arcMeridian

-- 平行圏弧長。parallel of latitude
parLatitude :: Double -> Double -> Double
parLatitude lat lon = lon * (a * (cos lat)) / (sqrt (1-(e2*((sin lat)^2))))