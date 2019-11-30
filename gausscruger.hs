module GaussCruger where
import Earth
import Matrix

-- ゾーンの定義 ゾーンは西経180が0になる。
-- 東経 > 0, 西経 < 0
-- 想定 0,0 -> 31 , +180 -> 60, -180 -> 1
-- 
mod6 :: Double -> Int
mod6 lon = floor ((lon*180)/(pi*6))

zone :: Double -> Int
zone lon = mod6 lon + 31

-- ゾーンの中心 lambda0 -pi<= l0 <=pi
l0 :: Double -> Double
l0 lon = ((fromIntegral(mod6 lon)) :: Double)*pi/30+(pi/60)


-- 補助関数
toR :: [Double] -> [Double]
toR [lat,lon,h] = [Matrix.toR lat, Matrix.toR lon,h]

-- wgs2gc 緯度経度座標 -> UTM平面直角座標
wgs2gc :: Double -> [Double] -> (Double,Double)
wgs2gc l0 [lat,lon] = (x'gc lat l,y'gc lat l) where
    l = lon - l0 

y'gc :: Double -> Double -> Double
y'gc lat l = (s lat)+(n'*l^2)/2*sp*cp+(n'*l^4)/24*sp*(cp^3)*(5-t2+9*t'2+4*t'2^2)
            +(n'*l^6)/720*sp*cp^5*(61-58*t2+t2^2+270*t'2-330*t2*t'2)
            +(n'*l^8)/40320*sp*cp^7*(1385-3111*t2+543*t2^2-t2^3)
    where n' = n lat
          sp = sin lat
          cp = cos lat
          t2 = (tan lat)^2
          t'2 = e'2 * (cos lat)^2

x'gc :: Double -> Double -> Double
x'gc lat l = (n'*l)*cp+(n'*l^3)/6*cp^3*(1-t2+t'2)
             +(n'*l^5)/120*cp^5*(5-18*t2+t2^2+14*t'2-58*t2*t'2)
             +(n'*l^7)/5040*cp^7*(61-479*t2+179*t2^2-t2^3)
    where n' = n lat
          sp = sin lat
          cp = cos lat
          t2 = (tan lat)^2
          t'2 = e'2 * (cos lat)^2

wgs2utm :: [Double] -> (Int,Double,Double)
wgs2utm [lat,lon,h] = (zone lon,m0*k*(x'gc lat l)+x0,m0*k*(y'gc lat l)) where
    l = (lon - (l0 lon))
    k = 1 + (h/(sqrt ((m 0)*(n 0))))
    m0 = 0.9996
    x0 = 500000
