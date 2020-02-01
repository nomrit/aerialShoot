module GaussCruger where
import Earth
import Matrix

-- ゾーンの定義 ゾーンは西経180が1になる。
-- 東経 > 0, 西経 < 0
-- 想定 0,0 -> 31 , +180 -> 60, -180 -> 1
-- 
mod6 :: Double -> Int
mod6 lon = floor ((Matrix.toD lon)/6)

zone :: Double -> Int -- 経度からZONE番号
zone lon = mod6 lon + 31
enoz :: Int -> Double -- ZONE番号から中心の経度
enoz z = Matrix.toR(fromIntegral((z-31) * 6 + 3)::Double)

-- 緯度からそのゾーンの中心 lambda0 -pi<= l0 <=pi
l0 :: Double -> Double
l0 lon = enoz (zone lon)

-- 補助関数
toR :: [Double] -> [Double]
toR [lat,lon,h] = [Matrix.toR lat, Matrix.toR lon,h]
toD :: [Double] -> [Double]
toD [lat,lon,h] = [Matrix.toD lat, Matrix.toD lon,h]

--
-- wgs2utm
--

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

--
-- wgs2utm [lat,lon,h] = (z,x,y)
wgs2utm :: [Double] -> (Int,Double,Double)
wgs2utm [lat,lon,h] = (zone lon,m0*k*(x'gc lat l)+x0,m0*k*(y'gc lat l)) where
    l = (lon - (l0 lon))
    k = 1 + (h/(sqrt ((m 0)*(n 0))))
    m0 = 0.9996
    x0 = 500000

-------------------------------------
-- utm2wgs
--
largeA = a*(1-e2)*(1+3*e2/4+45*(e2^2)/64+175*(e2^3)/256+11025*(e2^4)/16384+43659*(e2^5)/65536) 
largeA2 = a*(1-e2)*(((((43659/65536*e2+11025/16384)*e2+175/256)*e2+45/64)*e2+3/4)*e2+1)

-- 補助関数


xy2lp :: (Double,Double) -> (Double,Double)
xy2lp (x,y) = (l,p) where p = y/largeA
                          l = x/((n p)*(cos p))
                          
lp2xy :: (Double,Double) -> (Double,Double)
lp2xy (l,p) = ((x'gc p l),(y'gc p l))

nextlp :: (Double,Double) -> (Double,Double) -> (Double,Double) -> (Double,Double)
nextlp (x,y) (x',y') (l,p)
    = (l',p') where p' = p-(y'-y)/largeA
                    l' = l-(x'/((n p')*(cos p'))-x/((n p)*(cos p)))

diff :: Double -> Double -> Bool
diff dl dp = sqrt(abs(dl)^2+abs(dp)^2) < (pi/(180*3600*1000))

unm0k :: (Double,Double) -> (Double,Double)
unm0k (x,y) = ((x-x0)/(m0*k),(y/(m0*k)))
              where k = 1 + (0/(sqrt ((m 0)*(n 0))))
                    m0 = 0.9996
                    x0 = 500000

-- u2w 
-- x:horizontal,y:vertical
{- u2w :: (Double,Double) -> (Double,Double) -> [(Double,Double)]
u2w (x,y) (l,p)
    = [(x',y'),(l',p'),(dl,dp)]
        where (x',y') = lp2xy (l,p)
              (l',p') = nextlp (x,y) (x',y') (l,p)
              (dl,dp) = (l'-l,p'-p) -}

u2w :: (Double,Double) -> (Double,Double) -> [Double]
u2w (x,y) (l,p)
        | (diff dl dp) = [p',l',0]
        | otherwise = u2w (x,y) (l',p') 
    where (x',y') = lp2xy (l,p)
          (l',p') = nextlp (x,y) (x',y') (l,p)
          (dl,dp) = (l'-l,p'-p)

-- utm2wgs (z,x,y) -> [lat,lon,h]
--
utm2wgs :: (Int,Double,Double) -> [Double]
utm2wgs (z,x,y) = zipWith (+) [0,(enoz z),0] (u2w (unm0k (x,y)) (xy2lp (x,y)))
              


