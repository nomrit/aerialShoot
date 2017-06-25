module Geodesics
       where
import Earth

--
-- Problem 1
--
--
-- Schreiber

-- spherical excess
se :: Double -> Double -> Double -> Double
se lat a s = (s^2*sin(a)*cos(a)) / (2*m(lat)*n(lat))

--
theX :: Double -> Double -> Double -> Double
theX a s e = s*cos(a)+(2*e/3)*s*sin(a)

theY :: Double -> Double -> Double -> Double
theY a s e = s*sin(a)-(e/3)*s*cos(a)

theXY :: Double->Double->Double->(Double,Double)
theXY lat a s = ((theX a s e),(theY a s e))
  where e = se lat a s
  
--
phiF :: Double -> Double -> Double
phiF x lat =
  ((x / n1) - (3*x^2)/(2*n1^2)*t1*eta12 - (x^3)/(2*n1^3)*(1-t1^2)*eta12 +
   (x^4)/(2*n1^4)*t1*eta12) * v1 + lat
  where n1 = n lat
        t1 = tan lat
        eta12 = e'2*((cos lat)^2)
        v1 = 1+e'2*((cos lat)^2)

--
lambda2 :: Double -> Double -> Double -> Double
lambda2 y pf lon =
  ((y/nf)-(y^3)/(3*nf^3)*tf2+(y^5)/(15*nf^5)*tf2*(1+3*tf2))/(cos pf)+lon
  where nf = n pf
        tf2 = (tan pf)^2

phi2 :: Double -> Double -> Double
phi2 y pf =
  (-(y2/(2*nf2)*tf)+(y2^2)/(24*nf2^2)*tf*(1+3*tf^2+etaf2-9*tf^2*etaf2)) * vf2 + pf
  where y2 = y*y
        nf2 = (n pf)^2
        tf = tan pf
        etaf2 = e'2*((cos pf)^2)
        vf2 = (1+e'2*((cos pf)^2))

--
gamma :: Double -> Double -> Double
gamma y pf =
  (y/nf)*tf -
  (y^3)/(6*nf^3)*tf*(1+2*tf^2+etaf2)+
  (y^5)/(120*nf^5)*tf*(1+20*tf^2+24*tf^4)
  where nf = n pf
        tf = tan pf
        etaf2 = e'2*((cos pf)^2)

reverseHeading:: Double -> Double -> Double ->Double ->Double
reverseHeading y pf a es = a + pi + (gamma y pf) - es

--
-- まとめ
--
tgtPosition :: [Double] -> Double -> Double -> [Double]
tgtPosition [lat1,lon1] s a =
  [(phi2 y pf),(lambda2 y pf lon1)] where (x,y) = theXY lat1 a s
                                          pf = phiF x lat1
