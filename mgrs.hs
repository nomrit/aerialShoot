module Mgrs where
import GaussCruger
import Matrix

-- 緯度→グリッド
-- 緯度はRadian
--
lat2grid :: Double -> String
lat2grid lat = if (d >= 80) then "X" else [takegrid (truncate ((d+80)/8))] where d = Matrix.toD lat

takegrid :: Int -> Char
takegrid n = gridlist !! n

gridlist = "CDEFGHJKLMNPQRSTUVWX"

-- セット番号
--本当は1,2,3,4,5,6だけど、moduloで出して0,1,2,3,4,5とした方がよいので、セット6をセット0と読み替える。
zone2set :: Int -> Int
zone2set z = (rem z 6)

-- 100000 meter square
-- 100000 meter格子はEとNで示す。

htmE = ["_STUVWXYZ","_ABCDEFGH","_JKLMNPQR","_STUVWXYZ","_ABCDEFGH","_JKLMNPQR"]
htmN = ["FGHJKLMNPQRSTUVABCDE","ABCDEFGHJKLMNPQRSTUV","FGHJKLMNPQRSTUVABCDE",
         "ABCDEFGHJKLMNPQRSTUV","FGHJKLMNPQRSTUVABCDE","ABCDEFGHJKLMNPQRSTUV"]

truncY :: Double -> Int
truncY y = truncate ((fromIntegral(rem (truncate y) 2000000)::Double)/100000.0)

x2htm :: Int -> Double -> Char
x2htm z x = (htmE !! (zone2set z)) !! (truncate (x/100000.0))

y2htm :: Int -> Double -> Char
y2htm z y = (htmN !! (zone2set z)) !! (truncY y)

xy2htm :: (Int,Double,Double) -> String
xy2htm (z,x,y) = [(x2htm z x),(y2htm z y)]

-- E,N
rem5 :: Double -> Double
rem5 d = (fromIntegral (rem a 100000)::Double) +b where (a,b) = properFraction d
--
gzd :: Int -> String
gzd z = if (z<10) then '0':(show z) else (show z)

--
wgs2mgrs :: [Double] -> (Int,String,String,Double,Double)
wgs2mgrs [lat,lon,h] = (z,(lat2grid lat),(xy2htm (z,x,y)),(rem5 x),(rem5 y))
                        where (z,x,y) = wgs2utm [lat,lon,h]

--
mgrs2wgs :: (Int,String,String,Double,Double) -> [Double]
mgrs2wgs (zz,b,gg,e,n) = utm2wgs (zz,x,y)
                            where x = eBase fst + e
                                  y = n+ htm2y (head b) (nBase (zone2set zz) snd)
                                  [fst,snd] = gg 

htm2y :: Char -> Double -> Double
htm2y h nbase
    | ((h == 'N') || (h == 'P')) = 0+nbase
    | (h == 'R') = 2000000+nbase
    | (h == 'T') = 4000000+nbase
    | ((h == 'V') || (h == 'W')) = 6000000+nbase
    | (h == 'Q') = if nbase > 1000000 then 0+nbase else 2000000+nbase
    | (h == 'S') = if nbase > 1000000 then 2000000+nbase else 4000000+nbase
    | (h == 'U') = if nbase > 1000000 then 4000000+nbase else 6000000+nbase
    | (h == 'X') = if nbase > 1500000 then 6000000+nbase else 8000000+nbase

eBaselist = [('A',100000),('B',200000),('C',300000),('D',400000),('E',500000),('F',600000),('G',700000),('H',800000),
             ('J',100000),('K',200000),('L',300000),('M',400000),('N',500000),('P',600000),('Q',700000),('R',800000),
             ('S',100000),('T',200000),('U',300000),('V',400000),('W',500000),('X',600000),('Y',700000),('Z',800000)]

eBase :: Char -> Double
eBase a = n where (Just n)=lookup a eBaselist

nBaselist = [
    [('F',0),('G',100000),('H',200000),('J',300000),('K',400000),('L',500000),('M',600000),('N',700000),('P',800000),
     ('Q',900000),('R',1000000),('S',1100000),('T',1200000),('U',1300000),('V',1400000),('A',1500000),('B',1600000),
     ('C',1700000),('D',1800000),('E',1900000)],
    [('A',0),('B',100000),('C',200000),('D',300000),('E',400000),('F',500000),('G',600000),('H',700000),('J',800000),
     ('K',900000),('L',1000000),('M',1100000),('N',1200000),('P',1300000),('Q',1400000),('R',1500000),('S',1600000),
     ('T',1700000),('U',1800000),('V',1900000)],
    [('F',0),('G',100000),('H',200000),('J',300000),('K',400000),('L',500000),('M',600000),('N',700000),('P',800000),
     ('Q',900000),('R',1000000),('S',1100000),('T',1200000),('U',1300000),('V',1400000),('A',1500000),('B',1600000),
     ('C',1700000),('D',1800000),('E',1900000)],
    [('A',0),('B',100000),('C',200000),('D',300000),('E',400000),('F',500000),('G',600000),('H',700000),('J',800000),
     ('K',900000),('L',1000000),('M',1100000),('N',1200000),('P',1300000),('Q',1400000),('R',1500000),('S',1600000),
     ('T',1700000),('U',1800000),('V',1900000)],
    [('F',0),('G',100000),('H',200000),('J',300000),('K',400000),('L',500000),('M',600000),('N',700000),('P',800000),
     ('Q',900000),('R',1000000),('S',1100000),('T',1200000),('U',1300000),('V',1400000),('A',1500000),('B',1600000),
     ('C',1700000),('D',1800000),('E',1900000)],
    [('A',0),('B',100000),('C',200000),('D',300000),('E',400000),('F',500000),('G',600000),('H',700000),('J',800000),
     ('K',900000),('L',1000000),('M',1100000),('N',1200000),('P',1300000),('Q',1400000),('R',1500000),('S',1600000),
     ('T',1700000),('U',1800000),('V',1900000)]]

nBase::Int -> Char -> Double
nBase s a = n where (Just n) = lookup a (nBaselist !! s)

