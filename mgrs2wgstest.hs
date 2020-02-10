-- mgrs2wgstest
--
import System.IO
import Data.List.Split
import GaussCruger
import Mgrs

list2tuple::[String] -> (Int,String,String,Double,Double)
list2tuple [zz,b,gg,e,n] = (((read::String->Int) zz),b,gg,((read::String->Double) e),((read::String->Double) n))

round7 :: Double -> Double
round7 x = (fromIntegral(round (x*10^7))::Double)/10^7

main = do
       mainloop stdin stdout

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh =
    do ineof <- hIsEOF inh
       if ineof
          then return ()
          else do inStr <- hGetLine inh
                  if (head inStr == '#')
                     then hPutStrLn outh inStr
                     else hPutStrLn outh ("("++inStr ++ ")," ++ (show$GaussCruger.toD (mgrs2wgs$list2tuple (splitOn "," inStr))))
                  mainloop inh outh
