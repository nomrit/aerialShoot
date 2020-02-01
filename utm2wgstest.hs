-- utm2wgstest
--
import System.IO
import Data.List.Split
import GaussCruger

list2tuple::[Double] -> (Int,Double,Double)
list2tuple [z,x,y] = (floor(z),x,y)

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
                     else hPutStrLn outh ("("++inStr ++ ")," ++ (show$GaussCruger.toD (utm2wgs$list2tuple (map (read::String->Double) (splitOn "," inStr)))))
                  mainloop inh outh
