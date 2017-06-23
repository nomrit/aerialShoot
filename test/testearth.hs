import Matrix
import Earth

--
eps = 1.0e-7

delta :: Double->Double->Double
delta a b = abs (a-b)

-- test of N
testN :: [Double] -> Bool
testN (lat:(_:(_:xs))) =
  (delta (Earth.n (Matrix.toR lat)) (head xs)) < eps


resultSetofN = [
  [0,0,0,6378137],
  [5,0,0,6378299.17458331],
  [10,0,0,6378780.8436645],
  [15,0,0,6379567.58203603],
  [20,0,0,6380635.80715708],
  [25,0,0,6381953.45717444],
  [30,0,0,6383480.91771629],
  [35,0,0,6385172.17492696],
  [40,0,0,6386976.16574968],
  [45,0,0,6388838.29017365],
  [50,0,0,6390702.04425636],
  [55,0,0,6392510.7274989],
  [60,0,0,6394209.17392684],
  [65,0,0,6395745.45337071],
  [70,0,0,6397072.48829356],
  [75,0,0,6398149.53236547],
  [80,0,0,6398943.46002912],
  [85,0,0,6399429.82159598],
  [90,0,0,6399593.62586402]]

-- test of M
testM :: [Double] -> Bool
testM (lat:(_:(_:xs))) =
  (delta (Earth.m (Matrix.toR lat)) (head xs)) < eps

resultSetOfM = [
  [0,0,0,6335439.32708388],
  [5,0,0,6335922.60614688],
  [10,0,0,6337358.12135533],
  [15,0,0,6339703.29885563],
  [20,0,0,6342888.4823063],
  [25,0,0,6346818.85856688],
  [30,0,0,6351377.1035842],
  [35,0,0,6356426.6958112],
  [40,0,0,6361815.82635334],
  [45,0,0,6367381.81556652],
  [50,0,0,6372955.92570951],
  [55,0,0,6378368.4395776],
  [60,0,0,6383453.857255],
  [65,0,0,6388056.04888572],
  [70,0,0,6392033.19238866],
  [75,0,0,6395262.32289099],
  [80,0,0,6397643.32651314],
  [85,0,0,6399102.22563801],
  [90,0,0,6399593.62586402]]
