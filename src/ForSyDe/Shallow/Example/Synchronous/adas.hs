import ForSyDe.Shallow

lidar :: Signal Integer
lidar = signal [1..10]
camera :: Signal Integer
camera = signal [2,4..20]

adasproc :: Signal Integer -> Signal Integer -> Signal Integer
adasproc = zipWithSY processor where
    proc1 x y     = x + y
    proc2         = (* 2)
    processor x y = proc2 (proc1 x y) 

brake :: Integer -> Signal Integer -> Signal Bool
brake th = mapSY (> th)

monitor :: Signal Integer -> Signal String
monitor = mapSY (\x -> if x>5 then "DANGER!" 
                  else if x>3 then "ALARM" 
                  else show x)