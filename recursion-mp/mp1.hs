module Mp1 where

fact 0 = 0
fact 1 = 1
fact n = n * fact (n-1)

gcd a b
   | a < b 	= Mp1.gcd b a   
   | b == 0	= a
   | otherwise  = Mp1.gcd b (a `mod` b)

--calc x
--   |
--   |

calcstep n
   | even n	= n / 2
   | otherwise	= n * 3 + 1

hd [] 	  = 0
hd (x:xs) = x

t1 []	  = []
t1 (x:xs) = xs

mytake n [] 	   = []
mytake n (x:xs)
   | n > length xs = (x:xs)
   | n == 0	   = []
   | otherwise	   = x : mytake (n-1) xs

mydrop n [] 	   = []
mydrop n (x:xs)
   | n > length xs = []
   | n == 0	   = x:xs
   | otherwise	   = mydrop (n-1) xs

--rev [] = []
--rev (x:xs) = 

--app xx yy
