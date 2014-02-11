module Mp1 where

--1. Factorial
fact 1 = 1
fact n = n * fact (n-1)


--2. GCD
gcd a b
   | a < b 	= Mp1.gcd b a   
   | b == 0	= a
   | otherwise  = Mp1.gcd b (a `mod` b)


--3. Calculator
--calc x
--   |
--   |

calcstep n
   | even n	= n / 2
   | otherwise	= n * 3 + 1

--4. Head
hd [] 	  = 0
hd (x:xs) = x


--5. Tail
t1 []	  = []
t1 (x:xs) = xs


--6. Take
mytake n [] 	   = []
mytake n (x:xs)
   | n > length xs = (x:xs)
   | n == 0	   = []
   | otherwise	   = x : mytake (n-1) xs


--7. Drop
mydrop n [] 	   = []
mydrop n (x:xs)
   | n > length xs = []
   | n == 0	   = x:xs
   | otherwise	   = mydrop (n-1) xs

--8. Reverse
--rev [] 	   	     	= []
--rev (x:xs)
--   | length (x:xs) == 1 = x
--   | length xs == 1  	= xs:x
--   | otherwise	     	= (rev xs) : x


--9. Append
--app [] [] = []
--app (x:xs) [] = x:xs
--app [] (x:xs) = x:xs

--10. Increment
inclist :: (Num a) => [a] -> [a]
inclist [] 	= []
inclist (x:xs)	= map (+ 1) (x:xs)

--11. Double
doublelist :: (Num a) => [a] -> [a]
doublelist []	  = []
doublelist (x:xs) = map (* 2) (x:xs)

--12. Sum
sumlist :: (Num a) => [a] -> a
sumlist [] = 0
sumlist (x:xs) = foldr (+) x (xs)

--13. Product
prodlist :: (Num a) => [a] -> a
prodlist [] = 1
prodlist (x:xs) = foldr (*) 1 (x:xs)

--14. Zip
--zip :: [t] -> [t1] -> [(t,t1)]
zip [] [] = []
zip xx yy
   | xx == []  = []
   | yy == []  = []
   | otherwise = (head xx, head yy):Mp1.zip (tail xx) (tail yy)

--15. Add Pairs
--addpairs xx yy
