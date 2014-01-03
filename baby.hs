doubleMe x = x + x
doubleUs x y = x*2+y*2

factorial x = if x < 2 then 1 else (x * (factorial (x - 1)))


replicater2 :: (Num a, Ord a) => a -> b -> [b]
replicater2 num rep
	   | num < 0 = error "can't replicate negative times."
	   | num == 0 = []
	   | num > 0 = rep:replicater2 (num-1) rep

replicater :: (Num a, Ord a) => a -> b -> [b]  
replicater num rep
	   | num <= 0    = []  
	   | otherwise = rep:replicater (num-1) rep  

taker :: (Num a, Ord a) => a -> [b] -> [b]
taker 0 [] = []
taker _ [] = error "taking more than list"
taker num (l:ls)
      | num < 0 = error "negative size list"
      | num == 0 = []
      | num > 0 = l:taker (num-1) ls

reverser :: [a] -> [a]
reverser [] = []
reverser (b:bs) = reverser bs ++ [b]

repeat' :: a -> [a]  
repeat' x = x:repeat' x 


elem2 :: (Eq a) => a -> [a] -> Bool
elem2 _ [] = False
elem2 test (x:xs) 
      | test == x = True
      | otherwise = elem2 test xs


quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

qs :: (Ord a) => [a] -> [a]
qs [] = []
qs (x:xs) = 
   let lte = qs [a | a <- xs, a <= x]
       gt  = qs [a | a <- xs, a >  x]
   in lte ++ [x] ++ gt

divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)

flip2 :: (a -> b -> c) -> (b -> a -> c)
flip2 f x y = f y x

sosut :: (Integral a) => a
sosut = sum (filter p (map (^2) [1..10000]))
      where p x = x < 10000 && x `mod` 2 ==1

chain :: (Integral a) => a -> [a]
chain x
      | x < 1 = error "Can't chain non-positive numbers"
      | x == 1 = [1]
      | odd x = x : chain((x * 3) + 1)
      | otherwise = x : chain (x `div` 2)

fsnch :: Int
fsnch = length (filter p (map chain [1..100]))
      where p xs = length xs > 15

elem3 :: (Eq a) => a -> [a] -> Bool
elem3 item xs = foldl (\p x -> p || item == x) False xs

max2 :: (Ord a) => [a] -> a
max2 = foldr1 (\x acc -> if x > acc then x else acc)

maxl :: (Ord a) => [a] -> a  
maxl = foldr1 (\x acc -> if x > acc then x else acc)

