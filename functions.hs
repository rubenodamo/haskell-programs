-- TASK 1
-- a
second1 :: [a] -> a
second1 xs = head(tail xs)
-- b
second2 :: [a] -> a 
second2 xs = xs !! 1
-- c
second3 :: [a] -> a 
second3 (_:x:_) = x

-- TASK 2
-- a
xor1 :: Bool -> Bool -> Bool
xor1 False True = True
xor1 True False = True
xor1 _ _ = False
-- b 
xor2 :: Bool -> Bool -> Bool
xor2 x y = 
    if x then
        if y then False else True 
    else
        y
--c
xor3 :: Bool -> Bool -> Bool
xor3 = (/=)

-- TASK 3
sumsqr :: Int -> Int 
sumsqr n = sum[x^2 | x <- [1..n]]

-- TASK 4
grid :: Int -> [(Int,Int)] 
grid n = [(x,y) | x <- [0..n], y <- [0..n], x /= y]

-- TASK 5 
euclid :: Int -> Int -> Int 
euclid x y | x == y = x
           | x < y = euclid x (y-x)
           | otherwise = euclid (x-y) y

-- TASK 6
fastrev :: [a] -> [a]
fastrev xs = rev xs []

rev :: [a] -> [a] -> [a]
rev []     ys = ys
rev (x:xs) ys = rev xs (x:ys)