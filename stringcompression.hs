-- import allows char to int conversion for TASK 8
import Data.Char(digitToInt)

-- TASK 1
chomp :: String -> String
chomp x = takeWhile (== head x) x

-- TASK 2
munch :: String -> String
munch = take 9 . chomp

-- TASK 3
runs :: String -> [String]
runs "" = []
runs xs = ys : runs (drop (length ys) xs)
        where ys = munch xs

-- TASK 4
encode :: String -> [(Char,Int)]
encode xs = [(head x, length x) | x <- runs xs]

-- TASK 5
flatten :: [(Char,Int)] -> String
flatten [] = []
flatten ((c,n):xs) = c : show n ++ flatten xs
--flatten (x:xs) = ((fst x) : (show (snd x) ++ flatten xs))

-- TASK 6
compress :: String -> String
compress = flatten . encode

-- TASK 7
decode :: [(Char,Int)] -> String
decode [] = ""
decode ((c,n):xs) = replicate n c ++ decode xs
-- decode (x:xs) = ((replicate (snd x) (fst x)) ++ decode xs)

-- TASK 8
expand :: String -> [(Char,Int)]
expand "" = []
expand (x:y:xs) =  (x, digitToInt y) : expand xs

-- TASK 9
decompress :: String -> String
decompress =  decode . expand