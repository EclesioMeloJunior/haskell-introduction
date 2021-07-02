import Data.List

asc ::Int -> Int -> [Int]
asc n m
  | m == n = [m]
  | m > n = n : asc (n + 1) m
  | otherwise = []

mul :: [Int] -> [Int]
mul l =
  [ 2 * x | x <- l]

listMul :: [Int] -> Int -> [Int]
listMul l m =
  [ m * i | i <- l ]

mysum :: [Int] -> Int
mysum [] = 0
mysum (x:xs) = x + mysum xs

myevens :: [Int] -> [Int]
myevens [] = []
myevens (x : xs)
  | mod x 2 == 0 = x : myevens xs
  | otherwise = myevens xs

main :: IO()
main = do
  print $ mul (asc 1 10)
  print $ listMul (asc 0 10) 9
  print $ mysum (asc 0 10)
  print $ myevens (asc 20 30)