exists :: (Eq e) => e -> [e] -> Bool
exists _ [] = False
exists e (x : xs) = (x == e) || exists e xs

asc :: Int -> Int -> [Int]
asc f t 
  | f == t = [t]
  | f < t = f : asc (f+1) t
  | otherwise = []

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) 
  | x `exists` xs = removeDuplicates xs
  | otherwise = x : removeDuplicates xs

isascending :: [Int] -> Bool
isascending [] = True 
isascending [_] = True
isascending (x:y:xs) = (x <= y) && isascending(y : xs)

haspath :: (Eq a) => [(a, a)] -> a -> a -> Bool
haspath [] _ _ = False
haspath ((x, y) : xs) from to
  | (x == from) && (y == to) = True 
  | otherwise = 
    let xs' = [ (n, m) | (n, m) <- xs, n /= x ] in
      or [ haspath xs' m to | (n, m) <- xs, n == from ]

main :: IO()
main = do
  let a = asc 8 21
  let g = [(1, 2), (2, 3), (3, 2), (4, 3), (4, 5)]

  print $ elem "c" ["a", "b", "c"]
  print $ exists 9 a
  print $ removeDuplicates [1,2,3,3,2,1,10,9,8,10,3]
  print "Is Ascending" 
  print $ isascending (asc 0 10)
  print "Has path from 4 -> 2"
  print $ haspath g 4 2

  print "Has path from 4 -> 1"
  print $ haspath g 4 1

  print "Has path from 0 -> 0"
  print $ haspath g 0 0