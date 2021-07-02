addTuples :: [(Int, Int)] -> [Int]
addTuples t = [ x + y | (x, y) <- t ]

main :: IO()
main = do
  print $ addTuples [(1, 2), (3, 4)]
