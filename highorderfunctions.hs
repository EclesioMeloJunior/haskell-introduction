app :: (a -> b) -> a -> b
app f = f

add1 :: Int -> Int
add1 x = x + 1

myeven :: Int -> Bool 
myeven x = x `mod` 2 == 0

main :: IO()
main = do
  print $ app add1 3
  print $ (+ 1) 7
  print $ (+ 9) 9
  print $ (\x y z -> x + y + z) 1 2 3
  print $ app (/2) 9
  print $ map (\(x, y) -> x + y) [(1, 2), (3, 4)]
  
  let l = [9, 8, 7, 6, 5, 4] in
    print $ filter even l
