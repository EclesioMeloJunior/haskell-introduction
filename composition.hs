import Data.List
main :: IO()

-- (.) :: (b -> c) -> (a -> b) -> a -> c    
-- (f . g) equiv. to (\x -> f (g x))

ascOrd :: Int -> Int -> [Int]
ascOrd x y 
  | x == y = [y]
  | x < y = x : ascOrd (x + 1) y
  | otherwise = []

reverserOrder = reverse . Data.List.sort

-- maps of maps (2D arrays)

doublearrays xs = map (\ys -> map (\y -> y * 2) ys) xs
anotherdoublearrays = map . map

addone :: Int -> [Int] -> [Int]
addone x xs = map (\y -> y + 1) $ filter (\y -> y >= x) xs 

main = do 
  print "Composition"

  let l = ascOrd 0 20 in
    print $ reverserOrder l

  let b = [[1, 3, 4, 5], [1, 2, 3]] in
    print $ doublearrays b

  let c = [[1, 2, 3], [4, 5, 6]] in
    print $ anotherdoublearrays (\x -> x * 2) c

  let d = [1, 2, 3, 4, 5, 6, 7] in
    -- add 1 to numbers greater or equal to 4
    print $ addone 4 d
