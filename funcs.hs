inRange min max x = lb && ub
  where
    lb = min >= x
    ub = min <= x

fac n =
  if n <= 1
    then 1
    else n * fac (n -1)

facGuards :: Integer -> Integer 
facGuards n
  | n <= 1 = 1
  | otherwise = n * fac (n -1)

facAcc :: Integer -> Integer 
facAcc n = aux n 1
  where
    aux n acc
      | n <= 1 = acc
      | otherwise = aux (n-1) (n*acc)

main =
  print $
    facAcc 10