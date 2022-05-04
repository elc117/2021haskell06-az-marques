ends :: [Int] -> [Int]
ends (x:xs) = x:last xs:[]

deduzame :: [Integer] -> [Integer]
deduzame [] = []
deduzame (x:xs) = (2 * x) : deduzame (xs)

deduzame2 :: [Integer] -> [Integer]
deduzame2 [] = []
deduzame2 (x:xs) = if x > 2
  then x : deduzame2 xs
  else deduzame2 xs

geraTabela :: Int -> [(Int,Int)]
geraTabela 0 = []
geraTabela n = (n, n^2) : geraTabela (n-1)

contido :: Char -> String -> Bool
contido c [] = False
contido c (x:xs) = if c == x
  then True
  else contido c xs

translate :: [(Float, Float)] -> [(Float, Float)]
translate [] = []
translate ((x,y):xs) = (x+2.0,y+2.0) : translate xs

countLongs :: [String] -> Int
countLongs [] = 0
countLongs (x:xs) = if length x > 5
  then 1 + countLongs xs
  else countLongs xs

onlyLongs :: [String] -> [String]
onlyLongs [] = []
onlyLongs (x:xs) = if length x > 5
  then x : onlyLongs xs
  else onlyLongs xs