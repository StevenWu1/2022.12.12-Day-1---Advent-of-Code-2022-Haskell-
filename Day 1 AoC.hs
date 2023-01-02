import System.IO
import Data.List
help1 :: [String] -> [Int]
help1 = map read
breaker :: [String] -> [String] -> [[String]]
breaker [] ys = [ys]
breaker ("":xs) ys = ys : breaker xs []
breaker (x:xs) ys = breaker xs (ys ++ [x])

main = do
  x1 <- readFile "data.txt"
  let x2 = lines x1
  let d1 = maximum (map (sum.help1) (breaker x2 []))
  print d1
  let d2 = sum (take 3(reverse(sort(map (sum.help1) (breaker x2 [])))))
  print d2
  putStrLn "Happiness"
