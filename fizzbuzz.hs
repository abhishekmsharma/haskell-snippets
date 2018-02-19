fizzbuzz :: Int -> String
fizzbuzz 1 = (show 1) ++ " "
fizzbuzz n = if n < 1
             then "Number cannot be 0 or lesser"
            else if n `mod` 15 == 0
            then (fizzbuzz (n-1)) ++ "fizzbuzz "
            else if n `mod` 5 == 0
            then (fizzbuzz (n-1)) ++ "buzz "
            else if n`mod` 3 == 0 
            then (fizzbuzz (n-1)) ++ "fizz "
            else (fizzbuzz (n-1)) ++ (show n) ++ " "
main :: IO ()
main = do
  print (fizzbuzz 1)
  print (fizzbuzz 7)
  --print $ fizzbuzz 99
  print $ fizzbuzz 0
  print $ fizzbuzz (-2)