getMax :: [Int] -> Maybe Int
getMax [] = Nothing
getMax x = Just $ maximum(x)



reciprocal :: (Eq a, Fractional a) => a -> Maybe a
reciprocal 0 = Nothing
reciprocal x = Just $ 1 / x



rectangleArea :: Int -> Int -> Either String Int
rectangleArea x y = if x<0 then Left "Width is not positive" else if y<0 then Left "Height is not positive" else Right $ x * y


main :: IO ()
main = do
  print $ getMax []
  print $ getMax [99,12,37]
  print $ getMax [-99,-12,-37]
  print $ reciprocal 4
  print $ reciprocal 2
  print $ reciprocal 0
  print $ rectangleArea 5 10
  print $ rectangleArea (-5) 10
  print $ rectangleArea 5 (-10)