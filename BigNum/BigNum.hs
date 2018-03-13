{-
  Name: Abhishek Manoj Sharma
  Class: CS 252
  Assigment: HW1
  Date: February 9, 2018
  Description: To add, subtract, multiply, calculate powers, and check equalities of big numbers using Haskell.
-}

module BigNum (
  BigNum,
  bigAdd,
  bigSubtract,
  bigMultiply,
  bigEq,
  bigDec,
  bigPowerOf,
  prettyPrint,
  stringToBigNum,
) where

type Block = Int -- An Int from 0-999

type BigNum = [Block]

maxblock = 1000

bigAdd :: BigNum -> BigNum -> BigNum
bigAdd x y = bigAdd' x y 0

bigAdd' :: BigNum -> BigNum -> Block -> BigNum
bigAdd' [] [] carry = if carry > 0 then [carry] else []
  
bigAdd' [n1] [n2] carry = stringToBigNum(show(n1+n2+carry))

bigAdd' (n1:n1s) (n2:n2s) carry = currentResult : (bigAdd' n1s n2s currentCarry)
  where currentResult = (n1+n2+carry) `mod` maxblock
        currentCarry = if (n1+n2+carry)>999 then 1 else 0

bigAdd' (n1:n1s) [] carry = currentResult : (bigAdd' n1s [] currentCarry)
  where currentResult = (n1+0+carry) `mod` maxblock
        currentCarry = if (n1+0+carry)>999 then 1 else 0

bigAdd' [] (n2:n2s) carry = currentResult : (bigAdd' [] n2s currentCarry)
  where currentResult = (0+n2+carry) `mod` maxblock
        currentCarry = if (0+n2+carry)>999 then 1 else 0

bigSubtract :: BigNum -> BigNum -> BigNum
bigSubtract x y =
  if length x < length y
    then error "Negative numbers not supported"
    else reverse $ stripLeadingZeroes $ reverse result
      where result = bigSubtract' x y 0

stripLeadingZeroes :: BigNum -> BigNum
stripLeadingZeroes (0:[]) = [0]
stripLeadingZeroes (0:xs) = stripLeadingZeroes xs
stripLeadingZeroes xs = xs

-- Negative numbers are not supported, so you may throw an error in these cases
bigSubtract' :: BigNum -> BigNum -> Block -> BigNum
bigSubtract' [] [] borrow = if borrow>0 then error "Subtract result is a negative number. Not supported." else []

bigSubtract' (n1:n1s) (n2:n2s) borrow = if (n1-n2)-borrow<0 then (maxblock+(n1-n2)-borrow) : (bigSubtract' n1s n2s 1)
                                          else ((n1-n2)-borrow) : (bigSubtract' n1s n2s 0)
bigSubtract' (n1:n1s) [] borrow = (n1-borrow) : bigSubtract' n1s [] 0

bigEq :: BigNum -> BigNum -> Bool
bigEq n1 n2 = if n1==n2 then True else False

bigDec :: BigNum -> BigNum
bigDec x = bigSubtract x [1]

bigMultiply :: BigNum -> BigNum -> BigNum
bigMultiply _ [0] = [0]
bigMultiply [0] _ = [0]
bigMultiply _ [] = [0]
bigMultiply [] _ = [0]
bigMultiply n1 n2 = bigAdd n1 (bigMultiply n1 (bigDec n2))

bigPowerOf :: BigNum -> BigNum -> BigNum
bigPowerOf _ [0] = [1]
bigPowerOf [0] _ = [0]
bigPowerOf _ [] = [1]
bigPowerOf [] _ = [1]
bigPowerOf n1 n2 = bigMultiply n1 (bigPowerOf n1 (bigDec n2))

prettyPrint :: BigNum -> String
prettyPrint [] = ""
prettyPrint xs = show first ++ prettyPrint' rest
  where (first:rest) = reverse xs

prettyPrint' :: BigNum -> String
prettyPrint' [] = ""
prettyPrint' (x:xs) = prettyPrintBlock x ++ prettyPrint' xs

prettyPrintBlock :: Int -> String
prettyPrintBlock x | x < 10     = ",00" ++ show x
                   | x < 100    = ",0" ++ show x
                   | otherwise  = "," ++ show x

stringToBigNum :: String -> BigNum
stringToBigNum "0" = [0]
stringToBigNum s = stringToBigNum' $ reverse s

stringToBigNum' :: String -> BigNum
stringToBigNum' [] = []
stringToBigNum' s | length s <= 3 = read (reverse s) : []
stringToBigNum' (a:b:c:rest) = block : stringToBigNum' rest
  where block = read $ c:b:a:[]