module Conditional where

import qualified Test.QuickCheck as Q
import Data.List (minimum, sort)

checkMax :: Int -> Int -> Q.Property 
checkMax x y = x <= y Q.==> max x y == y

divideInverse :: Rational -> Rational -> Q.Property
divideInverse x y = y /= 0 Q.==> p * y == x
  where p = x / y

minElement :: Int -> [Int] -> Q.Property
minElement x xs = length xs >= 1 && x <= minimum xs Q.==> (x:(sort xs)) == sort (x:xs)


