module DayChoose where

import qualified Test.QuickCheck as Q
import System.Random

data Day = Monday | Tuesday | Wendsday | Thursday | Friday | Saturday | Sunday deriving (Show, Eq, Ord, Enum, Bounded)

instance Random Day where
  randomR (a,b) g = (toEnum $ fst $ randomR (n1, n2) g , g)
    where n1 = fromEnum a
          n2 = fromEnum b
  random g = randomR (minBound, maxBound) g


getDay :: IO Day 
getDay = Q.generate $ Q.choose (Monday, Friday)

getDay2 :: IO Day
getDay2 = Q.generate $ Q.oneof $ map return [Monday .. Friday]
