# QuickCheckTutorial
Hello and welcome to the [QuickCheck](http://hackage.haskell.org/package/QuickCheck) tutorial. This project will present the introduction to QuickCheck from basic stuff to the more advanced. We will be covering QuickCheck 2.16. version so if you have some other version be aware that some parts of code might not work.
# Install process   
First of all, you have to install QuickCheck and that can be easily done with the following command.
```
cabal install QuickCheck
```
After that, you can import QuickCheck into Haskell using `import qualified Test.QuickCheck as Q`.

# Introduction
First of all, I assume that you don't know anything about **QuickCheck**, so **QuickCheck** is a really awesome library used for testing. It is one of the most popular Haskell libraries and part of the reason [why functional programming has mattered](https://academic.oup.com/nsr/article/2/3/349/1427872/How-functional-programming-mattered).
In short, we want to express some property and **QuickCheck** try to prove it wrong. For example:
```
isSumCommutative :: Int -> Int -> Bool
isSumCommutative a b = a + b == b + a
```
So we express some property with a function which always return bool value. It should return always `True`, but if doesn't QuickCheck will warn you about that error. So how can we test if function `isSumCommutative` or the property which is the function expressing always `True`? By using the following code sample.
```
Q.quickCheck isSumCommutative
```
That should exit with the following message: `+++ OK, passed 100 tests.` If you get that message great, you just did your first test using **QuickCheck**, if not please consider retaking the previous steps again or contact me.
If we would use this function we would get:
```
absIsNothing :: Int -> Bool
absIsNothing x = abs x == x  -- or point free ap (==) abs
```
The command `Q.quickCheck absIsNothing` you should get the message something like this:
```
*** Failed! Falsifiable (after 2 tests):
-1
```
The test shouldn't be the same because **QuickCheck** generates random tests on which will try to disapprove your property.

# Step two
Ok, we saw how **QuickCheck** works on really simple function. Now let's try to take it further. As we said above **QuickCheck** generates random test cases, 100 of them is the default value. The first question should be how does the QuickCheck generate those random tests. To generate a random value of type a, we need a generator for values of that type: `Gen a`. The default generator for values of any type is `arbitrary`, which is a method of QuickCheck's Arbitrary type class. So generate function looks like this:
```
generate :: Gen a -> IO a
```
We can see it on a few examples:
```
Prelude Q> Q.generate Q.arbitrary :: IO Int
-6
Prelude Q> Q.generate Q.arbitrary :: IO Double
-6.770001696938178
Prelude Q> Q.generate Q.arbitrary :: IO [Int]
[-14,-27,-28,-6,-14]
Prelude Q> Q.generate Q.arbitrary :: IO [Maybe Bool]
[Nothing,Just True,Just True,Just True]
Prelude Q> Q.generate Q.arbitrary :: IO (Either Int Char)
Right '('
```
---
QuickCheck also providers us with several functions that we can use to generate random values in our own instances of **Arbitrary**.  For example:
```
choose :: Random a => (a, a) -> Gen a
```
And if we want to dice a cube, we have:
```
dice :: Gen Int
dice = choose (1, 6)
```
So, if you want to roll it just call `generate dice`. Because dice is a generator of the random values and `generate` takes it and transform it into `IO` monad.  
# Choose my day
For example, we have a data structure like this:
```
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
```
If we want to make a similar function like `dice` above we can try something like this:
```
Prelude Q> Q.choose (Monday, Friday)
<interactive>:23:89: error:
    • Can't make a derived instance of ‘System.Random.Random Day’:
        ‘System.Random.Random’ is not a stock derivable class (Eq, Show, etc.)
        Try enabling DeriveAnyClass
    • In the data declaration for ‘Day’
```
You probably can see the reason why the upper code produced an error.  We had to make  the following code: 
```
import qualified Test.QuickCheck as Q
import System.Random

data Day = Monday | Tuesday | Wendsday | Thursday | Friday | Saturday | Sunday deriving (Show, Enum, Bounded)

instance Random Day where
  randomR (a,b) g = (toEnum $ fst $ randomR (n1, n2) g, g)
    where n1 = fromEnum a
          n2 = fromEnum b
  random g = randomR (minBound, maxBound) g


getDay :: IO Day 
getDay = Q.generate $ Q.choose (Monday, Friday)
```
We have to define functions `randomR` and `random` which works like the setting the distribution probability of your data. In the upper case you can see that we use uniform distribution. 

# Another useful function
One also usefull function from **QuickCheck** is `sized` which can be used to construct generators that depend on a size parameter.
```
sized :: (Int -> Gen a) -> Gen a
```
So, let's try to generate a random list using the `sized` function. 
```
randomList :: Arbitrary a => Gen [a]
randomList = sized $ (\n -> do
                         day <- getDay
                         sequence [ arbitrary | 
```


