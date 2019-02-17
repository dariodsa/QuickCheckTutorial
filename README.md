# QuickCheckTutorial
Hello and welcome to the [QuickCheck](http://hackage.haskell.org/package/QuickCheck) tutorial. This project will present introduction to QuickCheck from basic stuff to the more advandce.I will be covering QuickCheck 2.16. version so if you have some other version be aware that some parts of code might not work.      
# Install process   
First of all you have to install QuickCheck and that can be easily done with following command.  
```
cabal install QuickCheck
```
After that you can import QuickCheck into Haskell using `import qualified Test.QuickCheck as Q`.    

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
That should exit with the following message: `+++ OK, passed 100 tests.` If you get that message great, you just did your first test using **QuickCheck**, if not please consider retaking the previuos steps again or contacing me.
If we would using this function:    
```
absIsNothing :: Int -> Bool
absIsNothing x = abs x == x  -- or point free ap (==) abs
```
The command `Q.quickCheck absIsNothing` you should get message something like this:   
```
*** Failed! Falsifiable (after 2 tests):               
-1
```    
The test shouldn't be the same because **QuickCheck** generates random tests on which will try to disapprove your property. 
