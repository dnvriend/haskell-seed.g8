# haskell-seed.g8
A Haskell template project.

# Introduction
A very simple and small sbt project that compiles/test Haskell source files which is useful for learning Haskell. Please see the [sbt-haskell-plugin](https://github.com/dnvriend/sbt-haskell) for more information.

# Usage
You need at least sbt v0.13.13 and then type:

```
sbt new dnvriend/haskell-seed.g8
```

- Install the [IntelliJ plugin for Haskell](https://plugins.jetbrains.com/plugin/7453)
- Put your files in `src/main/haskell`
- Be sure to start out with a `Main.hs` file
- use sbt so launch the sbt console
- type: `~haskellTest` so it watches source files for triggered execution
- see the Haskell output on the console
- Have fun learning Haskell and FP using the workflow/tools you already know; IntelliJ and SBT!

## Installing Haskell
I successfully installed Haskell using [brew](http://brew.sh/), so if you don't have brew installed yet, click on the
link and follow the instructions:

```
brew install ghc cabal-install
brew link ghc
brew link --overwrite ghc
cabal update
cabal install ghc-mod
# to plot graphics
brew cask install aquaterm
brew install gnuplot
cabal install easyplot
```

## Learning Haskell Resources
The best resource when just starting with Haskell is [Learn you a Haskell for great good](http://learnyouahaskell.com/chapters). Another great place to learn is [Happy Learn Haskell Tutorial](http://www.happylearnhaskelltutorial.com/contents.html) and when you are done with those books you should take a look at more serious books like [Programming in Haskell 2nd Edition by Graham Hutton](https://www.amazon.com/Programming-Haskell-Graham-Hutton/dp/1316626229/ref=dp_ob_image_bk), [Learn Haskell by Will Kurt Manning EAP](https://www.manning.com/books/learn-haskell) and [Real World Haskell by Brian O' Sullivan](https://www.amazon.com/Real-World-Haskell-Bryan-OSullivan/dp/0596514980/ref=sr_1_1?s=books&ie=UTF8&qid=1481042542&sr=1-1&keywords=real+world+haskell). Of course you should take a look at [The Haskell Wiki - Tutorials section](https://wiki.haskell.org/Tutorials). The [Haskell Wiki](https://wiki.haskell.org/Haskell) contains everything for the [Haskell Programming Language](https://www.haskell.org/platform/) and where you're hooked, you will never go back. It is even possible to interoperate with Java by means of [inline-java](http://blog.tweag.io/posts/2016-10-17-inline-java.html) something you could look into. 

## Example
Create two files `Main.hs` and `PatternMatching.hs` in `src/main/haskell` and put the following code in it:

__Main.hs__:

```haskell
module Main where
import Data.Monoid
import PatternMatching

doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100 then x else doubleMe x

concat' :: (Enum a, Num a) => [a] -> [a] -> [a]
concat' xs ys = xs `Data.Monoid.mappend` ys

length' :: (Enum a, Num a) => [a] -> a
length' xs = sum [1 | _ <- xs]

doubled :: (Enum a, Num a) => a -> [a]
doubled n = [x*2 | x <- [1..n]]

-- output
echo :: String -> String -> IO ()
echo msg_one msg_two = putStrLn $ msg_one `Data.Monoid.mappend` msg_two

triangles :: (Eq a, Enum a, Num a) => a -> [(a, a, a)]
triangles n = [(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10], a^2 + b^2 == c^2, a + b + c == n]

main :: IO ()
main = do
       echo "doubleMeeeee 5: " $ show $ doubleMe 5
       echo "doubleSmallNumber 100: " $ show $ doubleSmallNumber 100
       echo "doubleSmallNumber 101: " $ show $ doubleSmallNumber 101
       echo "concat' [1,2] [3,4]: " $ show $ concat' [1,2] [3,4]
       echo "length' [1,2,3]: " $ show $ length' [1,2,3]
       echo "doubled 20: " $ show $ doubled 20
       echo "fst (8, 11): " $ show $ fst (8,11)
       echo "snd (8, 11): " $ show $ snd (8,11)
       echo "zip [1..3] [\"One\",\"Two\",\"Three\"]: " $ show $ zip [1..] ["One", "Two", "Three"]
       echo "unzip [(1,\"One\"), (2, \"Two\"), (3, \"Three\")]: " $ show $ unzip [(1,"One"), (2, "Two"), (3, "Three")]
       echo "triangles: " $ show $ triangles 24
       echo "lucky 7: " $ lucky 7
       echo "lucky 5: " $ lucky 5
       echo "factorial 5: " $ show $ factorial 5
       echo "charName 'a'  " $ charName 'a'
       echo "charName 'b'  " $ charName 'b'
       echo "charName 'c'  " $ charName 'c'
       echo "charName 'h'  " $ charName 'h'
       echo "addTuples [(1,2), (3,4)]: " $ show $ addTuples [(1,2), (3,4)]
       echo "head' [1,2,3]: " $ show $ head' [1,2,3]
       echo "head' \"hello\": " $ show $ head' "hello"
```

__PatternMatching.hs__:

```haskell
-- see: http://learnyouahaskell.com/syntax-in-functions#pattern-matching

module PatternMatching where
import Data.Monoid

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number seven!"
lucky x = "Sorry, you're out of luck pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
charName char = "Unexpected input " `Data.Monoid.mappend` show char

addTuples :: (Enum a, Num a) => [(a, a)] -> [a]
addTuples xs = [a + b | (a, b) <- xs]

-- returns the head of a list
head' :: (Enum a) => [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x : _) = x
```

Have fun!