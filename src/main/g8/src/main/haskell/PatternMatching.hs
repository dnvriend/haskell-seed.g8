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



