module Chapter2.BabyFirstFunctions where
import Data.Monoid

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



