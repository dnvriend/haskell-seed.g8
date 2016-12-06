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

