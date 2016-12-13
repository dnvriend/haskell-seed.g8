module Main where
import Data.Monoid
import Chapter2.BabyFirstFunctions

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