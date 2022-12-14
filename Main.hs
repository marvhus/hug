import System.Random.PCG 
import Control.Monad.ST
import Data.Char

randomFromSeed :: Int -> Int -> Int -> Int
randomFromSeed low high seed = runST $ do
  g <- initialize (fromIntegral seed) 0
  uniformR (low, high) g

getNums :: Int -> Int -> [Int]
getNums len seed = map (randomFromSeed 97 122) [start..end]
  where
    start = seed
    end = start + len

ints2Chars :: [Int] -> [Char]
ints2Chars nums = map (\x -> chr x) nums

genUsername :: Int -> Int -> String
genUsername len seedOffset = ints2Chars $ getNums len seedOffset

offsets :: [Int]
offsets = map (\x -> x*x) [1..]

usernames :: Int -> Int -> Int -> IO ()
usernames x count len = print $ map (genUsername (len-1)) $ drop x $ take y offsets
  where
    y = x + count
