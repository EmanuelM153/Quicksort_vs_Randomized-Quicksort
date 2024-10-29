import System.Random
import System.CPUTime
import Control.DeepSeq
import System.Environment
import qualified Data.DList as D
import Numeric

generateRandomListWithSeed :: Int -> Int -> [Int]
generateRandomListWithSeed n seed = take n (randoms (mkStdGen seed) :: [Int])

partition :: Ord a => [a] -> a -> ([a], [a])
partition xs pivot = (filter (< pivot) xs, filter (> pivot) xs)

qsort :: Ord a => [a] -> D.DList a
qsort [] = D.empty
qsort (p:xs) =
    let (lesser, greater) = partition xs p
    in D.append (qsort lesser) (D.cons p (qsort greater))

main :: IO ()
main = do
    args <- getArgs

    let conversionFactor = 10 ^^ (-12)
    let n = (read (head args) :: Int)
        nums = generateRandomListWithSeed n 2024

    start <- getCPUTime
    let sortedNums = qsort nums
    end <- sortedNums `deepseq` getCPUTime
    putStrLn $ Numeric.showFFloat Nothing (fromIntegral (end - start) * conversionFactor) ""
