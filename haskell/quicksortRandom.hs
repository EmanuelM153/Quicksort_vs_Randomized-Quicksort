import System.Random
import Text.Printf
import System.CPUTime
import Control.DeepSeq
import System.Environment
import qualified Data.DList as D
import Numeric

generateRandomListWithSeed :: Int -> Int -> [Int]
generateRandomListWithSeed n seed = take n (randoms (mkStdGen seed) :: [Int])

partition :: Ord a => [a] -> a -> ([a], [a])
partition xs pivot = (filter (< pivot) xs, filter (> pivot) xs)

qsortRandomized :: (Ord a, RandomGen g) => g -> [a] -> D.DList a
qsortRandomized _ [] = D.empty
qsortRandomized gen xs =
    let (pivotIndex, nextGen) = randomR (0, length xs - 1) gen
        pivot = xs !! pivotIndex
        (lesser, greater) = partition xs pivot
        (genLeft, genRight) = split nextGen
    in D.append (qsortRandomized genLeft lesser) (D.cons pivot (qsortRandomized genRight greater))

main :: IO ()
main = do
    args <- getArgs

    let conversionFactor = 10 ^^ (-12)
    let n = (read (head args) :: Int)
        nums = generateRandomListWithSeed n 2024

    startRandom <- getCPUTime
    let sortedNumsRandom = qsortRandomized (mkStdGen 1024) nums
    endRandom <- sortedNumsRandom `deepseq` getCPUTime
    putStrLn $ Numeric.showFFloat Nothing (fromIntegral (endRandom - startRandom) * conversionFactor) ""
