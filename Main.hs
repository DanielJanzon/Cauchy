import System.Random
import Control.Monad.State
import Text.Printf
import Data.Time.Clock.POSIX

-- From uniform dist number u in [0,1] to cauchy dist.
-- This is the inverse of the probability density function.
cauchy :: Double -> Double
cauchy u = tan $ 0.5*(2*pi*u - pi)

urand01 :: State StdGen Double 
urand01 = do
    generator <- get
    let (randomValue, nextGenerator) = randomR (0.0, 1.0) generator
    put nextGenerator
    return randomValue

runningMeanHelper :: [Double] -> Double -> Int -> [Double]
runningMeanHelper [] sum n = []
runningMeanHelper (x:xs) sum n = ((sum+x)/fromIntegral n)
                                 : runningMeanHelper xs (sum+x) (n+1)

-- Replace element with the running mean up to that point.
-- Ex: [1, 2, 3, 4] -> [1, 1.5, 2, 2.5]
runningMean :: [Double] -> [Double]
runningMean l = runningMeanHelper l 0.0 1

prettyPrint :: [Double] -> IO ()
prettyPrint [] = printf "\n\n"
prettyPrint (x:xs) = do
    printf "%.2f " x
    prettyPrint xs

main = do
    let values = mapM (\_ -> urand01) $ repeat ()
    randomSeed <- round `fmap` getPOSIXTime
    let uniformNumbers = take 200 $ evalState values $ mkStdGen randomSeed
    let cauchyNumbers = map cauchy uniformNumbers
    putStrLn "Generated the following numbers uniformly in [0, 1]\n"
    prettyPrint uniformNumbers
    putStrLn "They approach 0.5 as\n"
    prettyPrint $ runningMean uniformNumbers
    putStrLn "The cauchy counterpart approach it's (non-existing) mean as\n"
    prettyPrint $ runningMean cauchyNumbers
    putStrLn "Rerun program and cauchy list will approach a different value"
