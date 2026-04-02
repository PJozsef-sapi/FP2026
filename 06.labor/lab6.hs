import Data.List

-- PART I

-- 1. Sum of numbers between two integers
sumInterval :: Integral a => a -> a -> a
sumInterval start end = sum [start..end]

-- 2. Sum of primes between two integers
realDivisors :: Integral a => a -> [a]
realDivisors n = [i | i <- [2..(div n 2)], mod n i == 0]

isPrime :: Integral a => a -> Bool
isPrime n
    | n < 2     = False
    | otherwise = null (realDivisors n)

primeSum :: Integral a => [a] -> a
primeSum ls = sum (filter isPrime ls)

--3. Sum of numbers with the most real divisors (not 1 and number) between two integers
mostRD :: Integral a => [a] -> [(a, Int)]
mostRD ls = filter (\(n, rd) -> rd == maxRD) ls2
    where
        ls2 = [(n, length $ realDivisors n) | n <- ls]
        maxRD = maximum $ map snd ls2

-- Main
main1 :: IO()
main1 = do
    putStr "x1? : "
    x1 <- readLn :: IO Int
    putStr "x2? : "
    x2 <- readLn :: IO Int
    let start = min x1 x2
        end = max x1 x2
    putStrLn ("Sum of integers between " ++ show start ++ " and " ++ show end ++ ": " ++ show (sumInterval start end))
    putStrLn ("Sum of prime numbers between " ++ show start ++ " and " ++ show end ++ ": " ++ show (primeSum [start..end]))
    putStrLn ("Sum of numbers with most divisors: " ++ show (sum . map fst $ mostRD [start..end]))

-- PART II

-- 1. Fibonacci numbers until n
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibsUntil :: Int -> [Int]
fibsUntil n
    | n <= 50 = []
    | otherwise = takeWhile (<= n) fibs

-- 2. Primes until n
primesUntil :: Int -> [Int]
primesUntil n = filter isPrime [2..n]

main2 :: IO()
main2 = do
    putStr "n? : "
    n <- readLn :: IO Int
    putStrLn $ "Fibonacci numbers until " ++ show n
    putStrLn $ unwords $ map show $ fibsUntil n
    putStrLn $ "Prime numbers until " ++ show n
    putStrLn $ unwords $ map show $ primesUntil n
