-- PART I

-- 1. Find the squares of the first n even numbers.

f1 :: Integral a => a -> [a]
f1 n = [(i*2)^2 | i <- [1..n]]

-- 2. Create the following sequence: [1 r 1, 2 r 2, ..., n r n] where r n = repeated n times

f2 :: Int -> [Int]
f2 n = concatMap (\x -> replicate x x) [1..n]

-- 3. Create the following sequence: [2 r 1, 4 r 2, ..., 2n r n] where r n = repeated n times

f3 :: Int -> [Int]
f3 n = concatMap (\x -> replicate x (x*2)) [1..n]

-- 4. Create the following sequence: [n, n-1, ..., 2, 1, 1, 2, ..., n-1, n]

f4 :: Int -> [Int]
f4 n = reverse [1..n] ++ [1..n]

-- 5. Create the following sequence: [True, False, True, False, ...]

f5 :: Int -> [Bool]
f5 n = take n (map even [0..])

-- 6. Create the following sequence: [0, 1, -1, 0, 1, -1, ...]

f6 :: Int -> [Int]
f6 n = take n (map (\x -> mod x 3 - 1) [1..])

-- PART II

getdivisors :: Integral a => a -> [a]
getdivisors n = [i | i <- [1..(div n 2)], mod n i == 0] ++ [n]