import Data.List (group, sort)

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

-- 1. Get the amount of a number's divisors.

getdivisoramount :: Integral a => a -> Int
getdivisoramount n = length ([i | i <- [1..(div n 2)], mod n i == 0] ++ [n])

-- 2. Get the largest odd divisor of a number.

getLargestOddDivisor :: Integral a => a -> a
getLargestOddDivisor n
    | odd n = n
    | otherwise = head [i | i <- [div n 2, div n 2 - 1 .. 1], odd i && mod n i == 0]

-- 3. Get the amount of digits in a number on base p.

digitsInBase :: (Integral p, Num a) => p -> p -> a
digitsInBase base 0 = 1
digitsInBase base num = go num 0
    where
        go 0 acc = acc
        go x acc = go (div x base) (acc + 1)

-- 4. Get the Fibonacci numbers between a and b.

fibo :: (Ord a, Num a) => a -> a -> [a]
fibo a b = filter (\x -> x > a && x < b) (fibo2 0 1 0)
    where
        fibo2 a1 b1 res
            | res < b = res : fibo2 b1 res (res + b1)
            | otherwise = [res]

-- PART III

-- 1. Get the average of all positive numbers in a list.

positiveAVG :: (Floating a, Ord a) => [a] -> a
positiveAVG arr = avg . filter (> 0) $ arr
    where
        avg arr = sum arr / fromIntegral (length arr)

-- 2. Get the list that contains the original list's every nth element.

listN :: Integral a => [a] -> a -> [a]
listN ls n = [i | (idx, i) <- zip [1..] ls, mod i n == 0]

-- 3. Reverse each number in a list.

reverseList ls = map ((\x -> read x :: Int) . reverse . show)

-- 4. Gets the poistion of the largest element.

-- METHOD 1
maxElementPos :: (Num a1, Enum a1, Ord a2) => [a2] -> [a1]
maxElementPos ls = [idx | (idx, i) <- zip [1..] ls, i == myMax] where myMax = maximum ls

-- METHOD 2
maxElementPos2 :: (Ord a1, Num a2, Enum a2) => [a1] -> (a1, [a2])
maxElementPos2 (x:xs) = foldl aux (x, [0]) (zip xs [1..])
    where
        aux (currentMax, positions) (elem, i)
            | elem > currentMax = (elem, [i])
            | elem == currentMax = (elem, positions ++ [i])
            | otherwise = (currentMax, positions)

-- 5. Determine the most occurring element of a list.

elof ls = fst $ head $ filter (\x -> snd x == maxCount) ls2
  where
    grouped = (group . sort) ls
    ls2 = map (\x -> (head x, length x)) grouped
    maxCount = maximum $ map snd ls2