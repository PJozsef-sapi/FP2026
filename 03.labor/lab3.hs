-- PART I | OBSERVATIONS are uh. not here

atlag :: (Floating a) => [a] -> a
atlag ls = sum ls / fromIntegral (length ls)

{-
    PART II + III
    Map tests can be found below the functions they test as t# .
    Each exercise is numbered.
-}

-- 1. Determine the length of an array.

-- METHOD 1

myLength1 :: (Num a, Integral b) => [a] -> b
myLength1 [] = 0
myLength1 (_:xs) = 1 + myLength1 xs

-- METHOD 2

myLength2 :: (Num a, Integral b) => [a] -> b
myLength2 xs = helper xs 0
    where
        helper [] res = res
        helper (_:xs) res = helper xs (res + 1)

t1 :: [Integer]
t1 = map myLength2 [[1, 2, 3], [2, 3, 4, 56], [], [1]]

-- 2. Multiply a list's elements.

-- METHOD 1

{-# ANN myProduct1 "HLint: ignore Use foldr" #-}
myProduct1 :: Num a => [a] -> a
myProduct1 [] = 1
myProduct1 (x:xs) = x * myProduct1 xs

-- METHOD 2

myProduct2 :: Num a => [a] -> a
myProduct2 xs = helper xs 1
    where
        helper [] res = res
        helper (x:xs) res = helper xs (x * res)

t2 :: [Double]
t2 = map myProduct2 [[1, 2, 3], [34, 0.5], [0, 1, 9382, 9483984, 3984938438]]

-- 3. Find the smallest element in a list.

{-# ANN myMinimum "HLint: ignore Use min" #-}
myMinimum :: Ord a => [a] -> a
myMinimum [] = error "The list is empty."
myMinimum [x] = x
myMinimum (x:xs) = if x < myMinimum xs then x else myMinimum xs

t3 :: [Integer]
t3 = map myMinimum [[290483, 2938293, 2, 32923], [3872837283, 9238928392839, 9892], [1]]

-- 4. Find the largest element in a list.

{-# ANN myMaximum "HLint: ignore Use max" #-}
myMaximum :: Ord a => [a] -> a
myMaximum [] = error "The list is empty."
myMaximum [x] = x
myMaximum (x:xs) = if x > myMaximum xs then x else myMaximum xs

t4 :: [Integer]
t4 = map myMaximum [[290483, 2938293, 2, 32923], [3872837283, 9238928392839, 9892], [1]]

-- 5. Find the element at a specified index in a list.

myIndex :: (Num a, Integral b) => [a] -> b -> a
myIndex arr idx
    | null arr = error "The list is empty."
    | idx >= myLength2 arr || idx < 0 = error "Index out of bounds."
    | otherwise = helper 0 idx arr
        where
            helper _ _ [] = error "Index out of bounds."
            helper curr target (head:tail)
                | curr == target = head
                | otherwise = helper (curr + 1) target tail

t5 :: [Integer]
t5 = map (uncurry myIndex) [([1, 2, 3], 0), ([2, 3, 4, 5, 6, 7], 3)]

-- 6. Concat two lists.

{-# ANN myConcat "HLint: ignore Use foldr" #-}
myConcat :: [a] -> [a] -> [a]
myConcat [] ys = ys
myConcat (x:xs) ys = x : myConcat xs ys

-- 7. Check if a list is a palindrome. A palindrome list means it's the same way backwards as it is forwards.

-- METHOD 1 WITH HELPER: myReverse to reverse the list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (head:tail) = myConcat (myReverse tail) [head]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome arr = arr == myReverse arr

-- METHOD 2
isPalindrome2 :: Eq a => [a] -> Bool
isPalindrome2 [] = True
isPalindrome2 [_] = True
isPalindrome2 arr = head arr == last arr && isPalindrome ((init . tail) arr)

-- 8. Turn an integer into a list of digits.

itol :: Integral a => a -> [a]
itol n
    | n < 0 = error "Input must be positive."
    | n < 10 = [n]
    | otherwise = itol (div n 10) ++ [mod n 10]

-- 9. Requeue the head of a list.

requeue :: [a] -> [a]
requeue (head:tail) = tail ++ [head]

-- 10. Get the average of an integer list's elements.

listAVG :: (Integral nums, Fractional avg) => [nums] -> avg
listAVG arr
    | null arr = 0
    | otherwise = fromIntegral (mySum arr) / fromIntegral (myLength2 arr)
    where
        mySum [] = 0
        mySum (head:tail) = head + mySum tail

-- 11. Convert a base 10 integer to base 'base'.

toBase :: Integral a => a -> a -> [a]
toBase base 0 = [0]
toBase base number = reverse $ go number
    where
        go 0 = []
        go n = mod n base : go (div n base)

-- 12. Convert an integer from base 'base' to base 10.

fromBase :: Integral a => a -> [a] -> a
fromBase base digits = go (reverse digits) 0
    where
        go [] _ = 0
        go (head:tail) pos = head * base^pos + go tail (pos+1)