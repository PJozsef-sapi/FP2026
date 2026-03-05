atlag :: (Floating a) => [a] -> a
atlag ls = sum ls / fromIntegral (length ls)

-- PART II

-- 1. Determine the length of an array.

-- METHOD 1

myLength1 :: Integral a => [a] -> a
myLength1 [] = 0
myLength1 (_:xs) = 1 + myLength1 xs

-- METHOD 2

myLength2 :: Integral a => [a] -> a
myLength2 xs = helper xs 0
    where
        helper [] res = res
        helper (_:xs) res = helper xs (res + 1)


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