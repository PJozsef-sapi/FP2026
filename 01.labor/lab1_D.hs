-- PART I

-- 1. Add two numbers
myadd :: Num a => a -> a -> a
myadd a b = a + b

-- 2. Subtract two numbers
mysub :: Num a => a -> a -> a
mysub a b = a - b

-- 3. Multiply two numbers
mymult :: Num a => a -> a -> a
mymult a b = a * b

-- 4. Divide two numbers
floatdiv :: Fractional a => a -> a -> a
floatdiv a b = a / b

intdiv :: Integral a => a -> a -> a
intdiv a b = a `div` b

-- 5. Get the remainder of an integer division
mymod :: Integral a => a -> a -> a
mymod a b = a `mod` b

-- 6. Calculate the root of a first degree polynomial (ax + b = 0)
root1 :: Fractional a => a -> a -> a
root1 a b = (-b) / a

-- 7. Get the absolute value of a number

-- METHOD 1
myabs :: (Num a, Ord a) => a -> a
myabs a = if a >= 0 then a else -a

-- METHOD 2
myabs2 :: (Num a, Ord a) => a -> a
myabs2 a
    | a >= 0 = a
    | otherwise = -a

-- 8. Get the sign of an integer (-1 for negative, 1 for positive)

-- METHOD 1 (you can chain if statements)
{-# ANN sign "HLint: ignore Use guards" #-}
sign :: (Fractional a, Ord a) => a -> Char
sign a = if a > 0 then '+' else if a < 0 then '-' else '0'

-- METHOD 2
sign2 :: (Fractional a, Ord a) => a -> Char
sign2 a
    | a > 0 = '+'
    | a < 0 = '-'
    | otherwise = '0'

-- 9. Get the largest number from two.
{-# ANN mymax "HLint: ignore Use max" #-}
mymax :: (Fractional a, Ord a) => a -> a -> a
mymax a b = if a > b then a else b

-- 10. Get the smallest number from two.
{-# ANN mymin "HLint: ignore Use min" #-}
mymin :: (Fractional a, Ord a) => a -> a -> a
mymin a b = if a < b then a else b

-- 11. Calculate the roots of a second degree polynomial (ax^2 + bx + c = 0)
root2 :: (Ord a, Floating a) => a -> a -> a -> [a]
root2 a b c
    | delta < 0 = error "The roots are complex."
    | delta == 0 = [x1]
    | otherwise = [x1, x2]
    where
        delta = b**2 -4*a*c
        x1 = (-b + sqrt delta) / (2*a)
        x2 = (-b - sqrt delta) / (2*a)

-- 12. Are two value pairs nearly the same? Example:
--     (1,2) = (2,1) TRUE
--     (1,2) = (3,4) FALSE
paireq :: Eq a => (a, a) -> (a, a) -> Bool
paireq p1 p2 = (a == c && b == d) || (a == d && b == c)
    where
        (a, b) = p1
        (c, d) = p2

-- 13. Calculate the factorial of an integer.

-- METHOD 1
fact1 :: (Eq t, Num t) => t -> t
fact1 0 = 1
fact1 n = n * fact1 (n-1)

-- METHOD 2
fact2 :: (Ord t, Num t) => t -> t
fact2 n
    | n < 0 = error "Input cannot be negative."
    | n == 0 = 1
    | otherwise = n * fact2 (n-1)

-- METHOD 3 | EXAMPLE INPUT >> fact3 1 5
fact3 :: (Ord t, Num t) => t -> t -> t
fact3 res n
    | n < 0 = -1
    | n == 0 = res
    | otherwise = fact3 (res*n) (n-1)

-- LEFT TO DO: I-14, II

-- 14. Calculate x^n, n > 0.

-- METHOD 1
mypow1 :: (Fractional a, Integral b) => a -> b -> a
mypow1 x n = if n > 0 then x^n else error "The exponent must be a positive integer."

-- METHOD 2
mypow2 :: (Fractional a, Integral b) => a -> b -> a
mypow2 x n
    | n < 0 = error "The exponent must be a positive integer."
    | n == 0 = 1
    | n == 1 = x
    | otherwise = x * mypow2 x (n-1)

-- METHOD 3

-- PART II

-- 1. Calculate the square roots of the first n numbers.
firstNsqrt :: (Enum a, Floating a) => a -> [a]
firstNsqrt n = [sqrt a | a <- [1..n]]

-- 2. Calculate the first n perfect squares.
firstNPS :: (Num a, Enum a) => a -> [a]
firstNPS n = [n^2 | n <- [1..n]]

-- 3. Calculate the first n cubed integers.
firstNcubes :: (Num a, Enum a) => a -> [a]
firstNcubes n = [n^3 | n <- [1..n]]

-- 4. Get the first n numbers that are not perfect squares. TBA (To Be Added)

-- 5. Get the first n powers of x.
xpow1ton :: (Num a, Integral b) => a -> b -> [a]
xpow1ton x n = [x^a | a <- [1..n]]

-- 6. Get the even divisors of a number.
evendiv :: Integral a => a -> [a]
evendiv x = [a | a <- [1..x], mod x a == 0, even a]

-- 7-13 TBA