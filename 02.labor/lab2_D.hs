-- 1. Calculate the digit's product.

-- METHOD 1
digitprod :: Integral a => a -> a
digitprod n
    | n < 0 = error "Input cannot be negative."
    | n < 10 = n
    | otherwise = mod n 10 * digitprod (div n 10)

-- METHOD 2
digitprod2 :: Integral a => a -> a
digitprod2 n
    | n < 0 = error "Input cannot be negative."
    | otherwise = helper n 1
    where
        helper 0 acc = acc
        helper x acc = helper (div x 10) (acc * mod x 10)

-- 2. Caltulate the digits' sum.

-- METHOD 1
digitsum :: Integral a => a -> a
digitsum n
    | n < 0 = error "Input cannot be negative."
    | n < 10 = n
    | otherwise = mod n 10 + digitsum (div n 10)

-- METHOD 2
digitsum2 :: Integral a => a -> a
digitsum2 n
    | n < 0 = error "Input cannot be negative."
    | otherwise = helper n 0
    where
        helper 0 acc = acc
        helper x acc = helper (div x 10) (acc + mod x 10)

-- 3. Count the number of digits.

-- METHOD 1
digitamount :: Integral a => a -> a
digitamount n
    | n < 0 = error "Input cannot be negative."
    | n < 10 = 1
    | otherwise = 1 + digitamount (div n 10)

-- METHOD 2
digitamount2 :: Integral a => a -> a
digitamount2 n
    | n < 0 = error "Input cannot be negative."
    | n == 0 = 1
    | otherwise = helper n 0
    where
        helper 0 acc = acc
        helper x acc = helper (div x 10) (acc + 1)

-- 4. Calculate the sum of the specified digits in the number.

sumSpecified :: Integral a => a -> a -> a
sumSpecified n d
    | n < 0 = error "Argument 1 cannot be negative."
    | d < 0 || d > 9 = error "Argument 2 is not a digit."
    | n == 0 = 0
    | mod n 10 == d = d + sumSpecified (div n 10) d
    | otherwise = sumSpecified (div n 10) d

-- 5. Count all even digits.

amountEvenDigits :: Integral a => a -> a
amountEvenDigits n
    | n < 0 = error "Input cannot be negative."
    | n < 10 = if even n then 1 else 0
    | even (mod n 10) = 1 + amountEvenDigits (div n 10)
    | otherwise = amountEvenDigits (div n 10)

-- 6. Find the largest digit.

largestDigit :: Integral a => a -> a
largestDigit n
    | n < 0 = error "Input cannot be negative."
    | n < 10 = n
    | otherwise = max (mod n 10) (largestDigit (div n 10))

-- 7. I won't try to translate this.

countDigitBase :: Integral a => a -> a -> a -> a
countDigitBase n b d
    | n < 0 = error "Input cannot be negative."
    | b < 2 = error "Base must be at least 2."
    | d < 0 || d > b = error "Digit not valid in specified base."
    | n == 0 = 0
    | mod n b == d = 1 + countDigitBase (div n b) b d
    | otherwise = countDigitBase (div n b) b d

-- 8. Calculate the n-th Fibonacci number. (GENERALIZED)

fibonacci :: Integral a => a -> a
fibonacci n
    | n < 0 = error "Input cannot be negative."
    | otherwise = helper n 0 1
    where
        helper 0 a _ = a
        helper k a b = helper (k-1) b (a+b)