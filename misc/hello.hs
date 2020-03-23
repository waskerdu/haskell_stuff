--main = print(intListLength( hailstoneSeq 42) )
--main = print(summer( doubler [1,3,8,6] )  `mod` 10)
--main = print(validate 4012888888881882)
main = print(hanoi 3 "a" "b" "c")

intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (x:xs) = 1 + intListLength(xs)

toDigits :: Integer -> [Integer]
toDigits n
    | n < 1 = []
    | n < 10 = [n]
    | otherwise = toDigits(n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n < 1 = []
    | n < 10 = [n]
    | otherwise = n `mod` 10 : toDigitsRev(n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:(y:zs)) = (x + x) : y : doubleEveryOther(zs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
    | x < 10 = x + sumDigits(xs)
    | otherwise = 1 + x - 10 + sumDigits(xs)

validate :: Integer -> Bool
validate n = sumDigits(doubleEveryOther(toDigits(n))) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n start via end
    | n == 0 = []
    | otherwise = (hanoi (n-1) start end via) ++ [(start, end)] ++ (hanoi (n-1) via start end)