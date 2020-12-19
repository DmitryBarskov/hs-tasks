module Part3 where

primes :: [Integer]
primes = 2 : filter isPrime [3, 5 ..]

isPrime :: Integer -> Bool
isPrime n = all (\p -> n `mod` p /= 0) (takeWhile (\p -> p * p <= n) primes)

------------------------------------------------------------
-- PROBLEM #18
--
-- Проверить, является ли число N простым (1 <= N <= 10^9)
prob18 :: Integer -> Bool
prob18 = isPrime

------------------------------------------------------------
-- PROBLEM #19
--
-- Вернуть список всех простых делителей и их степеней в
-- разложении числа N (1 <= N <= 10^9). Простые делители
-- должны быть расположены по возрастанию
prob19 :: Integer -> [(Integer, Int)]
prob19 x = map (\d -> (d, factorize d x)) (primeDivisors x)

primeDivisors :: Integer -> [Integer]
primeDivisors x = filter (\p -> x `mod` p == 0) (takeWhile (<= x) primes)

factorize :: Integer -> Integer -> Int
factorize divisor number
  | number `mod` divisor == 0 = 1 + factorize divisor (number `div` divisor)
  | otherwise = 0

------------------------------------------------------------
-- PROBLEM #20
--
-- Проверить, является ли число N совершенным (1<=N<=10^10)
-- Совершенное число равно сумме своих делителей (меньших
-- самого числа)
prob20 :: Integer -> Bool
prob20 n = 2 * n == sum (divisors n)

------------------------------------------------------------
-- PROBLEM #21
--
-- Вернуть список всех делителей числа N (1<=N<=10^10) в
-- порядке возрастания
prob21 :: Integer -> [Integer]
prob21 = divisors

sqrt' :: Integral a => a -> a
sqrt' x = round (sqrt (fromIntegral x))

divisors :: Integer -> [Integer]
divisors n = halfDivisors ++ allDivisors halfDivisors []
  where
    allDivisors [] acc = acc
    allDivisors (x:xs) acc =
      let a = (n `div` x)
      in if a == x
        then allDivisors xs acc
        else allDivisors xs (a : acc)
    halfDivisors = filter isDivisor [1..(sqrt' n)]
    isDivisor candidate = n `mod` candidate == 0

------------------------------------------------------------
-- PROBLEM #22
--
-- Подсчитать произведение количеств букв i в словах из
-- заданной строки (списка символов)
prob22 :: String -> Integer
prob22 text = product (map (max 1 . count 'i') (words text))

count :: Eq a => a -> [a] -> Integer
count item = fromIntegral . length . filter (== item)

------------------------------------------------------------
-- PROBLEM #23
--
-- На вход подаётся строка вида "N-M: W", где N и M - целые
-- числа, а W - строка. Вернуть символы с N-го по M-й из W,
-- если и N и M не больше длины строки. Гарантируется, что
-- M > 0 и N > 0. Если M > N, то вернуть символы из W в
-- обратном порядке. Нумерация символов с единицы.
prob23 :: String -> Maybe String
prob23 str
  | n >= len || m >= len = Nothing
  | m < n = Just (reverse (slice m n))
  | otherwise = Just (slice n m)
  where
    ((n, m), text) = rangeAndText str
    len = length str
    slice a b = take (b - a + 1) (drop a text)

rangeAndText :: String -> ((Int, Int), String)
rangeAndText str = ((read a, read b), text)
  where
    (a, b) = splitOn '-' range
    (range, text) = splitOn ':' str

splitOn :: Char -> String -> (String, String)
splitOn symbol = iter ""
  where
    iter :: String -> String -> (String, String)
    iter acc "" = error "No such symbol"
    iter acc (x : xs) = if symbol == x then (reverse acc, xs) else iter (x : acc) xs

------------------------------------------------------------
-- PROBLEM #24
--
-- Проверить, что число N - треугольное, т.е. его можно
-- представить как сумму чисел от 1 до какого-то K
-- (1 <= N <= 10^10)
prob24 :: Integer -> Bool
prob24 num = helper 1 0
  where
    helper i probe
      | probe < num = helper (i + 1) (probe + i)
      | probe == num = True
      | probe > num = False

------------------------------------------------------------
-- PROBLEM #25
--
-- Проверить, что запись числа является палиндромом (т.е.
-- читается одинаково слева направо и справа налево)
prob25 :: Integer -> Bool
prob25 = isPalindrome . digits

isPalindrome :: Eq a => [a] -> Bool
isPalindrome word = word == reverse word

digits :: Integer -> [Int]
digits = iter []
  where
    iter acc 0 = acc
    iter acc n = iter (fromIntegral (n `mod` 10) : acc) (n `div` 10)

------------------------------------------------------------
-- PROBLEM #26
--
-- Проверить, что два переданных числа - дружественные, т.е.
-- сумма делителей одного (без учёта самого числа) равна
-- другому, и наоборот
prob26 :: Integer -> Integer -> Bool
prob26 a b = sum (divisors a) == a + b && sum (divisors b) == a + b

------------------------------------------------------------
-- PROBLEM #27
--
-- Найти в списке два числа, сумма которых равна заданному.
-- Длина списка не превосходит 500
prob27 :: Int -> [Int] -> Maybe (Int, Int)
prob27 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #28
--
-- Найти в списке четыре числа, сумма которых равна
-- заданному.
-- Длина списка не превосходит 500
prob28 :: Int -> [Int] -> Maybe (Int, Int, Int, Int)
prob28 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #29
--
-- Найти наибольшее число-палиндром, которое является
-- произведением двух K-значных (1 <= K <= 3)
prob29 :: Int -> Int
prob29 k = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #30
--
-- Найти наименьшее треугольное число, у которого не меньше
-- заданного количества делителей
prob30 :: Int -> Integer
prob30 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #31
--
-- Найти сумму всех пар различных дружественных чисел,
-- меньших заданного N (1 <= N <= 10000)
prob31 :: Int -> Int
prob31 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #32
--
-- В функцию передаётся список достоинств монет и сумма.
-- Вернуть список всех способов набрать эту сумму монетами
-- указанного достоинства
-- Сумма не превосходит 100
prob32 :: [Int] -> Int -> [[Int]]
prob32 = error "Implement me!"
