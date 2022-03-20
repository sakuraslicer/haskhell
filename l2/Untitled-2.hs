-- Лабораторная работа №2
-- Задание 1: Определите функцию, принимающую на вход целое число n и возвращающую список, содержащий n элементов, упорядоченных по возрастанию

-- Список натуральных чисел.
t1 :: Integer -> [Integer]
t1 0 = [ ]
t1 x = t1 (x-1)++(x:[])

-- Список нечетных натуральных чисел.
t2 :: Integer -> [Integer]
t2 0 = [ ]
t2 x = t2 (x - 1)++(2 * x - 1:[])

-- Список четных натуральных чисел.
t3 :: Integer -> [Integer]
t3 0 = [ ]
t3 x = t3 (x-1)++(2*x:[])

-- Список кубов натуральных чисел.
t4 :: Integer -> [Integer]
t4 0 = [ ]
t4 x = t4 (x-1)++(x*x*x:[])

-- Список факториалов.
-- n!=n*(n-1)*---*1
t5 :: Integer -> [Integer]
factorial 0 = 1
factorial x = x*factorial(x-1)
t5 0 = [ ]
t5 x = t5 (x-1)++[factorial(x)]  

-- Список степеней десятки.
t6 :: Integer -> [Integer]
deg 1 = 1
deg x = 10*deg(x-1)
t6 0 = [ ]
t6 x = t6 (x-1)++[deg(x+1)]

-- Список треугольных чисел
-- Число точек, которые могут быть расставлены в форме правильного треугольника.
t7 :: Integer -> [Integer]
piram 1 = 1
piram(x) = x+piram(x-1)
t7 0 = [ ]
t7 x = t7 (x-1)++[piram(x)]

-- Список пирамидальных чисел
-- https://ru.wikipedia.org/wiki/%D0%9F%D0%B8%D1%80%D0%B0%D0%BC%D0%B8%D0%B4%D0%B0%D0%BB%D1%8C%D0%BD%D0%BE%D0%B5_%D1%87%D0%B8%D1%81%D0%BB%D0%BE
t8 :: Integer -> [Integer]
pyramid 1 = 1
pyramid(x) = x+pyramid(x-1)
p 1 = 1
p x = pyramid(x) + p(x-1)
t8 0 = [ ]
t8 x = t8 (x-1)++[p(x)]


-- Задание 2.1
gN :: Int -> [a] -> a
gN 0 (x:_) = x
gN a (_:xs) = gN (a-1) xs

-- Задание 2.2
chS :: [Int] -> [Int]
chS [] = []
chS (x:xs) | (x>0) = (negate x) : chS xs
           | otherwise = x:(chS xs)