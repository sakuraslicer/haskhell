-- Лабораторная работа №1
triangle :: Float -> Float -> Float -> [Float]
triangle a b c = [a * 180 / (a + b + c), b * 180 / (a + b + c), c * 180 / (a + b + c)]