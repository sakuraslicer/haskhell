{--

--}
t1::((Char,Integer), String, [Double])
t1=(('f',1), "test", [3.5, 2.5])

triangle :: Float -> Float -> Float -> [Float]
triangle a b c = [a * 180 / (a + b + c), b * 180 / (a + b + c), c * 180 / (a + b + c)]

getN :: Int -> [a] -> a
getN _ [] = error "Empty list"
getN 0 (x:_) = x
getN k (_:xs) = getN (k-1) xs

task :: [Int] -> [Int]
task [] = []
task (x:xs) | (x>0) = (negate x) : task xs
            | otherwise = x:(task xs)


taskx :: [Integer] -> [Integer]
taskx [] = []
taskx (x:xs) = map (\x -> if (x>0) then negate x else id x) (x:xs)
