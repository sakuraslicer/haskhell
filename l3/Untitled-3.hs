-- 1.1
avg :: [Double] -> Double
avg x = (foldr (\ a b -> a+b) 0.0 x) / (fromIntegral $ length x)

-- 1.2
scalProd :: [Double] -> [Double] -> Double
scalProd x y =  foldr (+) 0.0 (zipWith (*) x y)

-- 1.3
countOfNeg :: [Int] -> Int
countOfNeg = length . filter (<0)

-- 1.4
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort (filter (< x) xs) ++ [x] ++ quickSort (filter (>= x) xs)

-- 1.5
quickSortX :: (a -> a -> Bool) -> [a] -> [a]
quickSortX fc [] = []
quickSortX fc (x:xs) = quickSortX fc (filter (fc x) xs) ++ [x] ++ (quickSortX fc (filter (\ y -> not $ fc x y) xs))

-- Задание 2.1 
gNx :: Int -> [a] -> a
gNx k = snd . head . dropWhile ((k /= ) . fst) . zip [0..]

-- Задание 2.2
chSx :: [Integer] -> [Integer]
chSx [] = []
chSx (x:xs) = map (\x -> if (x>0) then negate x else id x) (x:xs)