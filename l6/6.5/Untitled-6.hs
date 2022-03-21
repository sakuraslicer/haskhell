import System.IO
func :: Float->Float->Float->IO ()
func a b c = do
    putStr "Первый угол: "
    print (a * 180 / (a + b + c)) 
    putStr "Первый угол: "
    print (b * 180 / (a + b + c))
    putStr "Третий угол: "
    print (c * 180 / (a + b + c))
    return ()


main = do
        putStrLn "Сторона a: "
        a1 <- getLine
	
        putStrLn "Сторона b: "
        b1 <- getLine 

        putStrLn "Сторона c: "
        c1 <- getLine 
	
        func (read a1 :: Float) (read b1 :: Float) (read c1 :: Float)