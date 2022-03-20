import System.IO
func :: Float->Float->Float->IO ()
func a b c = do
    putStr "First angle: "
    print (a * 180 / (a + b + c)) 
    putStr "Second angle: "
    print (b * 180 / (a + b + c))
    putStr "Third angle: "
    print (c * 180 / (a + b + c))
    return ()


main = do
	putStrLn "Enter a: "
	a1 <- getLine
	
	putStrLn "Enter b: "
	b1 <- getLine 

	putStrLn "Enter c: "
	c1 <- getLine 
	
	func (read a1 :: Float) (read b1 :: Float) (read c1 :: Float)