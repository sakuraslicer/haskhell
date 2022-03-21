import System.IO
reader :: [String]->Int->IO ()
reader (a:ab) n = do
    if (n ==0) then return () else do
        putStr a
        putStr "\n"
        reader ab (n-1)
        return ()
reader [] n = do
        return ()

main = do
        putStrLn "Имя файла: "
        fileName <- getLine
        putStrLn "Количество строк для вывода: "
        numberOfLines <- getLine
        fileContent <- openFile fileName ReadMode
        stringArray <- hGetContents fileContent
        reader (lines stringArray) (read numberOfLines::Int)
        hClose fileContent