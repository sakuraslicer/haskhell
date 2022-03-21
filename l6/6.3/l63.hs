import System.IO

main = do 
        putStrLn "Имя файла: "
        fileName <- getLine
        fromHandle <- openFile fileName ReadMode
        fileContent <- hGetContents fromHandle
        hPutStr stdout fileContent