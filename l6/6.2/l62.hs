import System.Environment
import Data.List

main = do 
        putStrLn "Введенные аргументы: "
        args <- getArgs
        mapM putStrLn args