import System.IO

reader :: String -> Int
reader x = (read x) :: Int

main = do
         putStrLn "Введите a:"
         a <- getLine
         putStrLn "Введите b:"
         b <- getLine
         putStrLn $ "a+b=" ++ (show $ (reader a)+(reader b))
