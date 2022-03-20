data LibObj = Book String String | Magazine String Int Int | NewsPaper String Int Int Int deriving (Eq,Show)
 
isPeriodic :: LibObj -> Bool
isPeriodic (Book _ _)          = False
isPeriodic (Magazine _ _ _)    = True
isPeriodic (NewsPaper _ _ _ _) = True
 
checkT :: LibObj -> String -> Bool
checkT (Book t a) s = (s == t) 
checkT (Magazine t y m) s = (s == t)
checkT (NewsPaper t y m d) s = (s == t)
 
getByTitle :: [LibObj] -> String -> [LibObj]
getByTitle [] _ = []
getByTitle (o:os) s | (checkT o s) = o : getByTitle os s
                    | otherwise = getByTitle os s
                    
getMon :: LibObj -> Int
getMon (Book _ _) = 0
getMon (Magazine _ y m) = m
getMon (NewsPaper _ y m d) = m
                    
getYear :: LibObj -> Int
getYear (Book _ _) = 0
getYear (Magazine _ y m) = y
getYear (NewsPaper _ y m d) = y
                    
getByMonth :: [LibObj] -> Int -> Int -> [LibObj]
getByMonth [] _ _ = []
getByMonth (o:os) y m = if ((getMon o) == m) && ((getYear o) == y) then o : (getByMonth os y m) else (getByMonth os y m)
 
gM :: [LibObj] -> Int -> [LibObj]
gM [] _     = []
gM (o:os) m = if ((getMon o) == m)  then o : (gM os m) else (gM os m)
 
getByMonths :: [LibObj] -> [Int] -> [LibObj]
getByMonths [] _     = []
getByMonths _ []     = []
getByMonths o (m:ms) = (gM o m) ++  getByMonths o ms
 
getAuthors :: [LibObj] -> [String]
getAuthors [] = []
{- Book - Name - Author -}
getAuthors ((Book _ a):last) = a : getAuthors last
getAuthors ((Magazine _ _ _):last) = getAuthors last  
getAuthors ((NewsPaper _ _ _ _):last) = getAuthors last