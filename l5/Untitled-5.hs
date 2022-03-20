{-- data Prop = Var String | And Prop Prop | Or Prop Prop | Not Prop deriving (Eq,Show)
 
vars' :: Prop -> [String] -> [String]
vars' (Var x) acc = if x `elem` acc then acc  else x : acc
vars' (And x y) acc = vars' x (vars' y acc)
vars' (Or x y) acc = vars' x (vars' y acc)
vars' (Not x) acc = vars' x acc
 
vars :: Prop -> [String]
vars x = vars' x []
 
 
getVal :: String -> [(String,Bool)] -> Bool
getVal s vt = snd $ head w
              where w = filter (\ q -> s == fst q) vt  
 
calcValue :: Prop -> [(String,Bool)] -> Bool
calcValue (Var x) vt = getVal x vt
calcValue (And x y) vt = (calcValue x vt) && (calcValue y vt)
calcValue (Or x y) vt = (calcValue x vt) || (calcValue y vt)
calcValue (Not x) vt =  not (calcValue x vt) --}
