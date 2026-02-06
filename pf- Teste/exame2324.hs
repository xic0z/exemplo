substs :: Eq a => (a,a) -> [a] -> [a]
substs (_,_) [] = []
substs (x,y) (h:t)
    | x == h = y : substs (x,y) t 
    | otherwise = h : substs (x,y) t

progressao :: [Int] -> Maybe Int
progressao []  = Nothing
progressao [_] = Just 0  -- Uma progressão de um elemento tem diferença 0 por convenção
progressao (x:y:xs) = verifica (y - x) (y:xs)
  where
    verifica d [a]    = Just d
    verifica d (a:b:as)
        | b - a == d  = verifica d (b:as)
        | otherwise   = Nothing

