import Data.Char 

-- | Exercício 1
digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (h:t) 
    | isAlpha h = (h:ls,ds)
    | isDigit h = (ls,h:ds) 
    | otherwise = (ls,ds)
    where (ls,ds) = digitAlpha t 

--ou (com utilização de acumuladores)--

digitAlphaAc :: String -> (String,String)
digitAlphaAc s = digitAlphaAcAux s ([],[])

    where 
      digitAlphaAcAux :: String -> (String,String) -> (String,String)
      digitAlphaAcAux [] acc = acc
      digitAlphaAcAux (h:t) (ls,ds) | isAlpha h = digitAlphaAcAux t (h:ls,ds) 
                                    | isDigit h = digitAlphaAcAux t (ls, h:ds)
                                    | otherwise = digitAlphaAcAux t (ls,ds) 