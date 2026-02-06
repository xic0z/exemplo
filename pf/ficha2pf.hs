import Data.Char

-- | Exercício 1 
-- | a
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

-- | b
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2)==0 then h : (funB t)
else (funB t)

-- | c
funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []

-- | Exercício 2
-- | a
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = 2*h : dobros t 

-- | b
numOcorre :: Char -> String -> Int
numOcorre c (h:t) = if c == h then 1 + numOcorre c t 
                    else numOcorre c t

-- | c
positivos :: [Int] -> Bool 
positivos (x:y) = if x < 0 then False
                  else positivos y

-- | d
soPos :: [Int] -> [Int] 
soPos [] = []
soPos (x:y) = if x < 0 then soPos y
              else x : soPos y 

-- ! e
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (x:y) = if x < 0 then x + somaNeg y
                else somaNeg y

-- | f
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t)
    | length t < 3 = h:t
    | otherwise = tresUlt t

-- | i
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):t) = (a+ra, b+rb, c+rc)
    where (ra,rb,rc) = sumTriplos t


soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t)
    | isDigit h = h : soDigitos t
    | otherwise = soDigitos t


minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) 
      | isLower h = 1 + minusculas t 
      | otherwise = minusculas t 

nums :: String -> [Int]
nums "" = []
nums (h:t) 
  | isDigit h = (ord h - ord '0'): nums t 
  | otherwise = nums t 

type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta :: Int -> Polinomio -> Int
conta a [] = 0
conta a ((coef,grau):t) = if grau == a then 1 + conta a t
                          else conta a t 

grau1 :: Polinomio -> Int
grau1 [] = 0
grau1 ((coef,grau):t) = if grau > grau1 t then grau
                        else grau1 t 

selgrau :: Int -> Polinomio -> Polinomio
selgrau a [] = []
selgrau a ((coef,grau):t) = if a == grau then (coef,grau) : selgrau a t 
                            else selgrau a t 

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((coef,grau):t) = if grau /= 0 then (coef * fromIntegral grau, grau -1) : deriv t
                        else []

calcula :: Float -> Polinomio -> Float
calcula a [] = 0
calcula a ((coef,grau):t) = coef * (a)^grau + calcula a t

simp :: Polinomio -> Polinomio 
simp [] = []
simp ((coef,grau):t) = if coef == 0 then simp t
                       else (coef,grau) : simp t  

mult :: Monomio -> Polinomio -> Polinomio
mult (coef1,grau1) [] = []
mult (coef1,grau1) ((coef,grau):t) = (coef1 * coef, grau1 + grau) : mult (coef1,grau1) t

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((coef,grau):t) = normaliza1 (coef,grau) (normaliza t) 

normaliza1 :: Monomio -> Polinomio -> Polinomio
normaliza1 (coef1,grau1) [] = [(coef1,grau1)]
normaliza1 (coef1,grau1) ((coef,grau):t) = if grau1 == grau then (coef1 + coef,grau) : t 
                                           else (coef,grau) : normaliza1 (coef1,grau1) t 

soma :: Polinomio -> Polinomio -> Polinomio
soma (x:xs) [] = normaliza (x:xs)
soma [] (x:xs) = normaliza (x:xs)
soma (x:xs) ((coef,grau):t) = normaliza ((x:xs) ++ ((coef,grau):t)) 


produto :: Polinomio -> Polinomio -> Polinomio
produto [] (x:xs) = []                     
produto (x:xs) [] = []                     
produto (x:xs) ((coef,grau):t) = normaliza (produtoAux x ((coef,grau):t) ++ produto xs ((coef,grau):t))

produtoAux :: Monomio -> Polinomio -> Polinomio
produtoAux (coef1,grau1) [] = []                       
produtoAux (coef1,grau1) ((coef,grau):t) = (coef1*coef, grau1+grau) : produtoAux (coef1,grau1) t

testarEtapa :: [Etapa] -> Bool  
testarEtapa (H x y, H a b) = if x >= 0 && x <= 23, a >= 0 && a <= 23, y >= 0 && y <= 59, b >= 0 && b <= 59 then True
                                else if x > a then True 
                                else False 

areaPolaux :: [Figura] -> Double 
areaPolaux Triangulo (p1 p2 p3) = 0.5 * (abs (posx p1 * (posy p2 - posy p3) + posx p2 * (posy p3 - posy p1) + posx p3 *  (posy p1 - posy p2)))
areaPolaux _ = 0

areaPol :: Poligonal -> Double
areaPol (p:ps) = sum [triangula (p:ps) areaPol]