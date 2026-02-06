import Data.List

-- Teste se um predicado e verdade para algum elemento de uma lista, por exemplo:
-- any odd [1..10] == True

any1 :: (a -> Bool) -> [a] -> Bool 
any1 _ [] = False
any1 p (x:xs)
    | p x       = True
    | otherwise = any1 p xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' _ _      _      = []

takeWhile1 :: (a->Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 p (x:xs)
    | p x = x : takeWhile1 p xs
    | otherwise = [] -- a partir do momento que um dado numero nao verifica o predicado, para!

dropWhile1 :: (a->Bool) -> [a] -> [a]
dropWhile1 _ [] = []
dropWhile1 p (x:xs)
    | p x = dropWhile1 p xs
    | otherwise = x:xs

span' :: (a-> Bool) -> [a] -> ([a],[a])
span' _ [] = ([], [])
span' p (x:xs)
    | p x       = (x : sim, nao)
    | otherwise = ([], x:xs)
  where (sim, nao) = span' p xs

deleteBy1 :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy1 _ _ [] = []
deleteBy1 p x (h:t)
    | p x h = t
    | otherwise = h : deleteBy1 p x t 

sortOn1 :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn1 f [] = []
sortOn1 f (h:t) = insertOn f h (sortOn1 f t)
    
insertOn :: (Ord b) => (a -> b) -> a -> [a] -> [a]
insertOn _ x [] = [x]
insertOn f x (h:t) = if f x > f h then h : insertOn f x t 
                     else x : h : t

type Polinomio = [Monomio]
type Monomio = (Float,Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau g p = filter (\m -> snd m == g) p

conta :: Int -> Polinomio -> Int
conta n p = length (selgrau n p)

grau :: Polinomio -> Int 
grau p = maximum (map snd p)

deriv :: Polinomio -> Polinomio
deriv p = map (\(c, g) -> (c * fromIntegral g, g - 1)) p

calcula :: Float -> Polinomio -> Float
calcula x p = sum (map (\(c, g) -> c * (x ^ g)) p)

simp :: Polinomio -> Polinomio
simp p = filter (\(c,g) -> c /= 0) p 

mult :: Monomio -> Polinomio -> Polinomio
mult (c,g) p = map (\(c1,g1) -> (c * c1, g + g1)) p 

ordena :: Polinomio -> Polinomio
ordena p = sortOn snd p 

normaliza :: Polinomio -> Polinomio
normaliza p = normalizaAux (ordena p)
  where
    normalizaAux [] = []
    normalizaAux [m] = [m]
    normalizaAux ((c1,g1):(c2,g2):cauda)
        | g1 == g2  = normalizaAux ((c1 + c2, g1) : cauda) -- Soma coeficientes se o grau for igual
        | otherwise = (c1,g1) : normalizaAux ((c2,g2) : cauda) -- Mantém e avança se forem diferentes

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = foldl (\acc m -> soma (mult m p2) acc) [] p1

type Mat a = [[a]]

dimOK :: Mat a -> Bool
dimOK [] = True  -- Uma matriz vazia está correta
dimOK (l:ls) = all (\linha -> length linha == length l) ls 

dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat m = (length m, length (head m))

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat m1 m2 = zipWith (zipWith (+)) m1 m2

transpose :: Mat a -> Mat a
transpose [] = []
transpose (l:ls) = foldr (zipWith (:)) (map (const []) l) (l:ls) 