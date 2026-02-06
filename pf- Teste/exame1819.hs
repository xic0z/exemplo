isSorted :: (Ord a) => [a] -> Bool
isSorted [] = False
isSorted [e] = True 
isSorted (x:x1:xs) 
    | x < x1 && isSorted (x1:xs) = True
    | otherwise = False 

inits :: [a] -> [[a]]
inits []     = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

inits1 :: [a] -> [[a]]
inits1 []     = [[]]
inits1 (x:xs) = [] : colaEmTodos x (inits1 xs)

-- Esta função faz exatamente o que o map (x:) fazia
colaEmTodos :: a -> [[a]] -> [[a]]
colaEmTodos x [] = []
colaEmTodos x (h:t) = (x : h) : colaEmTodos x t


maximumMB :: (Ord a, Num a) => [Maybe a] -> Maybe a
maximumMB [] = Nothing
maximumMB [Just x] = Just x 
maximumMB l@(Just x:Just x1:xs)
    | maximum (valorMaybe l) == x = Just x 
    | otherwise = maximumMB (Just x1:xs)

valorMaybe :: (Num a) => [Maybe a] -> [a]
valorMaybe [] = [] 
valorMaybe (Just x:resto) = x : valorMaybe resto  
valorMaybe (Nothing:resto) = valorMaybe resto 

maximumMB' :: (Ord a) => [Maybe a] -> Maybe a
maximumMB' [] = Nothing
maximumMB' (x:xs) = max x (maximumMB' xs) -- a funcao max recebe dois valores e compara os, entregando o maior deles, em haskell da para comparar valores do tipo maybe, nao é necessário extrair os valores para os comparar.

data LTree a = Tip a | Fork (LTree a) (LTree a)

listaLT :: LTree a -> [a]
listaLT (Tip x) = [x] 
listaLT (Fork esq dir) = listaLT esq ++ listaLT dir 

instance Show a => Show (LTree a) where
    show tree = profundidadeShow 0 tree  -- Começamos com nível 0.

profundidadeShow :: (Show a) => Int -> LTree a -> String
profundidadeShow n (Tip x) = replicate n '.' ++ show x ++ "\n"
profundidadeShow n (Fork esq dir) = profundidadeShow (n+1) esq ++ profundidadeShow (n+1) dir 

tree1 = Fork (Fork (Tip 7) (Tip 1)) (Tip 2)

type RelP a = [(a,a)]
type RelL a = [(a,[a])]
type RelF a = ([a], a->[a])

convPL :: (Eq a) => RelP a -> RelL a
convPL [] = []
convPL ((x,y):cauda) = insere (x,y) (convPL cauda)

insere :: (Eq a) => (a,a) -> RelL a -> RelL a
insere (x,y) [] = [(x,[y])]
insere (x,y) ((h,lista):t) 
    | x == h    = ((x,y:lista)) : t 
    | otherwise = (h,lista) : insere (x,y) t -- Continuar a procurar

convFP :: (Eq a) => RelF a -> RelP a 
convFP ([], f) = []
convFP (x:xs, f) = geraPares x (f x) ++ convFP (xs, f)

geraPares :: a -> [a] -> [(a,a)]
geraPares x [] = []
geraPares x (d:ds) = (x, d) : geraPares x ds

-- Definição da função f no arquivo exame1819.hs
f :: Int -> [Int]
f 1 = [3,4]
f 2 = [1,4,5]
f 3 = [7]
f 4 = [7]
f 5 = [7]
f 6 = [5]
f 7 = [6]
f _ = [] 

convPF :: (Eq a) => RelP a -> RelF a
convPF l = (limpaDuplicados (primeiros l), f)
  where
    f x = procuraDestinos x l

-- Extrai todas as primeiras componentes
primeiros :: [(a, a)] -> [a]
primeiros [] = []
primeiros ((x, y):xs) = x : primeiros xs

-- Remove duplicados para a lista de origens ser limpa
limpaDuplicados :: (Eq a) => [a] -> [a]
limpaDuplicados [] = []
limpaDuplicados (x:xs)
    | x `elem` xs = limpaDuplicados xs
    | otherwise   = x : limpaDuplicados xs

-- A função que a RelF vai usar internamente
-- A segunda coordenada do RelF a. 
procuraDestinos :: (Eq a) => a -> RelP a -> [a]
procuraDestinos _ [] = []
procuraDestinos v ((x, y):xs)
    | v == x    = y : procuraDestinos v xs
    | otherwise = procuraDestinos v xs 

r = [(1,3),(1,4),(2,1),(2,4),(2,5),(3,7),(4,7),(5,7),(6,5),(7,6)]