import System.Random 

inits:: [a] -> [[a]]
inits [] = []
inits [e] = [[],[e]]
inits (x:xs) = [] : map (x:) (inits xs)  

isPrefixOf:: Eq a => [a] -> [a] -> Bool
isPrefixOf [] [] = True
isPrefixOf [] _  = True
isPrefixOf _ []  = False
isPrefixOf (x:xs) (h:t)
    | x == h = isPrefixOf (x:xs) t 
    | otherwise = isPrefixOf xs t 

data BTree a = Empty
             | Node a (BTree a) (BTree a)
          deriving Show

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node n Empty Empty) = 1 
folhas (Node n esq dir) = 1 + folhas esq + folhas dir 

path :: [Bool] -> BTree a -> [a]
path [] (Node n esq dir)= [n]
path l Empty = []
path (x:xs) (Node n esq dir)
    | x == False = n : path xs esq 
    | otherwise  = n : path xs dir 

tree = Node 10 (Node 5 (Node 2 Empty Empty) Empty) (Node 15 Empty (Node 20 Empty Empty))

type Polinomio = [Coeficiente]
type Coeficiente = Float

valor :: Polinomio -> Float -> Float
valor p x = aux p 0 x
  where
    aux [] _ _ = 0
    aux (c:cs) exp x = c * (x ^ exp) + aux cs (exp + 1) x

-- p = [x0, x1, x2, x3...]
-- A derivada de x0 desaparece, o x1 vira o novo x0, o x2 vira o novo x1...
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv (x:xs) = aux xs 1  -- ignoramos o primeiro x (expoente 0), como comeca no 1 o 1 sera o novo 0.

aux [] _ = []
aux (c:cs) exp = (c * exp) : aux cs (exp + 1)

soma :: Polinomio -> Polinomio -> Polinomio
soma [] [] = []
soma l [] = l
soma [] l = l 
soma (x:xs) (y:ys) = (x + y) : soma xs ys

type Mat a = [[a]]

quebraLinha :: [Int] -> [a] -> [[a]]
quebraLinha [] _ = []
quebraLinha l [] = []
quebraLinha (x:xs) l@(y:ys) = (take x l):quebraLinha xs (drop x l)

-- geraMat (Linhas, Colunas) (Mínimo, Máximo)
geraMat :: (Int, Int) -> (Int, Int) -> IO (Mat Int)
geraMat (0, colunas) _ = return [] -- Se não queres linhas, a matriz é vazia
geraMat (linhas, colunas) (a, b) = do
    linha  <- geraUmaLista colunas (a, b)    -- Cria 1 linha com 'y' colunas
    resto  <- geraMat (linhas-1, colunas) (a, b) -- Faz o resto das 'x-1' linhas
    return (linha : resto)

-- Função auxiliar para criar cada linha individualmente
geraUmaLista :: Int -> (Int, Int) -> IO [Int]
geraUmaLista 0 _ = return []
geraUmaLista n (a, b) = do
    valor <- randomRIO (a, b)            -- Sorteia 1 número
    proximos <- geraUmaLista (n-1) (a, b) -- Sorteia os outros n-1
    return (valor : proximos)            -- Junta tudo: [1, 5, 3...]

