type MSet a = [(a,Int)]

cardMSet :: MSet a -> Int
cardMSet [] = 0
cardMSet [(l,n)] = n
cardMSet ((l,n):(l1,n1):resto) = n + n1 + cardMSet resto 

moda :: MSet a -> [a]
moda [] = []
moda [(l,n)] = l : [] 
moda ((l,n):(l1,n1):resto)
    | n == n1 = l : moda ((l1,n1):resto)
    | otherwise = l : [] 

converteMSet :: MSet a -> [a]
converteMSet [] = []
converteMSet ((l,n):resto) = (replicate n l) ++ converteMSet resto

-- 1. Coloca um par no sítio correto da lista.
insereOrdenado :: (a, Int) -> MSet a -> MSet a
insereOrdenado (elemento, n) [] = [(elemento, n)]
insereOrdenado (elemento, n) ((l, n1):resto)
    | n > n1    = (elemento, n) : (l, n1) : resto
    | otherwise = (l, n1) : insereOrdenado (elemento, n) resto

-- 2. Função que remove o elemento da lista (para ele não ficar duplicado)
removeElemento :: Eq a => a -> MSet a -> MSet a
removeElemento x [] = []
removeElemento x ((l, n):resto)
    | x == l    = resto
    | otherwise = (l, n) : removeElemento x resto

-- 3. A função principal
addNcopies :: Eq a => MSet a -> a -> Int -> MSet a
addNcopies mset x n = 
    let 
        -- Vê quanto o 'x' já tinha. Se não tinha nada, assume 0.
        valorAntigo = case lookup x mset of
                        Just v  -> v
                        Nothing -> 0
        
        -- Calcula o novo total de ocorrências
        novoTotal = valorAntigo + n
        
        -- Remove a versão "velha" do elemento da lista
        listaLimpa = removeElemento x mset
    in 
        -- Insere a versão "nova" no lugar correto por ordem de grandeza
        insereOrdenado (x, novoTotal) listaLimpa

data SReais = AA Double Double | FF Double Double
            | AF Double Double | FA Double Double
            | Uniao SReais SReais

instance Show SReais where
    show (AA x y) = "]" ++ show x ++ "," ++ show y ++ "["
    show (FF x y) = "[" ++ show x ++ "," ++ show y ++ "]"
    show (AF x y) = "]" ++ show x ++ "," ++ show y ++ "]"
    show (FA x y) = "[" ++ show x ++ "," ++ show y ++ "["
    show (Uniao i1 i2) = "(" ++ show i1 ++ " U " ++ show i2 ++ ")"

pertence :: Double -> SReais -> Bool
pertence n (AA x y) = n > x && n < y
pertence n (FF x y) = n >= x && n <= y
pertence n (AF x y) = n > x && n <= y
pertence n (FA x y) = n >= x && n < y 

tira :: Double -> SReais -> SReais
-- Casos de União: Aplicamos a remoção recursivamente em ambos os lados
tira n (Uniao a b) = Uniao (tira n a) (tira n b)

-- Caso FF [x, y]
tira n (FF x y)
    | n == x = AF x y                   -- Se tiras o início, fica aberto à esquerda
    | n == y = FA x y                   -- Se tiras o fim, fica aberto à direita
    | n > x && n < y = Uniao (FA x n) (AF n y) -- No meio: parte-se em dois
    | otherwise = FF x y                -- Fora do intervalo: não muda nada

-- Caso AF ]x, y]
tira n (AF x y)
    | n == y = AA x y                   -- Se tiras o fim (que era fechado), fica AA
    | n > x && n < y = Uniao (AA x n) (AF n y) -- No meio: parte em AA e AF
    | otherwise = AF x y

-- Caso FA [x, y[
tira n (FA x y)
    | n == x = AA x y                   -- Se tiras o início (que era fechado), fica AA
    | n > x && n < y = Uniao (FA x n) (AA n y) -- No meio: parte em FA e AA
    | otherwise = FA x y

-- Caso AA ]x, y[
tira n (AA x y)
    | n > x && n < y = Uniao (AA x n) (AA n y) -- No meio: parte em dois AA
    | otherwise = AA x y

data RTree a = R a [RTree a]
-- Uma árvore simples:
--      1
--    /   \
--   2     3
--        / \
--       4   5
arvoreExemplo = R 1 [R 2 [], R 3 [R 4 [], R 5 []]]

percorre :: [Int] -> RTree a -> Maybe [a]
percorre [] (R x lista) = Just [x] -- Se o caminho é vazio ([]), significa que chegaste ao destino final da tua viagem pela árvore. De acordo com o enunciado, a função deve dar a "lista de valores por onde esse caminho passa"
percorre (h:t) (R x lista)
    | (h < 1) || (h > length lista) = Nothing
    | otherwise = case percorre t (lista !! (h-1)) of
                    Nothing -> Nothing
                    Just res -> Just (x : res) 
