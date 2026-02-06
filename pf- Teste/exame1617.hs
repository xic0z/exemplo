-- Junta todas as strings da lista numa só, separando-as
-- pelo caracter ’\n’. Por exemplo, unlines ["Prog", "Func"] == "Prog\nFunc".

unlines1 :: [String] -> String
unlines1 [] = ""
unlines1 [h] = h
unlines1 (h:t) = h ++ "\n" ++ unlines1 t

removeOc:: (Eq a) => [a] -> [a] -> [a]
removeOc [] [] = []
removeOc l [] = l
removeOc [] _ = [] 
removeOc (h:t) (x:xs) = removeOc (deleteOnce x (h:t)) (xs) 

deleteOnce :: Eq a => a -> [a] -> [a]
deleteOnce _ [] = []
deleteOnce x (h:t)
    | x == h = t 
    | otherwise = h : deleteOnce x t 

data Seq a = Nil | Inicio a (Seq a) | Fim (Seq a) a

primeiro :: Seq a -> a
primeiro (Inicio x _) = x 
primeiro (Fim Nil x) = x
primeiro (Fim resto x) = primeiro resto 

semUltimo :: Seq a -> Seq a
semUltimo (Fim resto x) = resto 
semUltimo (Inicio x Nil) = Nil 
semUltimo (Inicio x resto) = Inicio x (semUltimo resto)

data BTree a = Empty | Node a (BTree a) (BTree a)
               deriving Show 
tree = Node 10 (Node 5 (Node 2 Empty Empty) Empty) (Node 15 Empty (Node 20 Empty Empty))

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune n (Node x esq dir) = Node x (prune (n-1) esq) (prune (n-1) dir) 

semMinimo :: (Ord a) => BTree a -> BTree a
semMinimo (Node x Empty dir) = dir 
semMinimo (Node x esq dir) = Node x (semMinimo esq) dir 

type Tabuleiro = [String]

exemplo :: Tabuleiro
exemplo = ["..R.",
           "R...",
           "...R",
           ".R.."] 

posicoes :: Tabuleiro -> [(Int,Int)]
posicoes tab = auxiliar tab 0 -- Começamos na linha 0

auxiliar :: Tabuleiro -> Int -> [(Int,Int)]
auxiliar [] _ = []
auxiliar (h:t) linha = 
    let coluna = ondeEstaOR h 0
    in (coluna, linha) : auxiliar t (linha + 1)

ondeEstaOR :: String -> Int -> Int
ondeEstaOR (h:t) col
    | h == 'R'  = col          
    | otherwise = ondeEstaOR t (col + 1) 

valido :: Tabuleiro -> Bool
valido tab = validaPosicoes (posicoes tab)

-- Esta função agora só lida com a lista de (Int, Int)
validaPosicoes :: [(Int,Int)] -> Bool
validaPosicoes [] = True -- Se não há rainhas, está tudo bem
validaPosicoes (q:qs) = 
    if atacaAlguem q qs      -- A primeira rainha (q) ataca alguma das outras (qs)?
    then False         -- Se sim, o tabuleiro é inválido
    else validaPosicoes qs -- Se não, verificamos o resto das rainhas

-- Verifica se a rainha (c,l) ataca alguma rainha da lista
atacaAlguem :: (Int,Int) -> [(Int,Int)] -> Bool
atacaAlguem _ [] = False
atacaAlguem (c1,l1) ((c2,l2):t)
    | c1 == c2 = True                       -- Mesma coluna
    | c1 + l1 == c2 + l2 = True             -- Diagonal 1
    | c1 - l1 == c2 - l2 = True             -- Diagonal 2
    | otherwise = atacaAlguem (c1,l1) t