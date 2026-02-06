data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt
            deriving Show

-- Dada uma destas expressoes calcula o seu valor.
calcula :: ExpInt -> Int
calcula (Const x)           = x
calcula (Simetrico e)       = - (calcula e)
calcula (Mais e1 e2)        = (calcula e1) + (calcula e2)
calcula (Menos e1 e2)       = (calcula e1) - (calcula e2)
calcula (Mult e1 e2)        = (calcula e1) * (calcula e2)

-- Defina uma funcao de forma a que infixa (Mais (Const 3) (Menos (Const 2) (Const 5))) 
-- de como resultado "(3 + (2 - 5))".
infixa :: ExpInt -> String
infixa (Const num) = show num
infixa (Simetrico exp) = "(-(" ++ infixa exp ++ "))"
infixa (Mais a b) = '(':infixa a ++ " + " ++ infixa b ++ ")"
infixa (Menos a b) = '(':infixa a ++ " - " ++ infixa b ++ ")"
infixa (Mult a b) = '(':infixa a ++ " * " ++ infixa b ++ ")"

-- Defina uma outra funcao de conversao para strings de forma a que quando aplicada a expressao 
-- acima de como resultado "3 2 5 - +"
posfixa :: ExpInt -> String
posfixa (Const num) = show num
posfixa (Simetrico exp) = posfixa exp ++ " - "
posfixa (Mais a b) = posfixa a ++ " " ++ posfixa b ++ " + "
posfixa (Menos a b) = posfixa a ++ " " ++ posfixa b ++ " - "
posfixa (Mult a b) = posfixa a ++ " " ++ posfixa b ++ " * "

data RTree a = R a [RTree a]

--Soma os elementos da arvore.
soma :: Num a => RTree a -> a
soma (R e []) = e
soma (R e es) = e + sum (map soma es)

soma1 :: Num a => RTree a -> a
soma1 (R x filhos) = x + somaLista filhos
  where
    somaLista [] = 0
    somaLista (f:fs) = soma1 f + somaLista fs

-- Calcula a altura da arvore.
altura :: RTree a -> Int
altura (R e []) = 1
altura (R e es) = 1 + maximum (map altura es)

-- Remove de uma arvore todos os elementos a partir de uma determinada profundidade.
prune :: Int -> RTree a -> RTree a
prune 0 (R e filhos) = (R e [])
prune n (R e filhos) = R e (map (prune (n - 1)) filhos)

-- Gera a arvore simetrica.
mirror :: RTree a -> RTree a
mirror (R e []) = (R e [])
mirror (R e filhos) = R e (reverse (map mirror filhos))

-- Corresponde a travessia postorder da arvore.
postorder :: RTree a -> [a]
postorder (R e []) = [e]
postorder (R e filhos) = concat (map postorder filhos) ++ [e]

data LTree a = Tip a | Fork (LTree a) (LTree a)

-- Soma as folhas de uma arvore.
ltSum :: Num a => LTree a -> a
ltSum (Tip n) = n
ltSum (Fork a b) = ltSum a + ltSum b

-- Lista as folhas de uma arvore (da esquerda para a direita).
listaLT :: LTree a -> [a]
listaLT (Tip n) = [n]
listaLt (Fork a b) = listaLt a ++ listaLt b 

-- Calcula a altura de uma arvore.
ltHeight :: LTree a -> Int
ltHeight (Tip a) = 1 
ltHeight (Fork a b) = 1+ max (ltHeight a) (ltHeight b) 

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

data BTree a = Empty
             | Node a (BTree a) (BTree a)
          deriving Show

-- Separa uma arvore com informacao nos nodos e nas folhas em duas 
-- arvores de tipos diferentes.
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf b) = (Empty,Tip b)
splitFTree (No a esq dir) = (Node a esqB dirB, Fork esqL dirL)
    where
        (esqB,esqL) = splitFTree esq 
        (dirB,dirL) = splitFTree dir 

-- Sempre que as arvores sejam compatıveis as junta numa so.
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip b) = Just (Leaf b)
joinTrees (Node a esqB dirB) (Fork esqL dirL) = 
    case (joinTrees esqB esqL, joinTrees dirB dirL) of
         (Just e, Just d) -> Just (No a e d)
         _                -> Nothing
joinTrees _ _ = Nothing 