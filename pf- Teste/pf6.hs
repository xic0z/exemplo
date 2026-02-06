data BTree a = Empty
             | Node a (BTree a) (BTree a)
          deriving Show

-- Calcula a altura da arvore.
altura :: BTree a -> Int
altura Empty = 0
altura (Node _ l r) = 1 + max (altura l) (altura r)

-- Calcula o numero de nodos da arvore.
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node _ l r) = 1 + contaNodos l + contaNodos r 

-- Calcula o numero de folhas (i.e., nodos sem descendentes da arvore).
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ l r) = folhas l + folhas r

-- Remove de uma arvore todos os elementos a partir de uma determinada profundidade.
prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune x (Node e l r) = Node e (prune (x - 1) l) (prune (x - 1) r)

-- Dado um caminho (False corresponde a esquerda e True a direita) e uma arvore, da a lista com a informacao dos nodos
-- por onde esse caminho passa.
path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node e _ _) = [e]
path (h:t) (Node e l r)
    | h == True = e : path t r
    | otherwise = e : path t l

-- Da a arvore simetrica.
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node e l r) = Node e (mirror r) (mirror l)

-- Que generaliza a funcao zipWith para arvores binarias.
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node v1 l1 r1) (Node v2 l2 r2) = Node (f v1 v2) (zipWithBT f l1 l2) (zipWithBT f r1 r2)
zipWithBT _ _ _ = Empty

-- Generaliza a funcao unzip (neste caso de triplos) para arvores binarias.
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (x,y,z) l r) = (Node x e1 d1, Node y e2 d2, Node z e3 d3)
  where 
    (e1, e2, e3) = unzipBT l  -- Aqui separamos as três árvores da esquerda
    (d1, d2, d3) = unzipBT r  -- Aqui separamos as três árvores da direita

-- Determina o menor elemento de uma arvore binaria de procura nao vazia.
minimo :: Ord a => BTree a -> a
minimo (Node x Empty r) = x
minimo (Node x l r) = minimo l  

-- Remove o menor elemento de uma arvore binaria de procura nao vazia.
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x Empty r) = (Node x Empty r)
semMinimo (Node x l r) = Node x (semMinimo l) r

-- Calcula, com uma unica travessia da arvore o resultado das duas funcoes anteriores.
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty r) = (x,r)
minSmin (Node x l r) = (a, Node x b r)
    where (a, b) = minSmin l

-- Remove um elemento de uma arvore binaria de procura, usando a funcao anterior.
remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node e l r) 
    | x < e = Node e (remove x l) r
    | x > e = Node e l (remove x r)
    | otherwise = case r of Empty -> l
                            _ -> let (g,h) = minSmin r in Node g l h

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving (Show,Eq)
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
    deriving Show
type Turma = BTree Aluno -- arvore binaria de procura (ordenada por numero).

-- Verifica se um aluno, com um dado numero, esta inscrito.
inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False -- Se a árvore acabar, não encontrou
inscNum n (Node (num, nome, reg, classif) l r) 
    | n == num = True           -- Encontrou!
    | n < num  = inscNum n l    -- Procura no "saco" da esquerda
    | n > num  = inscNum n r    -- Procura no "saco" da direita

-- Verifica se um aluno, com um dado nome, esta inscrito.
inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome n (Node (num, nome, reg, classif) l r)
    | n == nome      = True           -- 1. Está aqui?
    | inscNome n l   = True           -- 2. Se não, está na esquerda?
    | inscNome n r   = True           -- 3. Se não, está na direita?
    | otherwise      = False          -- 4. Então não existe.

-- Lista o numero e nome dos alunos trabalhadores-estudantes (ordenados por numero).
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nome,reg,_) l r)
    | reg == TE = alunosDaEsq ++ [(num,nome)] ++ alunosDaDir
    | otherwise = alunosDaEsq ++ alunosDaDir 
  where
    alunosDaEsq = trabEst l
    alunosDaDir = trabEst r 

-- Calcula a classificacao de um aluno (se o aluno nao estiver inscrito a funcao deve retornar Nothing).
nota :: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing
nota n (Node (num,nome,reg,classif) l r)
    | n == num = Just classif
    | n < num = (nota n l)
    | n > num = (nota n r) 
    | otherwise = Nothing 

-- Calcula a percentagem de alunos que faltaram a avaliacao.
percFaltas :: Turma -> Float
percFaltas turma = (fromIntegral totalFaltas / fromIntegral totalAlunos)* 100
    where (totalFaltas,totalAlunos) = contarFaltas turma

contarFaltas :: Turma -> (Int,Int)
contarFaltas Empty = (0,0)
contarFaltas (Node (_,_,_,classif) l r)
    | eFalta classif = (fe+fd+1,te+td+1)
    | otherwise = (fe+fd,te+td+1)
  where 
    (fe,te) = contarFaltas l
    (fd,td) = contarFaltas r 
    eFalta Faltou = True 
    eFalta _ = False 

-- Calcula a media das notas dos alunos que passaram.
mediaAprov :: Turma -> Float
mediaAprov turma 
    | nAprovados == 0 = 0
    | otherwise = fromIntegral somaNotas / fromIntegral nAprovados
  where (somaNotas, nAprovados) = contarAprov turma

contarAprov :: Turma -> (Int, Int)
contarAprov Empty = (0, 0)
contarAprov (Node (_, _, _, classif) l r)
    | nota > 0  = (nota + somaE + somaD, 1 + qtdE + qtdD)
    | otherwise = (somaE + somaD, qtdE + qtdD)
  where
    (somaE, qtdE) = contarAprov l
    (somaD, qtdD) = contarAprov r
    -- A tua função auxiliar simplificada:
    nota = extraiNota classif
    
    extraiNota (Aprov x) = x
    extraiNota _         = 0

-- Calcula o racio de alunos aprovados por avaliados. Implemente esta funcao fazendo apenas uma travessia da arvore.
aprovAv :: Turma -> Float
aprovAv Empty = 0
aprovAv turma = fromIntegral aprovados / fromIntegral avaliados
    where (aprovados,avaliados) = aprovAvAux turma

aprovAvAux :: Turma -> (Int,Int) -- (aprovados,avaliados)
aprovAvAux Empty = (0,0)
aprovAvAux (Node (_,_,_,classif) l r)
    | eAprov classif = (apE + apD + 1, avE + avD + 1)
    | eRep classif = (apE + apD, avE + avD + 1)
    | otherwise = (apE + apD, avE + apD)
  where
    eAprov (Aprov x) = True
    extraiNota _ = False 
    eRep Rep = True
    eRep _ = False 
    eFalta Faltou = True
    eFalta _ = False
    (apE,avE) = aprovAvAux l
    (apD,avD) = aprovAvAux r 