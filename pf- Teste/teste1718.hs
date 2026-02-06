import System.Random

insert :: Ord a => a -> [a] -> [a]
insert n [] = n : [] 
insert n (x:xs)
    | n < x = n : (x:xs)
    | otherwise = x : insert n xs 

catMaybes :: [Maybe a] -> [a]
catMaybes [] = [] 
catMaybes (Just x:resto) = x : catMaybes resto
catMaybes (Nothing:resto) = catMaybes resto 

data Exp a = Const a
           | Var String
           | Mais (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

instance Show a => Show (Exp a) where
    show (Const x) = show x 
    show (Var s) = s 
    show (Mais x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (Mult x y) = "(" ++ show x ++ " * " ++ show y ++ ")"  

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn p [] = []
sortOn p (x:xs) = insertOn p x (sortOn p xs)
  where
    -- Esta função insere o x no sítio certo da lista já ordenada
    insertOn p x [] = [x]
    insertOn p x (y:ys)
        | p x <= p y = x : y : ys -- por convencao os elementos sao ordenados do menor para o maior, e aqui fazemos x : y .ys pq a lista que estamos a inserir o elemento ja é uma lista ordenada.
        | otherwise  = y : insertOn p x ys

amplitude :: [Int] -> Int
amplitude [] = 0
amplitude [e] = 0
amplitude l@(x:x1:xs) = (maximum l - minimum l)

-- o objetivo é juntar os menos numeros no lado esq e os maiores no lado dir.
parte :: [Int] -> ([Int], [Int])
parte l = auxMelhor 1 lOrd
  where 
    lOrd = sort l
    len = length lOrd -- comprimento da lista ordenada.
    
    -- Esta função testa o corte 'n' e compara com os outros
    auxMelhor n lista
        | n == len - 1 = (take n lista, drop n lista) -- Último corte possível
        | somaAmp n < somaAmp (n+1) = (take n lista, drop n lista)
        | otherwise = auxMelhor (n+1) lista
        where
            -- Calcula a soma das amplitudes para um corte n
            somaAmp i = amplitude (take i lista) + amplitude (drop i lista)

sort :: Ord a => [a] -> [a] 
sort [] = []
sort (x:xs) = insert x (sort xs)  -- Ordena o resto e DEPOIS insere o x

data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]
            deriving (Show,Eq)

ex :: Imagem
ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5),
                          Quadrado 4,
                          Mover (4,3) (Quadrado 2)])

conta :: Imagem -> Int
conta (Quadrado x) = 1 
conta (Mover (x,y) i) = conta i
conta (Juntar []) = 0
conta (Juntar (i:ims)) = conta i + conta (Juntar ims)  

-- Recebe (Contador, Imagem) e devolve (NovoContador, ImagemAlterada)
substitui :: Int -> Imagem -> (Int, Imagem)

-- Caso do Quadrado:
substitui n (Quadrado x)
    | n == 1    = (0, Juntar [])  -- Encontrou o escolhido! Apaga e põe o contador a 0
    | otherwise = (n - 1, Quadrado x) -- Não é este, mas "gastámos" um na contagem

-- Caso do Mover:
substitui n (Mover pos img) = 
    let (n1, imgNova) = substitui n img
    in (n1, Mover pos imgNova)

-- Caso do Juntar (O mais difícil):
substitui n (Juntar []) = (n, Juntar [])
substitui n (Juntar (i:ims)) =
    let (n1, iNova)   = substitui n i           -- Tenta apagar na primeira imagem
        (n2, imsNova) = auxJuntar n1 ims        -- Tenta apagar nas restantes usando o n que sobrou
    in (n2, Juntar (iNova : imsNova))
  where
    -- Função auxiliar para percorrer a lista de imagens dentro do Juntar
    auxJuntar n [] = (n, [])
    auxJuntar n (x:xs) = 
        let (nx, xNova)   = substitui n x
            (nxs, xsNova) = auxJuntar nx xs
        in (nxs, xNova : xsNova)

apaga :: Imagem -> IO Imagem
apaga img = do
    let total = conta img  -- Primeiro, contamos quantos existem
    if total == 0 
       then return img     -- Se não houver quadrados, não apaga nada
       else do
           n <- randomRIO (1, total) -- "Sorteamos" o número n
           return (snd (substitui n img)) -- Chamamos a função que remove o n-ésimo