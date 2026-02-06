import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.Char 
-- Dada uma lista de listas, calcula a soma total do comprimento de todas as sublistas.
total1 :: [[a]] -> Int
total1 ([]) = 0
total1 (x:t) = length x + total1 (t) 

-- Recebe uma lista de triplos (3-uplos) e devolve uma lista de pares (2-uplos) contendo 
-- apenas o primeiro e o terceiro elemento de cada triplo original.
fun :: [(a,b,c)] -> [(a,c)]
fun ([]) = [] 
fun ((x,y,z):t) = (x,z) : fun t  

-- Percorre uma lista de triplos (onde o primeiro elemento é uma String) e concatena todas 
-- as Strings numa única sequência, ignorando os restantes elementos dos triplos.
cola :: [(String,b,c)] -> String
cola ([]) = ""
cola ((x,y,z):t) = x ++ cola t 

-- Recebe:
-- Um ano de referência (ou ano de nascimento).
-- Uma idade mínima.
-- Uma lista de pares com (Nome, Ano). A função deve devolver a lista dos nomes das pessoas que,
-- subtraindo o ano de referência ao seu ano registado, tenham uma idade superior ou igual à mínima estipulada.
idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ ([]) = []
idade a i ((nome,ano):t)
    | (a - ano) >= i = nome : idade a i t 
    | otherwise = idade a i t 

-- Recebe duas listas e retorna a sua união. O resultado deve conter todos os elementos da primeira lista seguidos
-- dos elementos da segunda lista que ainda não existam na primeira (evitando duplicados).
union :: Eq a => [a] -> [a] -> [a]
union l [] = l 
union l (h:t)
    | elem h l = union l t 
    | otherwise = union (l ++ [h]) t 

-- Recebe duas listas e devolve os elementos comuns às duas.
intersect1 :: Eq a => [a] -> [a] -> [a]
intersect1 [] (x:xs) = []
intersect1 (x:xs) [] = []
intersect1 (x:xs) l2
    | elem x l2 = x : intersect1 xs l2 
    | otherwise = intersect1 xs l2 

-- 50 QUESTOES --

-- 1| Constroi a lista dos numeros inteiros compreendidos entre dois limites.
enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 a b
    | a > b = []
    | otherwise = a : enumFromTo1 (a+1) b 

-- 2| Constroi a lista dos numeros inteiros compreendidos entre dois limites
-- e espacados de um valor constante.
enumFromThenTo1 :: Int -> Int -> Int -> [Int]
enumFromThenTo1 a b c 
    | a > c = [] 
    | otherwise = a : enumFromThenTo1 b (b+(b-a)) c 

-- 3| Concatena duas listas.
conc1 :: [a] -> [a] -> [a]
conc1 [] l = l                    
conc1 (x:xs) l = x : conc1 xs l   

-- 4| Dada uma lista e um inteiro, calcula o elemento da lista que se encontra nessa posicao 
-- (assume-se que o primeiro elemento se encontra na posicao 0).
procura1 :: [a] -> Int -> a
procura1 (x:xs) a
    | a == 0 = x
    | otherwise = procura1 xs (a-1)  

-- 5|
reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 [e] = [e]
reverse1 (x:xs) = reverse1 xs ++ [x]  

-- 6|
take1 :: Int -> [a] -> [a]
take1 _ [] = []
take1 n (x:xs)
    | n == 0 = []
    | otherwise = x : take1 (n-1) xs 

-- 7|
drop1 :: Int -> [a] -> [a]
drop1 _ [] = []
drop1 n (x:xs)
    | n == 0 = (x:xs)
    | otherwise = drop1 (n-1) xs 

-- 8|
zip1 :: [a] -> [b] -> [(a,b)]
zip1 [] [] = []
zip1 [] (x:xs) = []
zip1 (x:xs) [] = []
zip1 (x:xs) (y:ys) = [(x,y)] ++ zip1 xs ys 

-- 9|
replicate1 :: Int -> a -> [a]
replicate1 n x
    | n == 0 = []
    | otherwise = x : replicate1 (n-1) x 

-- 11|
group1 :: Eq a => [a] -> [[a]]
group1 [] = []
group1 [e] = [[e]]
group1 (x:x1:xs)
    | x == x1 = let (h:t) = group1 (x1:xs) 
                in (x:h) : t
    | otherwise = [x] : group1 (x1:xs) 

-- 12|
concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 [[e]] = [e]
concat2 (x:xs) = x ++ concat2 xs 

-- 13|
inits1 :: [a] -> [[a]]
inits1 [] = [[]]
inits1 (x:xs) = [] : map (x:) (inits1 xs)

-- 14|
tails1 :: [a] -> [[a]] 
tails1 [] = [[]]
tails1 (x:xs) = (x:xs) : tails1 xs 

-- 15|
heads :: [[a]] -> [a]
heads [] = []
heads ([]:xs) = heads xs
heads (x:xs) = head x : heads xs 

-- 16|
total :: [[a]] -> Int
total [] = 0
total (x:xs) = length x + total xs 

-- 20|
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n m
    | m == 0 = []
    | otherwise = 1 : map(*n) (powerEnumFrom n (m-1)) -- todos os elementos foram multiplicados por n

-- 21|
isPrime :: Int -> Bool
isPrime n 
    | n < 2     = False
    | otherwise = verif n 2
  where
    verif n m
        | m * m > n      = True   -- Já passámos a raiz quadrada e não houve divisores
        | mod n m == 0   = False  -- Encontrámos um divisor!
        | otherwise      = verif n (m + 1) -- Tenta o próximo número

isPrefixOf1 :: Eq a => [a] -> [a] -> Bool
isPrefixOf1 l [] = False 
isPrefixOf1 [] l = True
isPrefixOf1 (x:xs) (y:ys) 
    | x == y = isPrefixOf1 xs ys
    | otherwise = False 

isSuffixOf1 :: Eq a => [a] -> [a] -> Bool
isSuffixOf1 s l
    | s == l           = True   -- Se forem iguais agora, então s é sufixo de l
    | l == []          = False  -- Se a lista principal acabou e não eram iguais, então não é sufixo
    | otherwise        = isSuffixOf1 s (tail l) -- Tira o primeiro de l e tenta de novo

isSubsequenceOf1 :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf1 l [] = False
isSubsequenceOf1 [] l = True
isSubsequenceOf1 (x:xs) (y:ys)
    | x == y = isSubsequenceOf1 xs ys 
    | otherwise = isSubsequenceOf1 (x:xs) ys  

elemIndices1 :: Eq a => a -> [a] -> [Int]
elemIndices1 n [] = []
elemIndices1 n (x:xs)
    | n == x    = 0 : map (+1) (elemIndices1 n xs) -- +1 para andar para o indice seguinte, para ver se  n == x.
    | otherwise = map (+1) (elemIndices1 n xs)

nub1 :: Eq a => [a] -> [a]
nub1 [] = []
nub1 (x:xs) = x : nub1 (apagaTudo x xs)

apagaTudo :: Eq a => a -> [a] -> [a]
apagaTudo n [] = []
apagaTudo n (x:xs)
    | n == x = apagaTudo n xs 
    | otherwise = x : apagaTudo n xs 

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete n (x:xs)
    | n == x = xs
    | otherwise = x : delete n xs

-- A função que apaga uma lista de elementos
deleteList :: Eq a => [a] -> [a] -> [a]
deleteList l [] = l                  -- Nada para apagar? Devolve a lista.
deleteList l (y:ys) = deleteList (delete y l) ys

insert :: Ord a => a -> [a] -> [a]
insert e [] = [e]
insert e (x:xs)
    | e > x = x : insert e xs
    | otherwise = e : x : xs 

unwords1 :: [String] -> String
unwords1 [] = ""
unwords1 [x] = x
unwords1 (h:t) = h ++ " " ++ unwords1 t 

unlines1 :: [String] -> String
unlines1 [] = ""
unlines1 [x] = x
unlines1 (h:t) = h ++ "\n" ++ unlines1 t ++ "\n" 

pMaior :: Ord a => [a] -> Int
pMaior l@(x:xs)
    | maximum l == x = 0
    | otherwise = 1 + pMaior xs

lookup1 :: Eq a => a -> [(a,b)] -> Maybe b 
lookup1 _ [] = Nothing
lookup1 n ((x,x1):xs)
    | n == x = Just x1
    | otherwise = lookup1 n xs 

preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [e] = [e]
preCrescente (x:x1:xs)
    | x <= x1 = x : preCrescente (x1:xs)
    | otherwise = [x] 

iSort1 :: Ord a => [a] -> [a]
iSort1 [] = []
iSort1 [e] = [e]
iSort1 (x:xs) = insert x (iSort1 xs)

menor :: String -> String -> Bool
menor l@(h:t) l1@(x:xs) = length l < length l1 

elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet l1 ((l,n):xs)
    | l1 == l = True
    | otherwise = elemMSet l1 xs 

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((l,n):t)
    | n == 0 = converteMSet t 
    | otherwise = l : converteMSet ((l,n-1):t)

-- Exercícios de árvores binárias

data LTree a = Leaf a
             | Fork (LTree a) (LTree a)

selectLeaf :: String -> LTree a -> Maybe a
selectLeaf "" (Leaf x) = Just x 
selectLeaf _  (Leaf x) = Nothing -- Ainda sobra caminho mas acabou a árvore

-- Casos para quando encontras um Cruzamento (Fork esq dir)
selectLeaf ('0':resto) (Fork esq dir) = selectLeaf resto esq -- Para onde vais?
selectLeaf ('1':resto) (Fork esq dir) = selectLeaf resto dir -- Para onde vais?
selectLeaf ""          (Fork esq dir) = Nothing -- O caminho acabou num Fork 

buildLTree :: [(String, a)] -> LTree a
-- Caso base: se só há um elemento e o caminho acabou, é uma folha
buildLTree [("", x)] = Leaf x

-- Caso recursivo: criar um Fork
buildLTree lista = Fork (buildLTree esq) (buildLTree dir)
  where
    -- Na 'esq', queremos todos os (p,x) onde o caminho p começava por '0'
    -- Mas atenção: passamos apenas o resto do caminho (tail p)
    esq = [ (tail p, x) | (p, x) <- lista, head p == '0' ]
    
    -- Na 'dir', fazemos o mesmo para os que começam por '1'
    dir = [ (tail p,x) | (p,x) <- lista, head p == '1' ] 

minRem :: Ord a => [a] -> (a,[a])
minRem l@(x:xs) 
    | x == minimum l = (x,xs) 
    | otherwise = (m, delete m l)
  where m = minimum l 

-- ARVORES BINARIAS --
-- Exemplo de Dicionário (d1)
d1 :: Dictionary
d1 = [R ('c', Nothing) [
        R ('a', Nothing) [
            R ('r', Nothing) [
                R ('a', Just "...") [
                    R ('s', Just "...") [] 
                ],
                R ('o', Just "...") [],
                R ('r', Nothing) [
                    R ('o', Just "...") [] 
                ]
            ]
        ]
     ]]

type Dictionary = [ RTree (Char, Maybe String) ]

insere :: String -> String -> Dictionary -> Dictionary
insere [] _ dict = dict
insere (h:t) desc [] = [R (h, v_novo) (insere t desc [])]
  where v_novo = if null t then Just desc else Nothing

insere (h:t) desc (R (c, v) filhos : resto)
    | h == c = R (c, v_atualizado) (insere t desc filhos) : resto 
    | h < c = R (h, v_novo) (insere t desc []) : (R (c, v) filhos : resto)
    | otherwise = R (c, v) filhos : insere (h:t) desc resto 
  where 
    v_atualizado = if null t then Just desc else v
    v_novo       = if null t then Just desc else Nothing

data BTree a = Empty | Node a (BTree a) (BTree a) 

inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node r e d) = inorder e ++ (r:inorder d)

numera :: BTree a -> BTree (a,Int)
numera Empty = Empty 
numera (Node r e d) = snd (numeraAux 1 (Node r e d))

numeraAux :: Int -> BTree a -> (Int,BTree (a,Int))
numeraAux n Empty = (n,Empty)
numeraAux n (Node r e d) =
  let (n2, novaE) = numeraAux n e
      (n3, novaD) = numeraAux (n2 + 1) d
  in (n3, Node (r, n2) novaE novaD)

unInorder :: [a] -> [BTree a]
unInorder [] = [Empty]
unInorder lista = [ Node raiz esq dir 
                  | i <- [0..length lista - 1],       -- 1. Escolher um índice para ser a raiz
                    let (parteEsq, raiz:parteDir) = splitAt i lista, -- 2. Partir a lista
                    esq <- unInorder parteEsq,        -- 3. Gerar todas as subárvores esquerdas
                    dir <- unInorder parteDir         -- 4. Gerar todas as subárvores direitas
                  ]


a1 = Node 5 (Node 3 Empty Empty) (Node 7 Empty (Node 9 Empty Empty))

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove n (Node r e d)
    | n < r = Node  r (remove n e) d
    | n > r = Node r e (remove n d) 
    | otherwise = case (e, d) of
        (Empty, _) -> d 
        (_, Empty) -> e  
        _          -> let maxEsq = maisDireita e 
                      in Node maxEsq (remove maxEsq e) d

maisDireita :: Ord a => BTree a -> a 
maisDireita (Node r e Empty) = r 
maisDireita (Node r e d) = maisDireita d
maisDireita Empty = error "Árvore vazia não tem máximo"

instance Show a => Show (BTree a) where 
    show Empty = "*"
    show (Node r e d) = "(" ++ show e ++ "<-" ++ show r ++ "->"  ++ show d ++ ")" 

data RTree a = R a [RTree a] deriving (Show, Eq)

paths :: RTree a -> [[a]]
paths (R x []) = [[x]]
paths (R x subarvores) = map (x:) (concat (map paths subarvores)) -- para cada lista interna coloca o x.

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node r Empty Empty) = 1
folhas (Node r e d) = folhas e + folhas d 

path :: [Bool] -> BTree a -> [a]
path [] _ = []
path _ Empty = []
path (x:xs) (Node r e d)
    | x == True = r : path xs d
    | otherwise = r : path xs e

profundidade :: BTree a -> Int
profundidade Empty = 0
profundidade (Node r e d) = 1 + max (profundidade e) (profundidade d)

mapBTree :: (a -> b) -> BTree a -> BTree b
mapBTree f Empty = Empty
mapBTree f (Node r e d) = Node (f r) (mapBTree f e) (mapBTree f d)

filtraBST :: Ord a => a -> a -> BTree a -> [a]
filtraBST a b (Node r e d)
    | r < a = filtraBST a b d 
    | r > a = filtraBST a b e 
    | otherwise = filtraBST a b e ++ [r] ++ filtraBST a b d 

isBST :: Ord a => BTree a -> Bool
isBST Empty = True
isBST a@(Node r e d)
    | ordemCresc (inorder a) = True
    | otherwise = False 

inorder1 :: BTree a -> [a]
inorder1 Empty = []
inorder1 (Node r e d) = (inorder e) ++ (r:inorder d) 

ordemCresc :: Ord a => [a] -> Bool
ordemCresc [] = True
ordemCresc [_] = True 
ordemCresc (x:x1:xs)
    | x <= x1 && ordemCresc (x1:xs) = True
    | otherwise = False

jogo :: Int -> (Int, Int) -> IO ()
jogo n (a, b) = do
    lista <- replicateM n (randomRIO (a, b))
    putStrLn "Indique um número:"
    alvo <- readLn
    let encontrou = temSubSoma alvo lista
    putStrLn ("Lista gerada: " ++ show lista)
    if encontrou 
        then putStrLn "A propriedade verificou-se."
        else putStrLn "A propriedade não se verificou."

temSubSoma :: Int -> [Int] -> Bool
temSubSoma _ [] = False
temSubSoma s l = prefixoSoma s l || temSubSoma s (tail l)
  where
    prefixoSoma 0 _ = True
    prefixoSoma _ [] = False
    prefixoSoma v (x:xs) = prefixoSoma (v - x) xs

eco :: IO ()
eco = do 
    putStrLn "Escreva uma palavra: "
    palavra <- getLine
    let resposta = map toUpper palavra 
    putStrLn ("Ora aqui está o pedido: " ++ resposta)

dobro :: IO ()
dobro = do
      putStrLn "Escreva um número: "
      number <- readLn
      let answer = 2*number 
      putStrLn ("O dobro é: " ++ show answer)

seguranca :: IO ()
seguranca = do
          putStrLn "Escreva a sua idade here: "
          idade <- readLn
          let entrada = maisQueDezoito idade
          if entrada
                then putStrLn "Pode entrar!"
                else putStrLn "Entrada proibida!"

maisQueDezoito :: Int -> Bool
maisQueDezoito n = n >= 18  

-- Definindo o tipo Matriz como uma lista de listas
type Mat a = [[a]]

geraMat :: (Int, Int) -> (Int, Int) -> IO (Mat Int)
geraMat (x, y) (a, b) = do
    matriz <- replicateM x (replicateM y (randomRIO (a, b)))
    return matriz

geraChave :: IO [Int]
geraChave = do
    lista <- replicateM 6 (randomRIO (1, 49))
    if semRepeticoes lista
        then return lista     
        else geraChave         

semRepeticoes :: [Int] -> Bool
semRepeticoes [] = True
semRepeticoes (x:xs) = not (elem x xs) && semRepeticoes xs 

geraTabuleiro :: Int -> IO [[Char]]
geraTabuleiro n = do
    tabuleiro <- replicateM n (replicateM n escolheUm)
    return tabuleiro
  where
    escolheUm = do
        nAleatorio <- randomRIO (0, 1) :: IO Int
        return (if nAleatorio == 0 then '~' else 'P')

geraCeu :: Int -> Int -> IO [[Char]]
geraCeu x y = replicateM x geraLinha
  where
    -- Definimos o que é uma linha: y repetições de um símbolo
    geraLinha = replicateM y geraSimbolo
    
    -- Definimos o que é um símbolo: a lógica do aleatório
    geraSimbolo = do
        nAleatorio <- randomRIO (1, 10) :: IO Int
        if nAleatorio == 7 
            then return '*' 
            else return '.'

geraDígito :: IO Int
geraDígito = randomRIO (1,9)

geraUmaSenha :: Int -> IO [Int]
geraUmaSenha k = replicateM k (randomRIO (1,9))

geraVáriasSenhas :: Int -> Int -> IO [[Int]]
geraVáriasSenhas n k = replicateM n (geraUmaSenha k)

interfaceSenhas :: IO ()
interfaceSenhas = do
                putStrLn "Quantas senhas quer gerar?"
                n <- readLn
                putStrLn "Qual o tamanho de cada senha?"
                k <- readLn 
                senhas <- geraVáriasSenhas n k 
                putStrLn ("Aqui estão as senhas: " ++ show senhas) 

data FileSystem = File Nome | Dir Nome [FileSystem]
type Nome = String

fs1 = Dir "usr" [Dir "xxx" [File "abc.txt", File "readme", Dir "PF" [File "exemplo.hs"]],
           Dir "yyy" [], Dir "zzz" [Dir "tmp" [], File "teste.c"] ]

listaFich fs = do
    putStrLn "Que ficheiro procuras?"
    nome <- getLine
    let path = procuraCaminho fs nome -- Chama o "cérebro"
    if path == "" 
       then putStrLn "Ficheiro não encontrado!"
       else putStrLn ("Caminho: " ++ path)

procuraCaminho :: FileSystem -> Nome -> String
-- Caso 1: Encontras o ficheiro que queres
procuraCaminho (File n) nomeAlvo 
    | n == nomeAlvo = n
    | otherwise     = ""

-- Caso 2: Estás numa pasta
procuraCaminho (Dir n conteudos) nomeAlvo = 
    let caminhoResto = procuraNaLista conteudos nomeAlvo
    in if caminhoResto == "" 
       then "" 
       else n ++ "/" ++ caminhoResto

-- Função auxiliar para percorrer a lista de sub-pastas/ficheiros
procuraNaLista :: [FileSystem] -> Nome -> String
procuraNaLista [] _ = ""
procuraNaLista (f:fs) nomeAlvo = 
    let res = procuraCaminho f nomeAlvo
    in if res /= "" then res else procuraNaLista fs nomeAlvo

data Contacto = Casa Integer
              | Trab Integer
              | Tim Integer
              | Email String
            deriving (Show)

-- type Nome = String
type Agenda = [(Nome, [Contacto])]

consultaIO :: Agenda -> IO ()
consultaIO ag = do
              putStrLn "Qual o nome que pretende procurar?"
              nomeProcurado <- getLine
              let contacts = nomeParaContactos nomeProcurado ag
              putStrLn ("Os contactos associados a esse nome são: " ++ show contacts)

nomeParaContactos :: Nome -> Agenda -> [Contacto]
nomeParaContactos name ((n1,c):xs)
    | name == n1 = c
    | otherwise = nomeParaContactos name xs 

minhaAgenda = [("Joao", [Casa 212345678, Tim 912345678]), ("Maria", [Email "maria@email.com"]), ("Pedro", [Trab 219998887])]

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet _ [] = []
insereMSet n ((n1,i):t)
    | n == n1 = (n1,i+1) : insereMSet n t 
    | otherwise = (n1,i) : insereMSet n t 

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet n ((n1,i):t)
    | n == n1 = removeMSet n t 
    | otherwise = (n1,i) : removeMSet n t 

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (x:xs) = (x, 1 + length (takeWhile (== x) xs)) : constroiMSet (dropWhile (== x) xs) 

elementosRepetidos:: Ord a => [a] -> [a]
elementosRepetidos [] = []
elementosRepetidos [e] = []
elementosRepetidos (x:xs)
    | elem x xs = x : elementosRepetidos xs 
    | otherwise = elementosRepetidos xs 

partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' (x:xs) =
    case x of 
      Left a  -> (a : ls, rs)
      Right b -> (ls, b : rs)
   where (ls,rs) = partitionEithers' xs 

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) =
    case x of
        Just n -> n : catMaybes xs 
        Nothing -> catMaybes xs 

data Movimento = Norte | Sul | Este | Oeste
               deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi, yi) (xf, yf)
    | xi < xf = Este  : caminho (xi + 1, yi) (xf, yf)
    | xi > xf = Oeste : caminho (xi - 1, yi) (xf, yf)
    | yi < yf = Norte : caminho (xi, yi + 1) (xf, yf)
    | yi > yf = Sul   : caminho (xi, yi - 1) (xf, yf)
    | otherwise = [] -- Caso base: xi == xf e yi == yf

-- TESTE 2019 --
lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP _ Empty = Nothing
lookupAP n (Node (k,v) esq dir)
    | n < k = lookupAP n esq
    | n > k = lookupAP n dir
    | otherwise = Just v 

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty Empty = Empty
zipWithBT f Empty _ = Empty
zipWithBT f _ Empty = Empty
zipWithBT f (Node r e d) (Node r1 e1 d1) = Node (f r r1) (zipWithBT f e e1) (zipWithBT f d d1) 

digitAlpha :: String -> (String,String)
digitAlpha "" = ("","")
digitAlpha (x:xs)
    | isDigit x = (x:ns,ls)
    | otherwise = (ns,x:ls)
  where (ns,ls) = digitAlpha xs 

data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)

firstSeq :: Seq a -> a
firstSeq (Cons a seq) = a 
firstSeq (App Nil seq2) = firstSeq seq2 
firstSeq (App seq1 seq2) = firstSeq seq1 

instance Show a => Show (Seq a) where
    show s = "<<" ++ recheio s ++ ">>"
      where
        recheio Nil = ""
        recheio (Cons x Nil) = show x
        recheio (Cons x s)   = show x ++ "," ++ recheio s
        recheio (App s1 s2)  = 
            case (s1, s2) of
                (Nil, _) -> recheio s2
                (_, Nil) -> recheio s1
                _        -> recheio s1 ++ "," ++ recheio s2

data Playlist = Vazia 
              | Musica String 
              | Juncao Playlist Playlist

instance Show Playlist where
    show :: Playlist -> String
    show s = "[" ++ recheio1 s ++ "]"
      where
        recheio1 Vazia = "[]"
        recheio1 (Musica m) = show m 
        recheio1 (Juncao p1 p2) = 
            case (p1,p2) of
                 (Vazia,_) -> recheio1 p2
                 (_,Vazia) -> recheio1 p1
                 _         -> recheio1 p1 ++ " | " ++ recheio1 p2

p1 = Musica "Yesterday"
p2 = Juncao (Musica "Let It Be") (Musica "Help")

getElem :: Mat a -> IO a
getElem mat = do
    let listaSimples = concat mat
    let tamanho = length listaSimples
    indiceAleatorio <- randomRIO (0, tamanho - 1)
    return (listaSimples !! indiceAleatorio)

corAleatoria :: [String] -> IO String
corAleatoria cores = do
    let tamanho = length cores
    indiceAleatorio <- randomRIO (0,tamanho-1)
    return (cores!!indiceAleatorio)

lancaDado :: Int -> IO Int
lancaDado n = do
    resultado <- randomRIO (1,n)
    putStrLn ("Saiu o numéro: " ++ show resultado)
    return resultado 