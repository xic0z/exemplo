import Data.Char 
import System.Random 

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices _ [] = []
elemIndices n (x:xs)
    | n == x    = 0 : map (+1) (elemIndices n xs)
    | otherwise = map (+1) (elemIndices n xs)

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] [] = True
isSubsequenceOf [] _  = True 
isSubsequenceOf _ []  = False
isSubsequenceOf (x:xs) (h:t)
    | x == h = isSubsequenceOf xs t
    | otherwise    = isSubsequenceOf (x:xs) t 

data BTree a = Empty | Node a (BTree a) (BTree a)

lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAp _ Empty = Nothing
lookupAP n (Node (x,y) esq dir)
    | n == x = Just y 
    | n < x = lookupAP n esq 
    | otherwise = lookupAP n dir 

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c 
zipWithBT f Empty Empty = Empty
zipWithBT f tree Empty = Empty
zipWithBT f Empty tree = Empty
zipWithBT f (Node x esq dir) (Node n esq1 dir1) = Node (f x n) (zipWithBT f esq esq1) (zipWithBT f dir dir1)

digitAlpha :: String -> (String,String)
digitAlpha "" = ("","")
digitAlpha (x:xs)
    | isDigit x = (x : numeros,letras)
    | otherwise = (numeros,x:letras)
  where 
    (numeros,letras) = digitAlpha xs 

data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)

firstSeq :: Seq a -> a 
firstSeq (Cons a _)   = a
firstSeq (App Nil s2) = firstSeq s2 -- Se o que está à esquerda é nada, ignora e vai para a direita
firstSeq (App s1 s2)  = firstSeq s1 -- Se o que está à esquerda NÃO é Nil (é outro App ou Cons), vai para a esquerda

tamanho :: Seq a -> Int
tamanho Nil = 0
tamanho (Cons _ s) = 1 + tamanho s
tamanho (App s1 s2) = tamanho s1 + tamanho s2

dropSeq :: Int -> Seq a -> Seq a
dropSeq n s | n <= 0 = s
dropSeq _ Nil = Nil
dropSeq n (Cons x s) = dropSeq (n-1) s
dropSeq n (App s1 s2)
    | n < tam1  = App (dropSeq n s1) s2
    | otherwise = dropSeq (n - tam1) s2
  where tam1 = tamanho s1


instance Show a => Show (Seq a) where
    show Nil = "<>"
    show s   = "<<" ++ showInner s ++ ">>"

-- Esta função só trata do conteúdo, sem os símbolos das pontas
showInner :: Show a => Seq a -> String
showInner Nil = ""
showInner (Cons x Nil) = show x
showInner (Cons x s)   = show x ++ "," ++ showInner s
showInner (App Nil s2) = showInner s2
showInner (App s1 Nil) = showInner s1
showInner (App s1 s2)  = showInner s1 ++ "," ++ showInner s2

type Mat a = [[a]]

getElem :: Mat a -> IO a
getElem mat = do
    let listaSimples = concat mat 
    let tamanho = length listaSimples
    indiceAleat <- randomRIO (0,tamanho-1)
    return (listaSimples !! indiceAleat)