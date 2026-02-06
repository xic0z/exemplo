
import Data.Char 
import System.Random 
-- 50 questões --
-- | 1 
enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 start end 
    | start > end = []
    | otherwise = start : enumFromTo1 (start + 1) end

-- | 2
enumFromThenTo1 :: Int -> Int -> Int -> [Int]
enumFromThenTo1 start next end
    | start > end && next >= start || start < end && next < start = []
    | otherwise = start : enumFromThenTo1 next (2 * next - start) end

-- | 3 
conc1 :: [a] -> [a] -> [a]
conc1 [] l = l
conc1 (h:t) l = h : conc1 t l 

-- | 4
encontraElemento :: [a] -> Int -> a
encontraElemento (h:_) 0 = h
encontraElemento (_:t) n = encontraElemento t (n - 1)

-- | 5
reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (h:t) = reverse1 t ++ [h]

-- | 6
take1 :: Int -> [a] -> [a]
take1 _ [] = []
take1 n (h:t)
    | n <= 0 = []
    | otherwise = h : take1 (n - 1) t

-- | 7


stringToInt :: String -> Int 
stringToInt (x:xs) = aux xs (digitToInt x)
   where aux :: String -> Int -> Int 
         aux (h:t) ac = aux t (ac*10 + (digitToInt h))
         aux [] ac = ac 

-- procura-se o minimo elemento na sub arv direita, remove-se o minimo e passa a ser o pai e procura-se o maximo elemento na sub arv esq
--remove :: Ord a => a -> BTree a -> BTree 
--remove _ Empty = Empty
--remove x (Node i e d)
   --      | x < i = Node i (remove x e) d
   --      | x > i = Node i e (remove x d)
   --      | x == i = case (e,d) of 
   --                   (Empty, d) -> d
   --                   (e, Empty) -> e 
   --                              -> let m = minimo d
   --                                     d' = semMinimo d 
   --                                 in Node m l d' 

-- FICHA 7
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt 
            deriving Show 

-- "7+4x5"
--e :: ExpInt
--e = Mais (Const 7) (Mult (Const 4) (Const 5))
--e' = (Const 7) 'Mais' ((Const 4) 'Mult' (Const 5))

calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico e) = - (calcula e) -- passar o valor da expressao para um inteiro
calcula (Mais e1 e2) = (calcula e1) + (calcula e2)
calcula (Menos e1 e2) = (calcula e1) - (calcula e2)
calcula (Mult e1 e2) = (calcula e1) * (calcula e2) 

infixa :: ExpInt -> String
infixa (Const x) = show x -- converte qualquer tipo para String.
infixa (Simetrico e) = "-" ++ (infixa e)
infixa (Mais e1 e2) = "(" ++ (infixa e1) ++ "+" ++ (infixa e2) ++ ")"
infixa (Menos e1 e2) = "(" ++ (infixa e1) ++ "-" ++ (infixa e2) ++ ")"
infixa (Mult e1 e2) = "(" ++ (infixa e1) ++ "*" ++ (infixa e2) ++ ")" 

--posfixa :: ExpInt -> String
--posfixa (Const x) = show x 
--posfixa (Simetrico e)
--posfixa (Mais e1 e2)
--posfixa (Menos e1 e2)
--posfixa (Mult e1 e2)

data RTree a = R a [RTree a]

soma :: Num a => RTree a -> a
soma (R i l) = i + sum (map soma l) -- vai aplicar a funcao soma a todas as listas, tenho de somar o somatório de cada lista.

altura :: RTree a -> Int
altura (R _ []) = 1 -- se eu tiver um pai e dps tiver uma lista vazia de filhos, a altura é 1.
altura (R i l) = 1 + maximum (map altura l) -- a altura é o valor maior das alturas.

prune :: Int -> RTree a -> RTree a
prune 1 (R i _) = R i []
prune n (R i l) | n > 0 = R i (map (prune (n-1)) l) -- l é a lista de filhos.

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

calcFib :: IO ()
calcFib = do
    putStr "Introduza o número que pretende calcular: "
    n <- getLine
    let r = fib (read n)
    putStr ("O valor é: " ++ show r) 

sum1 :: Integer -> Integer -> Integer
sum1 x y = x + y 

calcSum :: IO ()
calcSum = do
    putStr "Caro Francisco, insira o primeiro número: "
    n1 <- getLine
    putStr "Agora insira o segundo número: "
    n2 <- getLine
    let r = sum1 (read n1) (read n2)
    putStr ("O valor é: " ++ show r)

sub1 :: Integer -> Integer -> Integer
sub1 x y = x - y

calcSub :: IO ()
calcSub = do
    putStr "Caro utilizador, insira o primeiro dígito:"
    n1 <- getLine 
    putStr "Agora, o segundo dígito:"
    n2 <- getLine
    let r = sub1 (read n1) (read n2)
    putStr ("O valor é: " ++ show r) 

--Enunciado:
--Cria uma função mult1 que recebe dois inteiros e devolve o produto.
--Depois cria uma função calcMult :: IO () que pede dois números ao utilizador e mostra o resultado.

mult1 :: Int -> Int -> Int
mult1 x y = x * y

calcMult :: IO ()
calcMult = do
    putStr "Olá diga-me o primeiro número:"
    n1 <- getLine
    putStr "Agora o segundo número:"
    n2 <- getLine
    let r = mult1 (read n1) (read n2)
    putStr ("O valor é: " ++ show r) 

--Função que calcula a idade da pessoa
--Enunciado:
--Escreve uma função idade :: Integer -> Integer -> Integer que recebe o ano atual e o ano de nascimento e devolve a idade.
--Escreve também calcIdade :: IO () que pede esses dois valores ao utilizador e mostra a idade.

idade :: Integer -> Integer -> Integer
idade anoatual anonascimento = anoatual - anonascimento

calcIdade :: IO ()
calcIdade = do
    putStr "Olá diga-me o ano em que estamos:"
    n1 <- getLine
    putStr "Agora, diga-me em que ano nasceu:"
    n2 <- getLine
    let r = idade (read n1) (read n2)
    putStr ("O valor é: " ++ show r)

--Soma de três números
--Enunciado:
--Escreve uma função sum3 :: Integer -> Integer -> Integer -> Integer que soma três números.
--Depois faz a função calcSum3 que pergunta ao utilizador pelos três valores.

sum3 :: Integer -> Integer -> Integer -> Integer 
sum3 x y z = x + y + z

calcSum3 :: IO ()
calcSum3 = do
    putStr "Oi divo, diz me o primeiro número:"
    n1 <- getLine
    putStr "Oi novamente rei, diz me o segundo número:"
    n2 <- getLine
    putStr "Por fim lindo, diz me o terceiro número:"
    n3 <- getLine
    let r = sum3 (read n1) (read n2) (read n3)
    putStr ("Então amorzinho, o valor já calculado é: " ++ show r) 

--Função que calcula o dobro de um número
--Enunciado:
--Cria a função dobro :: Integer -> Integer, que devolve o dobro do número.
--A seguir, faz calcDobro, que pede um número ao utilizador e mostra o dobro.

dobro :: Integer -> Integer
dobro x = 2 * x 

calcDobro :: IO ()
calcDobro = do
    putStr "Oii, diz me um número:"
    n <- getLine
    let r = dobro (read n)
    putStr ("O dobro do número que me des-te é: " ++ show r) 

--Verificar se um número é positivo
--Enunciado:
--Cria a função positivo :: Integer -> Bool.
--Depois faz calcPositivo :: IO () que pede um número ao utilizador e diz se é positivo ou não.

positivo :: Integer -> Bool
positivo x = x >= 0 

calcPositivo :: IO ()
calcPositivo = do
    putStr "Oi lindo, dá-me um número:"
    n <- getLine
    let r = positivo (read n)
    putStr ("O número que me deste é positivo?: " ++ show r)

--Função que calcula o quadrado de um número
--Enunciado:
--Cria quadrado :: Integer -> Integer.
--Depois faz calcQuadrado que pede um número e mostra o quadrado.

quadrado :: Integer -> Integer
quadrado x = x ^ 2

calcQuadrado :: IO ()
calcQuadrado = do
    putStr "Oi divónico, dá me um número:"
    n <- getLine
    let r = quadrado (read n)
    putStr ("O número que me deste ao quadrado é: " ++ show r) 

--Média de dois números
--Enunciado:
--Cria media :: Float -> Float -> Float que devolve a média de dois valores.
--Depois faz calcMedia, que pede dois números ao utilizador (lembra-te que read para Float precisa de números com ponto, tipo 5.5).

media :: Float -> Float -> Float
media x y = (x + y) / 2

calcMedia :: IO ()
calcMedia = do
    putStr "Oi dá-me a nota do teu primeiro teste:"
    n <- getLine
    putStr "Agora a nota do teu segundo teste:"
    n1 <- getLine
    let r = media (read n) (read n1)
    putStr ("A tua nota final será: " ++ show r) 

--Converter minutos em segundos
--Enunciado:
--Cria minParaSeg :: Integer -> Integer que converte minutos para segundos.
--Depois faz calcMinSeg que pede ao utilizador um valor em minutos e mostra o resultado.

minParaSeg :: Integer -> Integer 
minParaSeg x = x * 60 

calcMinSeg :: IO ()
calcMinSeg = do
    putStr "Diga-me o valor de mins que quer passar para segundos:"
    n <- getLine
    let r = minParaSeg (read n)
    putStr ("O valor em segundos é: " ++ show r) 

--Função que diz se um número é par
--Enunciado:
--Cria ePar :: Integer -> Bool.
--Depois faz calcPar que pede um número e diz ao utilizador se é par ou ímpar.

ePar :: Integer -> Bool
ePar x = even x

calcPar :: IO ()
calcPar = do
    putStr "Diga-me o valor que quer verificar se é par:"
    n <- getLine
    let r = ePar (read n)
    putStr ("O valor é par:" ++ show r)

-- FICHA 8 --
-- Declare Exp a como uma instancia de Show.

data Exp a = Const1 a
           | Simetrico1 (Exp a)
           | Mais1 (Exp a) (Exp a)
           | Menos1 (Exp a) (Exp a)
           | Mult1 (Exp a) (Exp a)

showExp :: Show a => Exp a -> String
showExp (Const1 x) = show x          --- só consigo converter o tipo x em string se for uma instancia de show
showExp (Simetrico1 e) = "(" ++ "-" ++ (showExp e) ++ ")"
showExp (Mais1 e1 e2) = "(" ++ showExp e1 ++ "+" ++ showExp e2 ++ ")"
showExp (Menos1 e1 e2) = "(" ++ showExp e1 ++ "-" ++ showExp e2 ++ ")"
showExp (Mult1 e1 e2) = "(" ++ showExp e1 ++ "*" ++ showExp e2 ++ ")"

instance Show a => Show (Exp a) where
    show :: Show a => Exp a -> String
    show = showExp 

-- Declare Exp a como uma instancia de Eq.

calcula1 :: Num a => Exp a -> a 
calcula1 (Const1 x) = x
calcula1 (Simetrico1 e) = - (calcula1 e) -- passar o valor da expressao para um inteiro
calcula1 (Mais1 e1 e2) = (calcula1 e1) + (calcula1 e2)
calcula1 (Menos1 e1 e2) = (calcula1 e1) - (calcula1 e2)
calcula1 (Mult1 e1 e2) = (calcula1 e1) * (calcula1 e2) 

iguaisExp :: (Num a, Eq a) => Exp a -> Exp a -> Bool
iguaisExp e1 e2 = calcula1 e1 == calcula1 e2

instance (Num a, Eq a) => Eq (Exp a) where
    (==) = iguaisExp

-- Declare Exp a como instancia da classe Ord.

menorIgualExp :: (Num a, Ord a) => Exp a -> Exp a -> Bool
menorIgualExp e1 e2 = calcula1 e1 <= calcula1 e2

instance (Num a, Ord a) => Ord (Exp a) where
    (<=) = menorIgualExp 

-- Declare Exp a como instancia da classe Num.

maisExp e1 e2 = Const (calcula1 e1 + calcula1 e2)

multExp e1 e2 = Const (calcula1 e1 * calcula1 e2)

subExp e1 e2 = Const (calcula1 e1 - calcula1 e2)

absExp e = Const (abs (calcula1 e))

signumExp e = Const (signum (calcula e))

--fromIntegerExp :: Num a => Integer -> Exp a 
--fromIntegerExp e = Const (fromInteger e)

--instance (Num a) => Num (Exp a) where
 --   (+) :: Num a => Exp a -> Exp a -> Exp a
   -- (+) = maisExp
   -- (*) :: Num a => Exp a -> Exp a -> Exp a
   --- (*) = multExp
   -- (-) :: Num a => Exp a -> Exp a -> Exp a
   -- (-) = subExp
   -- abs :: Num a => Exp a -> Exp a
   -- abs = absExp
   -- signum :: Num a => Exp a -> Exp a
   -- signum = signumExp
   -- fromInteger :: Num a => Integer -> Exp a 
   -- fromInteger = fromIntegerExp

-- FICHA 9 --
-- teoria --
dialogo :: String -> IO String
dialogo s = putStr s >> getLine >>= (\r -> return r) -- (>>) executa a acao de mandar a string s para o ecra, para o utilizador receber a mensagem e dps faz o resto, fica à espera da resposta (>> - sequence), primeiro 

dialogo1 :: String -> IO String
dialogo1 s = do putStr s
                r <- getLine
                return r 

--calculaFib :: IO ()
--calculaFib = do  n <- dialogo "Introduza o número"
     --            i <- read n
      --           if i < 0 then do putStr "O fib não está definido para numeros negativos"
      --           else do let i' = fib i n 
        --                 putStr "O fib de "++ n ++"é" ++ (show i') 

calculaFib :: IO ()
calculaFib = do
    n <- dialogo "Introduza o número"
    i <- readIO n
    if i < 0 
        then putStrLn "O fib não está definido para números negativos"
        else do
            let r = fib i
            putStrLn ("O fib de " ++ show i ++ " é " ++ show r)

-- se eu invocar a funcao com um numero negativo, esta funcao vai fzr loop--
--Ficha 9--
datas :: IO (Int,Int,Int)
datas = do dia <- randomRIO (1,31) -- produza um valor entre 1-31
           mes <- randomRIO (1,12)
           ano <- randomRIO (2000,2025)
           return (dia,mes,ano)

bingo :: IO ()
bingo = do putStrLn "Começa!"
           l <- valores []
           putStrLn (show l)

--valores :: [Int] -> I0 [Int] -- se esta lista n tiver o comprimento 90 ainda n acabou
--valores l | length l == 90 = return
      --    | otherwise = do n <- randomRIO (1,90)
       --                    putStrLn (show n)
        --                   if n 'elem' l then
          --                 else valores (n:l)
