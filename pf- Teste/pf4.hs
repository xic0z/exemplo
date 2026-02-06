import Data.Char
import Data.List 
-- Dada uma string, devolve um par de strings: uma apenas com as letras presentes nessa string, e a outra
-- apenas com os numeros presentes na string.
digitAlpha :: String -> (String,String)
digitAlpha "" = ("","")
digitAlpha (h:t)
    | isAlpha h = (  digits, h:letters)
    | isDigit h = (h:digits,   letters)
    | otherwise = (  digits,   letters)
    where (digits, letters) = digitAlpha t

-- Dada uma lista de inteiros, conta o numero de valores nagativos, o numero de zeros e o numero de valores positivos,
-- devolvendo um triplo com essa informacao.
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t)
    | h < 0 = (negativos+1,zeros,positivos)
    | h == 0 = (negativos,zeros+1,positivos)
    | h > 0 = (negativos,zeros,positivos+1) 
    where (negativos,zeros,positivos) = nzp t 

-- Calcula simultaneamente a divisao e o resto da divisao inteira por subtraccoes sucessivas.
divMod1 :: Integral a => a -> a -> (a, a)
divMod1 dividendo divisor
    | dividendo < divisor  = (0, dividendo)
    | dividendo >= divisor = (1 + quociente, resto)
    where (quociente, resto) = divMod1 (dividendo - divisor) divisor

-- Utilizando uma funcao auxiliar com um acumulador, optimize seguinte definicao recursiva 
-- que determina qual o numero que corresponde a uma lista de digitos.
fromDigits :: [Int] -> Int
fromDigits l = fromDigitsAux l 0

fromDigitsAux :: [Int] -> Int -> Int
fromDigitsAux [] acc = acc
fromDigitsAux (h:t) acc = fromDigitsAux t (h + 10 * acc)

-- Criar uma função que conta quantos elementos tem uma lista (como o length), mas usando um acumulador.
length1 :: [a] -> Int
length1 l = lengthAux l 0

lengthAux :: [a] -> Int -> Int
lengthAux [] acc = acc   
lengthAux (h:t) acc = lengthAux t (acc+1) 

-- Somar todos os números de uma lista.
soma :: [Int] -> Int
soma l = somaAux l 0

somaAux :: [Int] -> Int -> Int
somaAux [] acc = acc
somaAux (h:t) acc = somaAux t (acc + h)

-- aux [5, 10, 2] 0 → Vê o 5. O novo acumulador é 0+5=5.
-- aux [10, 2] 5 → Vê o 10. O novo acumulador é 5+10=15.
-- aux [2] 15 → Vê o 2. O novo acumulador é 15+2=17.
-- aux [] 17 → A lista acabou. Devolve o resultado final: 17.

-- O Inversor de Listas.
meuReverse :: [a] -> [a]
meuReverse lista = aux lista [] 

aux :: [a] -> [a] -> [a]
aux [] acc    = acc             
aux (h:t) acc = aux t (h:acc)   

filtraPares :: [Int] -> [Int]
filtraPares l = filtraParesAux l []

filtraParesAux :: [Int] -> [Int] -> [Int]
filtraParesAux [] acc = reverse acc
filtraParesAux (h:t) acc = if mod h 2 == 0 then filtraParesAux t (h : acc)
                           else filtraParesAux t (acc)

-- Mínimo e Máximo (Dois Acumuladores).
-- Começamos com o primeiro elemento (h) a ser tanto o min como o max.
minEmax :: [Int] -> (Int, Int)
minEmax (h:t) = minEmaxAux t h h 

-- Caso base: a lista acabou, entregamos o par com os recordes finais.
-- Caso recursivo: comparamos o elemento atual 'h' com os nossos recordes.
minEmaxAux :: [Int] -> Int -> Int -> (Int, Int)
minEmaxAux [] mi ma = (mi, ma)
minEmaxAux (h:t) mi ma = 
    let novoMin = if h < mi then h else mi
        novoMax = if h > ma then h else ma
    in minEmaxAux t novoMin novoMax

fib :: Int -> Int
fib n = fibAux n 0 1  -- Começamos com 0 e 1 nas mãos

-- fibAux passos_que_faltam mao_esquerda mao_direita
fibAux 0 acc1 acc2 = acc1  -- Se não há mais passos, o resultado é a mao_esquerda
fibAux n acc1 acc2 = fibAux (n-1) acc2 (acc1 + acc2)

