import Data.Char

-- Recebe uma lista e produz a lista em que
-- cada elemento e o dobro do valor correspondente na lista de entrada.
dobros :: [Float] -> [Float]
dobros [] = []
dobros (x:xs) = 2 * x : dobros xs

-- Calcula o numero de vezes que um caracter ocorre numa string.
numOcorre :: Char -> String -> Int
numOcorre x "" = 0
numOcorre c (h:t) = if c == h then 1 + numOcorre c t
                    else numOcorre c t 

-- Testa se uma lista so tem elementos positivos.
positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) = if h <= 0 then False
                  else positivos t 

-- Retira todos os elementos nao positivos de uma lista de inteiros.
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if h < 0 then soPos t
              else h : soPos t 

-- Soma todos os numeros negativos da lista de entrada.
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if h <= 0 then h + somaNeg t
                else somaNeg t 

-- Devolve os ultimos tres elementos de uma lista. 
-- Se a lista de entrada tiver menos de tres elementos, devolve a propria lista.
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt l = if length l <= 3 
            then l
            else tresUlt (tail l) 

-- Calcula a lista das segundas componentes dos pares.
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,y) : t) = y : segundos t 

-- Testa se um elemento aparece na lista como primeira componente de algum dos pares.
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros x ((a,b):t) = if x == a then True
                           else nosPrimeiros x t 

-- Soma uma lista de triplos componente a componente.
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):t) = (a+ra, b+rb, c+rc)
    where (ra,rb,rc) = sumTriplos t

-- Recebe uma lista de caracteres, e selecciona dessa lista os caracteres que sao algarismos.
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) = if isDigit h then h : soDigitos t
                  else soDigitos t 

-- Recebe uma lista de caracteres, e conta quantos desses caracteres sao letras minusculas.
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) = if isLower h then 1 + minusculas t
                   else minusculas t 

-- Recebe uma string e devolve uma lista com os algarismos que ocorrem nessa string, pela mesma ordem.
nums :: String -> [Int]
nums "" = []
nums (h:t) = if isDigit h then digitToInt h : nums t
             else nums t 

type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- Indica quantos monomios de grau n existem em p.
conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta x ((a,b):t) = if x == b then 1 + conta x t 
                    else conta x t 

-- Indica o grau de um polinomio.
grau :: Polinomio -> Int
grau [] = 0
grau ((a,b):t) = if b > grau t then b 
                 else grau t 

-- Selecciona os monomios com um dado grau de um polinomio.
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau x ((coef,grau):t) = if x == grau then (coef,grau) : selgrau x t 
                            else selgrau x t

-- Calcula a derivada de um polinomio.
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((coef, grau):t)
    | grau > 0  = (coef * fromIntegral grau, grau - 1) : deriv t
    | otherwise = deriv t 

-- Calcula o valor de um polinomio para uma dado valor de x.
calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula 0 (h:t) = 0
calcula x ((coef,grau):t) = coef * (x ^ grau) + calcula x t 

-- Retira de um polinomio os monomios de coeficiente zero.
simp :: Polinomio -> Polinomio
simp [] = []
simp ((coef,grau):t) = if coef == 0 then simp t
                       else (coef,grau) : simp t 

-- Calcula o resultado da multiplicacao de um monomio por um polinomio.           
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (coef,grau) ((coef1,grau1):t) = (coef*coef1, grau + grau1) : mult (coef,grau) t 

-- Dado um polinomio constroi um polinomio equivalente em que nao podem aparecer varios monomios com o mesmo grau.
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((c,g):t) = normalizaAux (c,g) (normaliza t)

normalizaAux :: Monomio -> Polinomio -> Polinomio
normalizaAux m [] = [m]
normalizaAux (cm,gm) ((c,g):t)
    | gm == g = (cm + c,g) : t
    | otherwise = (c,g) : normalizaAux (cm,gm) t

-- Soma dois polinomios de forma a que se os polinomios que recebe estiverem normalizados produz tambem
-- um polinomio normalizado.
soma :: Polinomio -> Polinomio -> Polinomio
soma p [] = p
soma [] p = p
soma ((c,g):t) p = somaAux (c,g) (soma t p)

somaAux :: Monomio -> Polinomio -> Polinomio
somaAux m [] = [m]
somaAux (cm,gm) ((c,g):t)
    | gm == g = (cm + c,g) : t
    | otherwise = (c,g) : somaAux (cm,gm) t

-- Calcula o produto de dois polinomios.
produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto (m:t) p = produtoAux m p ++ produto t p

produtoAux :: Monomio -> Polinomio -> Polinomio
produtoAux _ [] = []
produtoAux (cm, gm) ((c, g):t) = (cm * c, gm + g) : produtoAux (cm, gm) t

-- Ordena um polinomio por ordem crescente dos graus dos seus monomios.
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (h:t) = insere h (ordena t)

-- Função auxiliar: coloca um monómio no sítio certo da lista.
insere :: Monomio -> Polinomio -> Polinomio
insere m [] = [m]
insere (c1, g1) ((c2, g2):t)
    | g1 <= g2  = (c1, g1) : (c2, g2) : t
    | otherwise = (c2, g2) : insere (c1, g1) t

-- Testa se dois polinomios sao equivalentes.
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)