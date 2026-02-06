-- Calcula o perımetro de uma circunferencia, dado o comprimento
-- do seu raio.
perimCirc :: Float -> Float 
perimCirc r = 2 * pi * r 

-- Calcula a distancia entre dois pontos no plano Cartesiano. Cada ponto
-- e um par de valores do tipo Double.
dist :: (Float,Float) -> (Float,Float) -> Float
dist (x,y) (x1,y1) = sqrt ((x1 - x)^2 + (y1 - y)^2)

-- Que recebe uma lista e devolve um par com o primeiro e o ultimo
-- elemento dessa lista.
primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)

-- Tal que multiplo m n testa se o numero inteiro m e multiplo de n.
multiplo :: Int -> Int -> Bool
multiplo x y = mod x y == 0 

-- Que recebe uma lista e, se o comprimento da lista for ımpar retira-lhe 
-- o primeiro elemento, caso contrario devolve a propria lista.
truncaImpar :: [a] -> [a]
truncaImpar l = if even (length l)
                then l
                else tail l 

-- Que calcula o maior de dois numeros inteiros.
max2 :: Int -> Int -> Int
max2 x y = if x > y then x 
           else y 

-- Que calcula o maior de tres numeros inteiros, usando a funcao max2.
max3 :: Int -> Int -> Int -> Int
max3 x y z = if max2 x y > z then max2 x y
             else z

-- A funcao nRaizes que recebe os (3) coeficientes de um polinomio de 2º grau e que
-- calcula o numero de raızes (reais) desse polinomio.
nRaizes :: Float -> Float -> Float -> Float
nRaizes a b c
    | delta > 0 = 2
    | delta == 0 = 1
    | delta < 0 = 0
    where delta = b^2 - (4 * a * c)

-- A funcao raizes que, usando a funcao anterior, recebe os coeficientes do polinomio
-- e calcula a lista das suas raızes reais.
raizes :: Float -> Float -> Float -> [Float]
raizes a b c = 
    case nRaizes a b c of
        0 -> []
        1 -> [(-b) / (2 * a)]
        2 -> [((-b) + sqrt delta) / (2 * a), ((-b) - sqrt delta) / (2 * a)]
    where delta = b^2 - 4 * a * c


type Hora = (Int,Int)

-- Testar se um par de inteiros representa uma hora do dia valida.
validaHora :: Hora -> Bool
validaHora (x,y) = x >= 0 && x <= 23 && y >= 0 && y <= 59 

-- Testar se uma hora e ou nao depois de outra (comparacao).
comparaHora :: Hora -> Hora -> Bool
comparaHora (a,b) (x,y) = validaHora (a,b) && validaHora (x,y) && a > x || validaHora (a,b) && validaHora (x,y) && a == x && b > y

-- Converter um valor em horas (par de inteiros) para minutos (inteiro).
horaParaMins :: Hora -> Int
horaParaMins (x,y) = (x * 60) + y 

-- Converter um valor em minutos para horas.
minsParaHoras :: Int -> Hora
minsParaHoras min = (div min 60, mod min 60)

-- Calcular a diferenca entre duas horas (cujo resultado deve ser o numero de minutos).
difEntreHoras :: Hora -> Hora -> Int
difEntreHoras (a,b) (x,y) = abs (horaParaMins (a,b) - horaParaMins (x,y))

-- Adicionar um determinado numero de minutos a uma dada hora.
addMins :: Hora -> Int -> Hora
addMins (x,y) mins = minsParaHoras (horaParaMins (x,y) + mins)


data Semaforo = Verde | Amarelo | Vermelho
    deriving (Show,Eq)

-- Defina a funcao next, que calcula o proximo estado de um semaforo.
next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde 

-- Defina a funcao stop, que determina se e obrigatorio parar num semaforo.
stop :: Semaforo -> Bool
stop cor = cor == Vermelho

-- Defina a funcao safe, que testa se o estado de dois semaforos num cruzamento e seguro.
safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 = s1 == Vermelho || s2 == Vermelho


data Ponto = Cartesiano Double Double | Polar Double Double
    deriving (Show,Eq)

-- Calcula a distancia de um ponto ao eixo vertical.
posx:: Ponto -> Double
posx ponto = case ponto of Cartesiano x _ -> x
                           Polar d a -> if a == pi/2 then 0 else d * cos a

-- Calcula a distancia de um ponto ao eixo horizontal.
posy :: Ponto -> Double
posy ponto = case ponto of Cartesiano _ y -> y
                           Polar d a -> if a == pi then 0 else d * sin a 

-- Calcula a distancia de um ponto a origem.
raio :: Ponto -> Double
raio ponto = case ponto of Cartesiano x y -> sqrt ((-x)^2 + (-y)^2)
                           Polar d a -> d 

data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
        deriving (Show,Eq)

-- Testa se uma figura e um polıgono.
--poligono :: Figura -> Bool
--poligono (Circulo c r) = False
--poligono (Retangulo p1 p2) = posx p1 /= posx p2 && posy p1 /= posy p2 -- Verifica que os pontos não têm o mesmo valor de x ou y
--poligono (Triangulo p1 p2 p3) = (posy p2 - posy p1) / (posx p2 - posx p1) /= (posy p3 - posy p2) / (posx p3 - posx p2) -- Verifica que os pontos não pertencem todos à mesma reta

-- Calcula a lista dos vertices de uma figura.
--vertices :: Figura -> [Ponto]
--vertices (Circulo _ _) = []
--vertices retang@(Retangulo p1 p2) = if poligono retang then [p1, Cartesiano (posx p1) (posy p2), Cartesiano (posx p2) (posy p1), p2] else []
--vertices triang@(Triangulo p1 p2 p3) = if poligono triang then [p1, p2, p3] else []
