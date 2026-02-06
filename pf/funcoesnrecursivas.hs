-- | Exercício 1 
-- | a
perim :: Double -> Double
perim r = 2 * pi * r

-- | b
dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt ((x2-x1) ^ 2 + (y2-y1) ^ 2)

-- | c
primUlt :: [a] -> (a,a)
primUlt l = (head l ,last l) 

-- | d
multiplo :: Int-> Int -> Bool
multiplo m n = if mod m n == 0
               then True
               else False

-- | e
truncaImpar :: [a] -> [a]
truncaImpar b = if mod (length b) 2 == 1
                then tail b 
                else b 

-- | f
max2 :: Int -> Int -> Int
max2 x y = if x < y
           then y
           else x 

-- | g
max3 :: Int -> Int -> Int -> Int
max3 x y z = if max2 x y < z
              then z 
              else max2 x y

-- | Exercício 2
-- | a
nRaizes :: Double -> Double -> Double -> Int
nRaizes a b c = if delta == 0 then 1 
                else if delta < 0 then 0
                else 2
                where delta = b^2 - 4*a*c  

-- | b
raizes :: Double -> Double -> Double -> [Double] 
raizes a b c = if nRaizes a b c == 0 then [] 
               else if nRaizes a b c == 1 then [raiz1]   
               else [raiz1,raiz2]
               where (raiz1,raiz2) = (((-b + sqrt(delta))/ (2*a)), ((-b - sqrt(delta))/ (2*a)))
                     delta = b^2 - 4*a*c

-- | Exercício 3
-- | a
horaValida :: (Int,Int) -> Bool
horaValida (a,b) = if a < 0 then False
            else if a > 23 then False 
            else if b > 59 then False
            else if b < 0 then False
            else True
-- | ou

horaValida2 :: (Int,Int) -> Bool
horaValida2 (a,b) = a >= 0 && a <= 23 && b >= 0 && b <= 59 

-- | b
horaDepois :: (Int,Int) -> (Int,Int) -> Bool 
horaDepois (a,b) (c,d) = if a > c then True 
                         else if a == c && b > d then True 
                        else False 
-- | ou

horaDepois2 :: (Int,Int) -> (Int,Int) -> Bool
horaDepois2 (h1, m1) (h2, m2) = h1 > h2 || (h1 == h2 && m1 > m2)

-- | c
hour2min :: (Int,Int) -> Int 
hour2min (a,b) = a * 60 + b  

-- | d
min2hour :: Int -> (Int,Int)
min2hour a =  (div a 60, mod a 60)  

-- | e
hourDiff :: (Int,Int) -> (Int,Int) -> Int
hourDiff (a,b) (c,d) = if a == c then abs (b - d)
                       else abs ( (a * 60 + b) - (c * 60 + d) )

-- | f
addMins :: Int -> (Int,Int) -> (Int,Int)
addMins min (a,b) = min2hour (hour2min (a,b) + min)  

-- | Exercício 4
data HORA = H Int Int
  deriving (Show, Eq)
horaValida4 :: HORA -> Bool
horaValida4 (H a b) = if a < 0 then False
            else if a > 23 then False 
            else if b > 59 then False
            else if b < 0 then False
            else True

-- | Exercício 5
-- | a
data Semaforo = Verde | Amarelo | Vermelho
   deriving (Show,Eq)
next :: Semaforo -> Semaforo 
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde 

-- | b
stop :: Semaforo -> Bool
stop Verde = False
stop Amarelo = False
stop Vermelho = True  

-- | c
safe :: Semaforo -> Semaforo -> Bool
safe Verde Verde = False
safe Verde Amarelo = False
safe Verde Vermelho = True
safe Vermelho Verde = True
safe Vermelho Vermelho = True
safe Amarelo Vermelho = True
safe Amarelo Amarelo = True  

-- | Exercício 6
-- | a
data Ponto = Cartesiano Double Double | Polar Double Double
             deriving (Show,Eq)
posx :: Ponto -> Double 
posx (Cartesiano x y) = abs x
posx (Polar r angulo) = abs (r * cos angulo)

-- | b
posy :: Ponto -> Double 
posy (Cartesiano x y) = abs y
posy (Polar r angulo) = abs (r * sin angulo)

-- | c
raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x ^ 2 + y ^ 2)
raio (Polar r angulo) = r

-- | d
angulo :: Ponto -> Double
angulo (Cartesiano x y) = if x < 0 && y==0 then pi 
                          else if x < 0 then pi + atan (y/x)
                          else atan (y/x) 
angulo (Polar r angulo) = angulo 

-- | e
dist1 :: Ponto -> Ponto -> Double 
dist1 (Cartesiano a b) (Cartesiano c d) = sqrt ((a-c)^2 +(b-d)^2) 
dist1 (Polar r1 angulo1) (Polar r2 angulo2) = sqrt ((x1-x2)^2 +(y1-y2)^2)
  where 
    x1 = r1 * cos angulo1
    x2 = r2 * cos angulo2
    y1 = r1 * sin angulo1
    y2 = r2 * sin angulo2   

-- | Exercício 7
(+>) :: Float -> Float -> Float
x +> y = x^2 + y

parte :: (Ord a) => a -> [a] -> ([a],[a])
parte _ [] = ([],[])
parte x (y:ys) | y < x = (y:as, bs)
               | otherwise = (as, y:bs)
    where (as,bs) = parte x ys 