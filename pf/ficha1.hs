module Ficha1 where
perimCirc :: Double -> Double
perimCirc r = 2 * 3.14 * r

dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

ponto1 :: (Double,Double)
ponto1 = (4.5,23)
ponto2 = (-5.6,234.3)

primUlt :: [Double] -> (Double,Double)
primUlt l = (head l, last l)

multiplo :: Int -> Int -> Bool
multiplo m n = if (mod m n) == 0
             then True
             else False

truncaImpar :: [a] -> [a]
truncaImpar l = if mod (length l) 2 /= 0
                then tail l
                else l 

--max2 :: Int -> Int -> Int
max2 x y = if x>y then x else y

{-
max3 x y z = if x>=y && x>=z then x
            else if y>=x && y<=x then y
                else z
-}

max3 x y z = max2 (max2 x y) z

--Exercício 2
{-
nRaizes :: (Double,Double,Double) -> Int
nRaizes (a,b,c) = if (b^2 - 4*a*c) < 0
                then 0
                else if (b^2 - 4*a*c) == 0
                    then 1
                    else 2
-}

nRaizes :: (Double,Double,Double) -> Int
nRaizes (a,b,c) = if delta < 0
                then 0
                else if delta == 0
                    then 1
                    else 2
    where delta = (b^2 - 4*a*c)

raizes :: (Double, Double, Double) -> [Double]
raizes (a,b,c) = if nRaizes (a,b,c) == 0
                then []
                else if nRaizes (a,b,c) == 1
                    then [(-b)/(2*a)]
                    else [raiz1, raiz2]

    where delta = (b^2 -4*a*c)
          raiz1 = ((-b) + (sqrt delta) / (2*a))
          raiz2 = ((-b) - (sqrt delta) / (2*a))
