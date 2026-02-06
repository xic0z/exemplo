module Ficha3pf1 where 
data Hora = H Int Int
           deriving Show
type Etapa = (Hora,Hora)
type Viagem = [Etapa]

type Poligonal = [Ponto]

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving (Show, Eq)

v :: Viagem 
v =[(H 9 30, H 10 25), (H 11 20, H 12 45), (H 13 30, H 14 45)]

w :: Viagem
w = [(H 9 30, H 10 25)] 

pol1 :: Poligonal
pol1 = [Cartesiano 0 0, Cartesiano 1 1, Cartesiano 0 0]   -- fechada

pol2 :: Poligonal
pol2 = [Cartesiano 0 0, Cartesiano 1 1, Cartesiano 2 2]   -- não fechada

pol3 :: Poligonal
pol3 = []                                                 -- vazia 

pol4 :: Poligonal
pol4 = [Cartesiano 1 1]                                   -- um ponto

pol5 :: Poligonal
pol5 = [Cartesiano 0 0, Cartesiano 1 1, Cartesiano 5 2, Cartesiano 7 8, Cartesiano 2 3, Cartesiano 9 0, Cartesiano 1 1]

pol6 :: Poligonal
pol6 = [Cartesiano 1 1, Cartesiano 2 2, Cartesiano 3 3]
novoPonto :: Ponto
novoPonto = Cartesiano 0 0

-- | Exercício 1 
-- | a 
validaHora :: Hora -> Bool 
validaHora (H h m) = h >= 0 && h < 24 && m >= 0 && m < 60


comparaHora :: Hora -> Hora -> Bool 
comparaHora (H h1 m1) (H h2 m2) = if h1 > h2 then True 
                                  else if h2 > h1 then False 
                                       else m1 > m2 

etapaValida :: Etapa -> Bool 
etapaValida (hi,hf) = validaHora hi && 
                      validaHora hf && 
                      comparaHora hf hi 

-- | b
validaViagem :: Viagem -> Bool 
validaViagem [e]       = etapaValida e 
validaViagem (e1:e2:es) = etapaValida e1                &&
                          comparaHora (fst e2) (snd e1) &&
                          validaViagem (e2:es) 

-- | c 
horaPartidaChegada :: Viagem -> Etapa 
horaPartidaChegada v | validaViagem v = (fst(head v), snd(last v))

-- | d
horaParaMinutos :: Hora -> Int 
horaParaMinutos (H h m) = h * 60 + m 

minutosParaHoras :: Int -> Hora
minutosParaHoras m = H (mod (div m 60) 24) (mod m 60) 

duracaoEtapa :: Etapa -> Int 
duracaoEtapa (hi,hf) = horaParaMinutos hf - horaParaMinutos hi 

tempoAViajarMin :: Viagem -> Int
tempoAViajarMin [] = 0
tempoAViajarMin (e:es) = duracaoEtapa e + tempoAViajarMin es 

tempoAViajar :: Viagem -> Hora
tempoAViajar v = minutosParaHoras (tempoAViajarMin v) 

-- | e
tempoEsperaMin :: Viagem -> Int 
tempoEsperaMin [] = 0
tempoEsperaMin [e] = 0
tempoEsperaMin (e1:e2:es) = horaParaMinutos (fst e2) - horaParaMinutos (snd e1) + tempoEsperaMin (e2:es) 

tempoEspera :: Viagem -> Hora
tempoEspera [] = H 0 0
tempoEspera [e] = H 0 0
tempoEspera v = minutosParaHoras (tempoEsperaMin v) 

-- | f
tempoTotalViagemMin :: Viagem -> Int 
tempoTotalViagemMin [] = 0
tempoTotalViagemMin [e] = duracaoEtapa e 
tempoTotalViagemMin v = tempoAViajarMin v + tempoEsperaMin v 

tempoTotalViagem :: Viagem -> Hora 
tempoTotalViagem [] = H 0 0
tempoTotalViagem [e] = minutosParaHoras (duracaoEtapa e)
tempoTotalViagem v = minutosParaHoras (tempoTotalViagemMin v) 

-- | Exercício 2
-- | a
comprimentoLinhaPoligonal :: Poligonal -> Double 
comprimentoLinhaPoligonal [] = 0
comprimentoLinhaPoligonal [e] = 0
comprimentoLinhaPoligonal (p1:p2:p) = sqrt ((posx p2 - posx p1)^2 + (posy p2 - posy p1)^2) + comprimentoLinhaPoligonal (p2:p) 

posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar r a) = r * cos a 

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar r a) = r * sin a 

-- | b 
testarPoligonal :: Poligonal -> Bool
testarPoligonal [] = False
testarPoligonal [e] = True
testarPoligonal (p1:p) = if p1 == last p then True
                         else False 

-- | c
triangula :: Poligonal -> [Figura]
triangula (p1:p2:p3:ps)
    | p1 == p3 = []
    | otherwise = Triangulo p1 p2 p3 : triangula (p1:p3:ps)
triangula _ = []

-- | d 
areaPol :: [Figura] -> Double
areaPol [] = 0
areaPol (h:t) = areaTris h + areaPol t

-- função auxiliar que calcula a área de um triângulo
areaTris :: Figura -> Double
areaTris (Triangulo p1 p2 p3) =
  0.5 * abs (posx p1 * (posy p2 - posy p3)
           + posx p2 * (posy p3 - posy p1)
           + posx p3 * (posy p1 - posy p2))
areaTris _ = 0

-- | e
mover :: Poligonal -> Ponto -> Poligonal
mover (h:t) p = (p:t) 

-- | f
zoom :: Double -> Poligonal -> Poligonal