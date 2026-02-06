import Data.List (sort) -- Precisamos do sort para a ordem não importar
import Data.Maybe (catMaybes) -- Caso queiras usar na função 'maior'

daPosicao :: [a] -> Int -> a
daPosicao (x:xs) n
    | n == 0 = x 
    | otherwise = daPosicao xs (n-1)

data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)  
posicao (x,y) (Norte:resto) = posicao (x,y+1) resto
posicao (x,y) (Sul:resto) = posicao (x,y-1) resto
posicao (x,y) (Este:resto) = posicao (x+1,y) resto
posicao (x,y) (Oeste:resto) = posicao (x-1,y) resto 

any1 :: (a -> Bool) -> [a] -> Bool
any1 p [] = False 
any1 p (x:xs) = p x || any1 p xs -- aqui usamos ou pq basta que um esteja certo

type Mat a = [[a]]

triSup :: (Num a, Eq a) => Mat a -> Bool
triSup [[]] = True
triSup matriz = aux 0 matriz
    where
        aux n [] = True 
        aux n (h:t) = zerosDeUmaLinha n h && aux (n+1) t

zerosDeUmaLinha :: (Num a, Eq a) => Int -> [a] -> Bool
zerosDeUmaLinha _ [] = False
zerosDeUmaLinha n lista = take n lista == replicate n 0

movimenta :: IO (Int, Int)
movimenta = movimentaAux (0, 0)

movimentaAux :: (Int, Int) -> IO (Int, Int)
movimentaAux (x, y) = do
    input <- getLine -- Lê a linha toda, limpando o Enter do caminho
    case input of
        "N" -> movimentaAux (x, y + 1)
        "S" -> movimentaAux (x, y - 1)
        "E" -> movimentaAux (x + 1, y)
        "O" -> movimentaAux (x - 1, y)
        _   -> return (x, y)

data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]

ex :: Imagem
ex = Mover (5,5)
      (Juntar [Mover (0,1) (Quadrado 5),
              Quadrado 4,
              Mover (4,3) (Quadrado 2)]) 

vazia :: Imagem -> Bool
vazia (Quadrado x) = False
vazia (Mover (x,y) i) = vazia i 
vazia (Juntar (i:is)) = all vazia (i:is) -- se usar any ele diz que está vazia só porque uma delas está.
                                         -- com o all ele diz que NÃO está vazia porque nem todas estão vazias.

maior :: Imagem -> Maybe Int
maior (Quadrado x) = Just x
maior (Mover (x,y) i) = maior i 
maior (Juntar []) = Nothing
maior (Juntar (h:t)) = maxMaybe (maior h) (maior (Juntar t))
  where
    maxMaybe Nothing m2 = m2
    maxMaybe m1 Nothing = m1
    maxMaybe (Just x) (Just y) = Just (max x y)

-- 1. A função que converte a árvore numa lista de quadrados posicionados
-- Recebe a posição atual (x,y) e a Imagem, devolve lista de ((x,y), tamanho)
aplana :: (Int, Int) -> Imagem -> [((Int, Int), Int)]
aplana (x, y) (Quadrado l) = [((x, y), l)] -- Encontrei um quadrado de tamanho l na posição (x, y).
aplana (x, y) (Mover (dx, dy) img) = aplana (x + dx, y + dy) img -- Vais te mover p esse local e desenhar o quadrado, pelo enunciado percebemos que as coordenadas se somam e é exatamente isso que fazemos.
aplana (x, y) (Juntar imgs) = concatMap (aplana (x, y)) imgs -- concatMap faz o map e depois "achata" tudo numa lista simples: [quadrado1, quadrado2, quadrado3]. É muito mais fácil de comparar!

-- 2. Definir a instância de Eq
instance Eq Imagem where
    i1 == i2 = sort (aplana (0,0) i1) == sort (aplana (0,0) i2)

-- Imagina que a tua lista de quadrados anotados é: [ ((5,5), 4), ((10,10), 2) ] E a minha é: [ ((10,10), 2), ((5,5), 4) ]
-- Os quadrados são os mesmos, mas como estão em ordem diferente, o Haskell diria que as listas são diferentes. Ao usarmos o sort, 
-- o Haskell organiza-as por uma ordem padrão (ex: primeiro os que têm X menor). Assim, se os quadrados forem os mesmos, as listas 
-- ficam idênticas.

-- Pega na imagem i1, assume que começas no (0,0) e faz a lista de todos os quadrados com as suas posições finais.
-- Ordena essa lista.
-- Faz o mesmo para a imagem i2.
-- Se as listas ordenadas forem iguais, então as imagens são visualmente iguais!