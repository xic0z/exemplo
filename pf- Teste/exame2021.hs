-- Remove a primeira ocorrência de um elemento 'x' numa lista
removeElement :: Eq a => a -> [a] -> [a]
removeElement _ [] = []
removeElement x (y:ys)
    | x == y    = ys           -- Encontrou: remove e para aqui
    | otherwise = y : removeElement x ys

-- Aplica o removeElement para cada item da segunda lista
removeOcrs :: Eq a => [a] -> [a] -> [a]
removeOcrs l [] = l            -- Se não há nada para remover, devolve a lista
removeOcrs l (x:xs) = removeOcrs (removeElement x l) xs