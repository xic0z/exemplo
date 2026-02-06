{-|
Module      : Tarefa1
Description : Validação de estados.

Módulo para a realização da Tarefa 1 de LI1\/LP1 em 2025\/26.
-}
module Tarefa1 where

import Labs2025

data Terreno = Ar | Agua | Terra | Pedra 
type Mapa = [[Terreno]]


-- | Função principal da Tarefa 1. Recebe um estado e retorna se este é válido ou não.
validaEstado :: Estado -> Bool
validaEstado e = 
    mapaValido (mapaEstado e) &&
    objetosValidos (mapaEstado e) (objetosEstado e) (minhocasEstado e) &&
    minhocasValidas (mapaEstado e) (objetosEstado e) (minhocasEstado e)

-- | mapa válido
mapaValido :: Mapa -> Bool
mapaValido [] = False
mapaValido m@(linha:_) = todasLinhasMesmoTamanho m && terrenosValidos m   

todasLinhasMesmoTamanho :: Mapa -> Bool
todasLinhasMesmoTamanho [linha] = True
todasLinhasMesmoTamanho [] = True 
todasLinhasMesmoTamanho (linha:linha2:t) = (length linha == length linha2) && todasLinhasMesmoTamanho (linha2:t) 


-- | objeto valido
