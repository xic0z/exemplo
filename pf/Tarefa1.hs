{-|
Module      : Tarefa1
Description : Validação de estados.

Módulo para a realização da Tarefa 1 de LI1\/LP1 em 2025\/26.
-}
module Tarefa1 where

import Labs2025

Terreno = Ar || Agua || Terra || Pedra 

-- | Função principal da Tarefa 1. Recebe um estado e retorna se este é válido ou não.
validaEstado :: Estado -> Bool
validaEstado e@(Estado {mapaEstado = mapa,objetosEstado= objetos, minhocasEstado = minhocas}) = eMatrizValida mapa &&
  
  
                                              validaMapa mapa && 
                                              verificaOpacos Terreno && 
                                              validaObjetos objetos mapa && 
                                              posicaoLivre mapa && 
                                              perfuraTerreno (lanterior,canterior) (latual,catual) m &&
                                              posicaoOcupada &&
                                              existeObjetoNaPosicao &&
                                              posicaoObejto &&
                                              existeMinhocaNaPosicao &&
                                              mesmaMinhoca &&
                                              disparoValido &&
                                              tempoArma &&
                                              donodisparoValido &&
                                              verificarVidaMinhocaEmAguaOuSemPosicao &&
                                              verificarVidaMinhoca &&
                                              quantidadeMunicoes  &&

validaMapa :: Mapa -> Bool
validaMapa [] = False 
validaMapa a = 
    let 
        primeiroComprimento = length (head a)
        verificaLinhas [] = True 
        verificaLinhas (x:xs) = 
            if length x == primeiroComprimento then verificaLinhas xs
            else False
    in 
        verificaLinhas a 


verificaOpacos :: Terreno -> Bool
verificaOpacos Ar      = False
verificaOpacos Agua    = False
verificaOpacos Terra   = True 
verificaOpacos Pedra   = True 


validaObjetos :: [Objeto] -> Mapa -> Bool
validaObjetos (o@Disparo{}:os) mapa = validaObjeto (posicaoDisparo o) mapa
validaObjetos (o@Barril{}:os) mapa = validaObjeto (posicaoBarril o) mapa


validaObjeto :: (Int,Int) -> Mapa -> Bool
validaObjeto (i,j) m = i < length m && j < length (head m) && i >= 0 && j >= 0

posicaoLivre :: (Int,Int) -> Mapa -> Bool
posicaoLivre (i,j) m = 
    if terreno == Ar || terreno == Agua
        then True 
        else False 
    where 
        terreno = (m !! i) !! j


perfuraTerreno :: (Int,Int) -> (Int,Int) -> Mapa -> Bool
perfuraTerreno (lanterior,canterior) (latual,catual) m =
    if (terrenoAtual == Agua || terrenoAtual == Ar) then True 
    else if (terrenoAtual == Terra || terrenoAtual == Pedra) && (terrenoAtual == Ar || terrenoAnterior == Agua) then True
    else False 
  where 
    terrenoAtual = (m !! latual) !! catual 
    terrenoAnterior = (m !! lanterior) !! canterior 


posicaoOcupada :: Posicao -> Estado -> Objeto -> Bool
posicaoOcupada (l,c) estado obj =
    posicaoOcupada (l,c) estado obj =
  if objEhDisparo obj
  then False
  else if existeObjetoNaPosicao (l,c) (objetos estado) obj || existeMinhocaNaPosicao (l,c) (minhocas estado) obj
       then True
       else False
  where
    objEhDisparo (Disparo _) = True
    objEhDisparo _           = False

existeObjetoNaPosicao :: Posicao -> [Objeto] -> Objeto -> Bool
existeObjetoNaPosicao _ [] _ = False
existeObjetoNaPosicao pos (o:os) me =
      if posicaoObjeto o == pos && o /= me
      then True
      else existeObjetoNaPosicao pos os me

posicaoObjeto :: Objeto -> Posicao
posicaoObjeto (Barril p)  = p
posicaoObjeto (Disparo p) = p

existeMinhocaNaPosicao :: Posicao -> [Minhoca] -> Objeto -> Bool
existeMinhocaNaPosicao _ [] _ = False
existeMinhocaNaPosicao pos (m:ms) me =
      case posicao m of
        Just p -> if p == pos && not (mesmaMinhoca m me) then True
                  else existeMinhocaNaPosicao pos ms me
        Nothing -> existeMinhocaNaPosicao pos ms me

mesmaMinhoca :: Minhoca -> Objeto -> Bool
mesmaMinhoca _ _ = False



data Objetos = Escavadora | Jetpack | Bazuca | Mina | Dinamite
             deriving (Eq,Show)


disparoValido :: Objetos -> Bool
disparoValido obj = if obj == Jatpack || obj = Escavadora then False
                    else True 



tempoArma :: Objetos -> Maybe Int -> Bool
tempoArma Bazuca Nothing      = True
tempoArma Bazuca (Just _)     = False
tempoArma Mina Nothing        = True
tempoArma Mina (Just x)       = x >= 0 && x <= 2
tempoArma Dinamite (Just x)   = x >= 0 && x <= 4
tempoArma Dinamite Nothing    = False


donodisparoValido :: Int -> [Minhoca] -> Bool 
donodisparoValido i (x:xs) = i >= 0 && i < length (x:xs)


validaMinhoca :: Estado -> Minhoca -> Bool
validaMinhoca e m = validaPosicao && validaVida && ValidaMunicoes
   where
    mapa = mapaEstado e 
    objs = minhocaEstado e 
    pos = posicaoMinhoca m 


validaPosicaoMinhoca = 
    case pos of 
        Nothing -> True 
        Just (x,y) -> dentroDoMapa mapa (x,y) 
                      && not (terrenoOpaco mapa (x,y))
                      && not (ocupadaPorOutraMinhoca (x,y))
                      && not (ocupadaPorBarril objs (x,y))



verificarVidaMinhocaEmAguaOuSemPosicao :: Maybe Posicao -> Terreno -> Int -> Bool
verificarVidaMinhocaEmAguaOuSemPosicao Nothing _ v       = v <= 0 
verificarVidaMinhocaEmAguaOuSemPosicao (Just _) Agua v   = v <= 0 
verificarVidaMinhocaEmAguaOuSemPosicao (Just _) _ v      = True 


verificarVidaMinhoca :: Int -> Bool
verificarVidaMinhoca x = if x >= 0 && x <= 100 then True 
                         else False 


quantidadeMunicoes :: Int -> Bool
quantidadeMunicoes x = if x >= 0 then True 
                       else False 


dentroDoMapa :: Mapa -> Posicao -> Bool
dentroDoMapa mapa (x,y) = 
    x >= 0 && y >= 0 && x < length mapa && y < length (head mapa)


terrenoOpaco :: Mapa -> Posicao -> Bool
terrenoOpaco mapa (x,y) = case (mapa !! x) !! y of
    Terra -> True
    Pedra -> True 
    _     -> False


terrenoAgua :: Mapa -> Posicao -> Bool
terrenoAgua mapa (x,y) = (mapa !! x) !! y == Agua 