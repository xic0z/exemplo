import System.Random
import System.IO.Error
import Data.List
-- cabal install random --lib

datas :: IO (Int,Int,Int)
datas = do dia <- randomRIO (1,31) -- produza um valor entre 1-31
           mes <- randomRIO (1,12)
           ano <- randomRIO (2000,2025)
           return (dia,mes,ano)

bingo :: IO ()
bingo = do putStrLn "Vai começar!"
           numeros [1..90]
           putStrLn "Terminou"

numeros :: [Int] -> IO ()
numeros [] = return ()
numeros l = do p <- randomRIO (0,(length l) -1)
               let e = l !! p
               putStrLn ("Saiu o numero" ++ show e)
               let l' = delete e l
               numeros l' 
-- se faz IO é seta, se nao faz é let


