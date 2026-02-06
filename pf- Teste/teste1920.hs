intersect :: Eq a => [a] -> [a] -> [a]
intersect [] [] = []
intersect l []  = l
intersect [] l  = []
intersect (x:xs) l@(y:ys)
    | elem x l  = x : intersect xs l
    | otherwise = intersect xs l 

tails :: [a] -> [[a]]
tails [] = [[]] 
tails (x:xs) = (x:xs) : tails xs 

type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

elems :: ConjInt -> [Int]
elems [] = []
elems ((x,y):resto)
    | x == y    = x : elems resto 
    | otherwise = [x..y] ++ elems resto 

geraconj :: [Int] -> ConjInt
geraconj [] = []
geraconj [x] = [(x,x)]
geraconj (x:x1:xs)
    | x1 == x + 1 = let ((inicio, fim):cauda) = geraconj (x1:xs)
                    in (x, fim) : cauda
    | otherwise   = (x, x) : geraconj (x1:xs)

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
    deriving (Show)

type Nome = String
type Agenda = [(Nome, [Contacto])]

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n e [] = [(n,[Email e])]
acrescEmail n e ((n1,contactos):resto)
    | n == n1   = (n, ((Email e):contactos)) : resto
    | otherwise = (n1,contactos) : acrescEmail n e resto 

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing 
verEmails n ((n1,contactos):resto) 
    | n == n1   = Just (soEmails contactos)
    | otherwise = verEmails n resto 

soEmails :: [Contacto] -> [String]
soEmails [] = []
soEmails ((Email e):xs) = e : soEmails xs 
soEmails (_:xs) = soEmails xs 

consulta :: [Contacto] -> ([Integer],[String])
consulta [] = ([],[])
consulta (x:xs) = case x of
    Email e -> (nums, e:strs)
    Casa n  -> (n:nums, strs)
    Trab n  -> (n:nums, strs)
    Tlm n   -> (n:nums, strs)
    where (nums, strs) = consulta xs

consultaIO :: Agenda -> IO ()
consultaIO a = do
    putStr "Introduza o nome: "
    nomeAconsultar <- getLine 
    let resultado = verContactos nomeAconsultar a 
    case resultado of
        Nothing -> putStrLn "Contacto inexistente."
        Just cts -> do
            let (nums, emails) = consulta cts -- Usamos a tua função consulta!
            putStrLn $ "Números: " ++ show nums
            putStrLn $ "Emails: " ++ show emails

verContactos :: Nome -> Agenda -> Maybe [Contacto]
verContactos n [] = Nothing
verContactos n ((n1,contactos):resto)
    | n == n1   = Just contactos
    | otherwise = verContactos n resto 

-- Agenda de teste
minhaAgenda :: Agenda
minhaAgenda = [
    ("Ana", [Tlm 911222333, Email "ana@gmail.com", Casa 210000000]),
    ("Pedro", [Email "pedro@pmail.pt", Trab 220000000]),
    ("Maria", [Tlm 966777888, Tlm 922333444])
  ] 

data RTree a = R a [RTree a] deriving (Show, Eq)

paths :: RTree a -> [[a]]
paths (R x []) = [[x]]
paths (R x filhos) =  map (x:) (concatMap paths filhos)