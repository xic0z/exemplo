data Frac = F Integer Integer

-- Dada uma fracao calcula uma fracao equivalente, irredutıvel, e com o denominador positivo.

mdc :: Integer -> Integer -> Integer
mdc a 0 = abs a
mdc a b = mdc b (a `mod` b) 

normaliza :: Frac -> Frac
normaliza (F n d) 
    | d_reduzido < 0 = F (-n_reduzido) (-d_reduzido)
    | otherwise      = F n_reduzido d_reduzido
  where 
    m = mdc n d
    n_reduzido = n `div` m
    d_reduzido = d `div` m

-- Defina Frac como instancia da classe Eq.
instance Eq Frac where
    (==) :: Frac -> Frac -> Bool
    f1 == f2 = a1 == a2 && b1 == b2
        where F a1 b1 = normaliza f1
              F a2 b2 = normaliza f2

-- Defina Frac como instancia da classe Ord.
instance Ord Frac where
    (<=) :: Frac -> Frac -> Bool
    f1 <= f2 = a * d <= c * b 
        where F a b = normaliza f1
              F c d = normaliza f2 

-- Defina Frac como instancia da classe Show.
instance Show Frac where
    show :: Frac -> String
    show (F a b) = "(" ++ show a ++ "/" ++ show b ++ ")"

-- Defina Frac como instancia da classe Num.
instance Num Frac where
    (+) :: Frac -> Frac -> Frac
    (F a b) + (F c d) = normaliza (F (a * d + c * b) (b * d))

    (*) :: Frac -> Frac -> Frac
    (F a b) * (F c d) = normaliza (F (a * c) (b * d))

    (-) :: Frac -> Frac -> Frac
    (F a b) - (F c d) = normaliza (F (a * d - c * b) (b * d))

    negate :: Frac -> Frac
    negate (F a b) = F (-a) b

    abs :: Frac -> Frac
    abs (F a b) = F (abs a) (abs b)

    fromInteger :: Integer -> Frac
    fromInteger x = F x 1

    signum :: Frac -> Frac
    signum (F a b) | a * b > 0  = F 1 1
                   | a * b < 0  = F (-1) 1
                   | otherwise  = F 0 1
    -- Esta função indica o sinal do número: devolve 1 se for positivo, -1 se for negativo e 0 se for zero.

-- Dada uma fracao f e uma lista de fracoes l, seleciona de l os elementos que sao maiores do que o dobro de f.
dobrosFrac :: Frac -> [Frac] -> [Frac]
dobrosFrac _ [] = []
dobrosFrac (F a b) ((F c d):t)
    | (F c d) > (F 2 1) * (F a b) = (F c d) : dobrosFrac (F a b) t
    | otherwise = dobrosFrac (F a b) t 

data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

-- Declare Exp a como uma instancia de Show.
instance Show a => Show (Exp a) where -- Precisamos garantir que o 'a' também pode ser mostrado
    show (Const x)      = show x
    show (Simetrico e)  = "(-" ++ show e ++ ")"
    show (Mais e1 e2)   = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    show (Menos e1 e2)  = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"
    show (Mult e1 e2)   = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"

-- Declare Exp a como uma instancia de Eq.
instance (Num a, Eq a) => Eq (Exp a) where
    (==) :: (Num a, Eq a) => Exp a -> Exp a -> Bool 
    e1 == e2 = avalia e1 == avalia e2

avalia :: Num a => Exp a -> a
avalia (Const x) = x
avalia (Simetrico e1) = - (avalia e1)
avalia (Mais e1 e2) = avalia e1 + avalia e2
avalia (Menos e1 e2) = avalia e1 - avalia e2
avalia (Mult e1 e2) = avalia e1 * avalia e2

-- Declare Exp a como instancia da classe Num.
instance (Ord a, Num a) => Num (Exp a) where
    (+) :: Num a => Exp a -> Exp a -> Exp a
    x + y = Mais x y
    
    (-) :: Num a => Exp a -> Exp a -> Exp a
    x - y = Menos x y
    
    (*) :: Num a => Exp a -> Exp a -> Exp a
    x * y = Mult x y
    
    negate :: Num a => Exp a -> Exp a
    negate (Simetrico a) = a
    negate a = Simetrico a
    
    fromInteger :: Num a => Integer -> Exp a
    fromInteger x = Const (fromInteger x)
    
    abs :: (Ord a, Num a) => Exp a -> Exp a
    abs (Const a) = Const (abs a)
    abs (Simetrico a) = abs a
    abs op = if avalia op < 0 
             then negate op
             else op
    -- aqui é o caso de ser uma operacao, por isso que usamos a funcao avalia, para sabermos o valor.
    
    signum :: Num a => Exp a -> Exp a
    signum a | avalia a < 0 = Const (-1)
             | avalia a == 0 = Const 0
             | otherwise = Const 1 

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]

instance Eq Data where
    (D d1 m1 a1) == (D d2 m2 a2) = d1 == d2 && m1 == m2 && a1 == a2

-- Defina Data como instancia da classe Ord.
instance Ord Data where
    (<=) :: Data -> Data -> Bool
    (D x y z) <= (D a b c)
        | z < c = True
        | z > c = False 
        | y < b = True
        | y > b = False
        | otherwise = x <= a 

-- Defina Data como instancia da classe Show.
instance Show Data where
    show (D d m a) = show d ++ "/" ++ show m ++ "/" ++ show a 

 