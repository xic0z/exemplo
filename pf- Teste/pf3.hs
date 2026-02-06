data Hora = H Int Int
          deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

-- Testar se um par de inteiros representa uma hora do dia valida.
validaHora :: Hora -> Bool
validaHora (H x y) = x >= 0 && x <= 23 && y >= 0 && y <= 59 

-- Testar se uma hora e ou nao depois de outra (comparacao).
comparaHora :: Hora -> Hora -> Bool
comparaHora (H a b) (H x y) = validaHora (H a b) && validaHora (H x y) && a > x || validaHora (H a b) && validaHora (H x y) && a == x && b > y

-- Testar se uma etapa esta bem construıda (i.e., o tempo de chegada e superior ao de partida e as horas sao validas).
etapaValida :: Etapa -> Bool
etapaValida (H x y, H a b) = comparaHora (H a b) (H x y) && validaHora (H x y) && validaHora (H a b)

-- Testa se uma viagem esta bem construıda (i.e., se para cada etapa, o tempo de
-- chegada e superior ao de partida, e se a etapa seguinte comeca depois da etapa anterior ter terminado)
viagemValida :: Viagem -> Bool
viagemValida [] = True
viagemValida [e] = etapaValida e
viagemValida (e1:e2:t) = etapaValida e1 && 
                         passagemValida e1 e2 && 
                         viagemValida (e2:t)
  where
    -- Função auxiliar para garantir que a etapa seguinte começa após a anterior terminar.
    passagemValida (_, fim1) (inicio2, _) = comparaHora inicio2 fim1

-- Calcular a hora de partida e de chegada de uma dada viagem.
partidaEchegada :: Viagem -> (Hora, Hora)
partidaEchegada v = (fst (head v), snd (last v))

-- Dada uma viagem valida, calcular o tempo total de viagem efectiva.
tempoDaViagem :: Viagem -> Hora
tempoDaViagem [] = H 0 0
tempoDaViagem v = minsParaHoras (horaParaMins (snd (partidaEchegada v)) - horaParaMins (fst (partidaEchegada v)))

-- Converter um valor em horas (par de inteiros) para minutos (inteiro).
horaParaMins :: Hora -> Int
horaParaMins (H x y) = (x * 60) + y 

-- Converter um valor em minutos para horas.
minsParaHoras :: Int -> Hora
minsParaHoras min = H (div min 60) (mod min 60)

-- Calcular o tempo total de espera.
tempoDeEspera :: Viagem -> Hora
tempoDeEspera [] = H 0 0
tempoDeEspera [e1] = H 0 0
tempoDeEspera (e1:e2:t) = minsParaHoras (horaParaMins (fst e2) - horaParaMins (snd e1) + horaParaMins (tempoDeEspera (e2:t)))

-- Calcular o tempo total da viagem (a soma dos tempos de espera e de viagem efectiva).
totalViagem :: Viagem -> Hora 
totalViagem [] = H 0 0
totalViagem v = minsParaHoras (horaParaMins (tempoDaViagem v) + horaParaMins (tempoDeEspera v)) 

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show   

type Nome = String
type Agenda = [(Nome, [Contacto])]

-- Dado um nome, um email e uma agenda, acrescenta essa informacao a agenda.
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email [] = [(nome, [Email email])] 
acrescEmail nome email ((n,cs):t)
    | nome == n = (n, Email email : cs) : t   
    | otherwise = (n,cs) : acrescEmail nome email t

-- Dado um nome e uma agenda, retorna a lista dos emails associados a esse nome. Se esse
-- nome nao existir na agenda a funcao deve retornar Nothing.
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [] = Nothing
verEmails nome ((n,cs):t)
    | nome == n = Just (soEmails cs)
    | otherwise = verEmails nome t 

soEmails :: [Contacto] -> [String]
soEmails [] = []
soEmails (Email e : t) = e : soEmails t
soEmails (_:t) = soEmails t

-- Dada uma lista de contactos, retorna a lista de todos os numeros de telefone dessa lista.
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (Casa n : t) = n : consTelefs t
consTelefs (Trab n : t) = n : consTelefs t
consTelefs (Tlm n : t)  = n : consTelefs t
consTelefs (Email _ : t) = consTelefs t

-- Dado um nome e uma agenda, retorna o numero de telefone de casa (caso exista).
casa :: Nome -> Agenda -> Maybe Integer
casa nome [] = Nothing
casa nome ((n,cs):t) 
    | nome == n = extraiCasa cs
    | otherwise = casa nome t

-- Função auxiliar para procurar apenas o contacto Casa numa lista de contactos
extraiCasa :: [Contacto] -> Maybe Integer
extraiCasa [] = Nothing
extraiCasa (Casa c : _) = Just c
extraiCasa (_ : t)      = extraiCasa t

type Dia = Int
type Mes = Int
type Ano = Int
-- type Nome = String

data Data = D Dia Mes Ano
          deriving Show

type TabDN = [(Nome,Data)]

-- Indica a data de nascimento de uma dada pessoa, caso o seu nome exista na tabela.
procura :: Nome -> TabDN -> Maybe Data
procura _ [] = Nothing 
procura nome ((n,d):t)
    | nome == n = Just d
    | otherwise = procura nome t

-- Calcula a idade de uma pessoa numa dada data.
idade :: Data -> Nome -> TabDN -> Maybe Int
idade _ _ [] = Nothing
idade (D d m a) nome ((n, D d1 m1 a1):t)
    | nome == n = Just (if m > m1 || (m == m1 && d >= d1) 
                        then a - a1 
                        else a - a1 - 1)
    | otherwise = idade (D d m a) nome t

-- Testa se uma data e anterior a outra data.
anterior :: Data -> Data -> Bool
anterior (D d1 m1 a1) (D d2 m2 a2)
    | a1 < a2 = True
    | a1 > a2 = False
    | m1 < m2 = True
    | m1 > m2 = False
    | otherwise = d1 < d2

-- Ordena uma tabela de datas de nascimento, por ordem crescente das datas de nascimento.
ordena :: TabDN -> TabDN
ordena [] = []
ordena (h:t) = insere h (ordena t)

-- Função auxiliar que coloca UM elemento no sítio certo
-- Se x for o menor elemento, comparativamente a TODOS os elementos dessa lista ele vai ficar dentro da lista.
insere :: (Nome, Data) -> TabDN -> TabDN
insere x [] = [x]
insere (n, d) ((n1, d1):t)
    | anterior d d1 = (n, d) : (n1, d1) : t
    | otherwise     = (n1, d1) : insere (n, d) t

-- Apresenta o nome e a idade das pessoas, numa dada data, por ordem crescente da idade das pessoas.
porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade dt tabela = porIdadeAux dt (ordena tabela)

porIdadeAux :: Data -> TabDN -> [(Nome,Int)]
porIdadeAux _ [] = []
porIdadeAux d ((nh,dh):t) = porIdadeAux d t ++ [(nh, calculaIdade dh d)] 

calculaIdade :: Data -> Data -> Int
calculaIdade (D dn mn an) (D d m a) = if m > mn || m == mn && d > dn then a - an else a - an - 1