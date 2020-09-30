import Data.List.Split
--Q1 Fatura do mês
--a ideia vai ser pegar a string e converter pra lista de valores com o seguinte padrao: (dia , mes , loja , custo)
--depois filtra a lista com os indices pares , q sao o mes e o preço
-- depois agrupa essa lista em uma lista de duplas (mes , custo)
-- depois soma todos os custos dado determinado mes

splitarLista:: String -> [String]
splitarLista [] = [""]
splitarLista (c:cs) 
    | c == ' ' || c==';' = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = splitarLista cs 

filtro:: [String] -> Int -> [String]
filtro lst ind 
    | lst == [] = []
    | ind `mod` 2 == 0 = head lst : filtro (tail lst) (ind+1)
    | otherwise = filtro (tail lst) (ind+1)

agruparDupla::[String] -> [(String , Double)]
agruparDupla []  = []
agruparDupla (x:xs) = (x , stof (head xs) ) : agruparDupla (tail xs) 

somarFaturaMes::[(String , Double)] -> String -> Double -> Double
somarFaturaMes lst mes total
    | lst == [] = total
    | fst (head lst) == mes = somarFaturaMes (tail lst) mes (total + snd (head lst))
    | otherwise =  somarFaturaMes (tail lst) mes total 

logMes :: String -> String -> Double
logMes mes fatura = somarFaturaMes (agruparDupla (filtro (splitarLista fatura) 1 )) mes 0

--Q2 minimo e maximo do cartão
fatura = "14 JAN;Amazon;40.32;15 JAN;Uber;14.84;25 JAN;Uber;34.24;02 FEV;Spotify;8.50;06 FEV;Uber;6.94;05 MAR;Burger;29.90;10 MAR;Burger;24.99;15 MAR;UCI;19.00;08 ABR;Itunes;3.50;13 ABR;Picpay;20.00;"

stof :: String -> Double
stof word = read word :: Double

splitList :: String -> Char -> [String]
splitList [] delim = [""]
splitList (c:cs) delim
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = splitList cs delim

splitFatura :: String -> [String]
splitFatura fatura = splitList fatura ';'

pegaValor :: [String] -> Int -> [Double]
pegaValor faturas contador
    | faturas == [] = []
    | (contador `mod` 3 )== 0 = (stof (head faturas )) : pegaValor (tail faturas) (contador + 1)
    | otherwise = pegaValor (tail faturas) (contador + 1)

minMax :: [Double] -> Double -> Double -> (Double , Double)
minMax [] menor maior = (menor , maior)
minMax (x:xs) menor maior 
    | x > maior = minMax xs menor x
    | x < menor = minMax xs x maior 
    | otherwise = minMax xs menor maior

minMaxCartao :: String -> (Double, Double)
minMaxCartao [] = (0.0 , 0.0)
minMaxCartao fatura = minMax (pegaValor (splitFatura fatura) 1) 10000.0 0.0


--Q3: replica
contador :: String -> Int -> Char-> Int
contador [] conta letra = conta
contador (x:xs) conta letra
    | x == letra = contador xs (conta +1) letra
    | otherwise = contador xs conta  letra
qtdChar :: String -> Int -> Char -> Bool
qtdChar str  n  letra 
    | cont == n = True
    | otherwise = False
    where cont = contador str 0 letra 


isReplica :: String -> Int -> Char -> Bool
isReplica [] 0 letra = True
isReplica [] n letra = False
isReplica (x:xs) n letra 
    | x == letra = isReplica xs (n-1) letra
    | otherwise = False

--Q4 decifra enigma
translateChar :: Char -> [(Char, Char)] -> Char
translateChar letter decode
    | letter == fst (head decode) = snd (head decode)
    | otherwise = translateChar letter (tail decode)

decEnigma :: String -> [(Char, Char)] -> String
decEnigma word decode 
    | word == [] = ""
    | otherwise = ((translateChar (head word) decode ):[]) ++ (decEnigma (tail word) decode )

--Q5 : Binario para inteiro
converteChar :: Char -> Int
converteChar x = read (x:[])

btoi :: String -> Int 
btoi [] = 0
btoi (y:ys) = (converteChar y)*(2^(length ys)) + btoi ys




--Q6 : Calculadora
type Comando = String
type Valor = Int

check:: [(Comando , Valor)] -> Bool
check [] = False
check (a:as) 
    | a == ("Divide" , 0) = True 
    | otherwise = check as
executa:: [(Comando , Valor)] -> Int 
executa list 
    | check list == True = -666
    | otherwise = calculadoraCheck list 0

calculadoraCheck:: [(Comando , Valor)] -> Int -> Int
calculadoraCheck [] x = x

calculadoraCheck (a:as) x 
    | fst a == "Multiplica"  = calculadoraCheck as (x* snd a)
    | fst a == "Soma"  = calculadoraCheck as (x + snd a)
    | fst a == "Subtrai"  = calculadoraCheck as (x - snd a) 
    | fst a == "Divide"  = calculadoraCheck as (x `div` snd a)


--Q7: Multiplicaçao de listas


mul2 :: [Int] -> [Int] -> [Int]
mul2 [] [] = []
mul2 [] (y:ys) = 0 : mul2 [] (ys)
mul2 (x:xs) [] = 0 : mul2 (xs) []
mul2 (x:xs) (y:ys) = x*y : mul2 (xs) (ys)


 