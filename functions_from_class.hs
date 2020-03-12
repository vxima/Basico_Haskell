--definir as seguintes funções 

--fatorial
fatorial t 
 | t==0 = 1 --se for 0 vai retornar 1
 | otherwise = t*fatorial (t-1) --se for diferente vai retornar x* o fatorial de x-1
 
 --forma alternativa de fatorial
 
fat :: Int -> Int 
fat 0 = 1 
fat n = n*fat(n-1)

allEqual a b c = (a==b) && (b==c)
--all4equal
--int->int->int->int->bool
all4equalFacil x y z w = ((x==y) && (y==z) && (z==w))

--usando allEqual
all4equal n m p q = (allEqual n m p ) && (allEqual m p q)
--equalCount

--listas !!

sumList soma
 | soma == [] = 0
 | otherwise = (head soma) + sumList(tail soma)

double :: [int] -> [int]
double [] = []
double (x:xs) = 2*x : double (xs)  -- dobra o valor, concatena na lista e chama recursivamente para o tail da lista

member :: [Int] -> Int -> Bool
member [] y = False
member (x:xs) y = if (y==x) then True else member(xs) (y)

digits :: String -> String 
digits [] = []
digits (x:xs) = if(x >= '0' && x <= '9') then x : digits(xs) else digits(xs)

sumPairs :: [Int] ->[Int] ->[Int]
sumPairs [] [] = [] -- soma duas listas vazias , retorna outra vazia
sumPairs [] (y:ys) = (y:ys)
sumPairs (x:xs) [] = (x:xs)
sumPairs (x:xs) (y:ys) = x+y : sumPairs (xs) (ys) -- soma as cabeças e faz uma chamada recursiva com o resto das listas 


maiorLista :: [Int] -> Int 
maiorLista [] = minBound :: Int
maiorLista [x] = x
maiorLista (x:xs)
 | x> maiorLista xs = x
 | otherwise        = maiorLista xs

baskara :: (Double , Double , Double) -> [Double]
baskara ( a , b, c ) 
 | (b*b) > 4.0*a*c = [((-b + sqrt(b*b - 4*a*c))/(2*a))] ++ [((-b - sqrt(b*b - 4*a*c))/2*a)]
 | (b*b) == 4.0*a*c = [((-b + sqrt(b*b - 4*a*c))/(2*a))] ++ [((-b - sqrt(b*b - 4*a*c))/(2*a))]
 | otherwise = []
