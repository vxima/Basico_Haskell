--definir as seguintes funções 

--fatorial
fatorial t 
 | t==0 = 1 --se for 0 vai retornar 1
 | otherwise = t*fatorial (t-1) --se for diferente vai retornar x* o fatorial de x-1

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
double [x] = 2*x
double (x:xs) = [2*x] : double xs 
