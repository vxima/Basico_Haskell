--Q1 : safe second
import Prelude hiding (Maybe (..))


data Maybe a = Just a |
               Nothing
               deriving(Show)

safeSecond :: [a] -> Maybe a 
safeSecond [] = Nothing 
safeSecond [a] = Nothing
safeSecond lst = Just (head (tail lst))

--Q2 : calculo seguro

stoi :: String -> Int
stoi word = read word :: Int

--fazer safeCalc apenas a funcao que pega a string , chama uma funcao auxiliar e printa o resultado
safeCalc :: String -> IO ()
safeCalc [] = putStr " "
safeCalc name = putStr (show (fazerOp (tratarString name)))

--funcao q trata a string para pegar os valores que precisa
tratarString :: String  -> (Int , String , Int)
tratarString [] = (0 , " " , 0)
tratarString lst = (stoi (take (getn lst 0) lst) , take 3 (drop (getn lst 0) lst) , stoi (drop ((getn lst 0)+3) lst))

--contartamanho da primeira 
getn :: String -> Int -> Int
getn "" x = x
getn (c:cs) x
  | c == 's' || c == 'm' || c == 'd' = x
  | otherwise = getn cs (x+1)

--funcao q pega a tupla e processa a operaçao
fazerOp :: (Int , String , Int) -> Maybe Int
fazerOp (x , "div" , 0) = Nothing
fazerOp (x , "sum" , y) = Just (x + y)
fazerOp (x , "sub" , y) = Just (x - y)
fazerOp (x , "mul" , y) = Just (x * y)
fazerOp (x , "div" , y) = Just (x `div` y)

