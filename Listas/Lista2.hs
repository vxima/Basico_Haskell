data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)
data Tree t = Node t (Tree t) (Tree t) 
                | Nilt
                deriving (Read)
--Q1 BST 1
inorder :: (Ord t) => Tree t -> [t]
inorder Nilt = []
inorder (Node t left right) = inorder left ++ [t] ++ inorder right

checkOrder :: (Ord t) => [t]  -> Bool
checkOrder []  = True
checkOrder [x] = True
checkOrder (a:as)   
    | (a > (head as)) = False 
    | otherwise = checkOrder as 

isBST :: (Ord t) => Tree t -> Bool
isBST Nilt = True
isBST (Node t Nilt Nilt) = True
isBST (Node t left right) = checkOrder (inorder (Node t left right)) 
--Q2 BST 2
insertList :: (Ord t) => Tree t -> [t] -> Tree t
insertList Nilt [] = Nilt 
insertList Nilt [x] = Node x Nilt Nilt
insertList Nilt lst = insertList (insertKey Nilt (head lst)) (tail lst)
insertList (Node t left right) [] = Node t left right
insertList (Node t left right) [x] = insertList (insertKey (Node t left right) x) []
insertList (Node t left right) lst = insertList (insertKey (Node t left right) (head lst)) (tail lst)

insertKey :: (Ord t) => Tree t -> t -> Tree t
insertKey Nilt key = Node key Nilt Nilt 
insertKey (Node t left right) key
    | (key == t) = Node t left right
    | (key < t ) = Node t (insertKey left key) right
    | (key > t ) = Node t left (insertKey right key)
       
--Q3 Calculadora arbórea
data Ops = SUM | MUL | SUB
           deriving (Read)

data IntTree = Nilto Int |
               NodeT Ops IntTree IntTree
               deriving (Read)

evalTree :: IntTree -> Int
evalTree (Nilto n) = n 
evalTree (NodeT SUM leftChild rightChild) = evalTree leftChild + evalTree rightChild
evalTree (NodeT SUB leftChild rightChild) = evalTree leftChild - evalTree rightChild
evalTree (NodeT MUL leftChild rightChild) = evalTree leftChild * evalTree rightChild


--Q4 Robo 1
projectWalk :: (Int , Int ) -> [Command] -> Direction -> (Int , Int)
projectWalk (x , y ) [] direct = (x , y)
--North
projectWalk (x , y ) (Forward n:ls) North  = projectWalk (x , y + n) ls North
projectWalk (x , y ) (Backward n:ls) North  = projectWalk (x , y - n) ls North
projectWalk (x , y ) (TurnLeft:ls) North  = projectWalk (x , y) ls West 
projectWalk (x , y ) (TurnRight:ls) North  = projectWalk (x , y) ls East
--South
projectWalk (x , y ) (Forward n:ls) South  = projectWalk (x , y - n) ls South
projectWalk (x , y ) (Backward n:ls) South  = projectWalk (x , y + n) ls South
projectWalk (x , y ) (TurnLeft:ls) South  = projectWalk (x , y) ls East 
projectWalk (x , y ) (TurnRight:ls) South  = projectWalk (x , y) ls West
--West
projectWalk (x , y ) (Forward n:ls) West  = projectWalk (x - n, y ) ls West
projectWalk (x , y ) (Backward n:ls) West  = projectWalk (x + n, y ) ls West
projectWalk (x , y ) (TurnLeft:ls) West = projectWalk (x , y) ls South
projectWalk (x , y ) (TurnRight:ls) West  = projectWalk (x , y) ls North
--East
projectWalk (x , y ) (Forward n:ls) East  = projectWalk (x + n, y ) ls East
projectWalk (x , y ) (Backward n:ls) East  = projectWalk (x - n, y ) ls East
projectWalk (x , y ) (TurnLeft:ls) East = projectWalk (x , y) ls North
projectWalk (x , y ) (TurnRight:ls) East  = projectWalk (x , y) ls South
destination :: (Int,Int) -> [Command] -> (Int,Int) 
destination (x , y ) []  =(x , y)
destination (x , y ) lst = projectWalk (x, y ) lst North

--Q5 Editor de texto
data Cmd = Cursor Int
           | Backspace Int
           | Delete Int
           | Insert String
           deriving (Read)
--funcao auxiliar
deletar :: (String , Int) -> String 
deletar (str , cursor ) = take (cursor) str

resto :: (String , Int ) -> String
resto ([] , cursor) = []
resto (str , cursor) 
    | cursor == 0 = str
    | otherwise = resto ((tail str) , (cursor - 1))
-- Cursor X , anda o cursor x casas
editCursor :: (String , Int) -> Int -> (String , Int)
editCursor ([] , atual) n = ([] , 0)
editCursor (str , atual) n = (str , atual + n)
--Backspace X , apaga x caracteres atras do cursor

editBackspace :: (String , Int ) -> Int -> (String , Int)
editBackspace ([] , atual) n = ([] , atual) 
editBackspace (str , atual) n = (deletar (str , atual - n) ++ resto (str , atual) , atual - n)
--delete x , apaga x caracteres na frente do cursor (inclui o do cursor)

editDelete :: (String , Int ) -> Int -> (String  , Int)
editDelete ([] , atual) n = ([] , atual)
editDelete (str , atual) n = (deletar (str , atual ) ++  resto (str , atual + n) , atual)
--insert S , insere a string S na posiçao do cursor

editInsert :: (String , Int ) -> String -> (String , Int)
editInsert (str , atual) s  = (deletar (str , atual ) ++ s ++ resto (str , atual ) , atual)
-- Editor de texto
editorTexto :: (String , Int) -> [Cmd] -> (String , Int)
editorTexto (str , cursor) [] = (str , cursor)
editorTexto (str , cursor) (Cursor n:lst) = (editorTexto (editCursor (str , cursor ) n ) lst)
editorTexto (str , cursor) (Backspace n:lst) = editorTexto (editBackspace (str , cursor) n)  lst
editorTexto (str , cursor ) (Delete n:lst) = editorTexto (editDelete (str , cursor) n   ) lst
editorTexto (str , cursor ) (Insert s:lst) = editorTexto (editInsert (str , cursor) s) lst

editText :: String -> [Cmd] -> String
editText [] [] = []
editText str [] = str
editText str lst = fst (editorTexto (str , 0) lst)
--Q6 Robo 2
faces :: Direction -> [Command] -> Direction
faces North [] = North
faces South [] = South
faces West [] = West
faces East [] = East
faces North (x:xs) 
  | x == TurnLeft = faces West xs 
  | x == TurnRight  = faces East xs
  |otherwise = faces North xs 
--  | x == (Forward _) = faces North xs 
--  | x == (Backward _) = faces North xs 
faces South (x:xs)
  | x == TurnLeft = faces East xs 
  | x == TurnRight  = faces West xs
  |otherwise = faces South xs 
--  | x == (Forward _)  = faces South xs 
--  | x == (Backward _)  = faces South xs
faces West (x:xs) 
  | x == TurnLeft = faces South xs 
  | x == TurnRight  = faces North xs
  | otherwise = faces West xs
--  | x == (Forward _) = faces West xs 
--  | x == (Backward _) = faces West xs
faces East (x:xs) 
  | x == TurnLeft = faces North xs 
  | x == TurnRight  = faces South xs
  | otherwise = faces East xs
--  | x == (Forward _) = faces East xs 
--  | x == (Backward _)  = faces East xs

--Q7 Altura da arvore

alturaArvore :: Tree t -> Int
alturaArvore Nilt = 0
alturaArvore (Node _ leftChild rightChild) = 1 + max (alturaArvore leftChild) (alturaArvore rightChild)