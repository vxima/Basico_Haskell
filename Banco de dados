 -- Banco de Dados

type BancoDados  = [(String , String)]
type Pessoa = String 
type Livro = String 
exemplo :: BancoDados
exemplo = [("Victor" , "One Piece") , 
           ("Pedro" , "JoJo") ,
           ("Riei" , "YuYu Hakusho")]
livros :: BancoDados ->  Pessoa -> [Livro]
livros bd p = [snd a | a <- bd , fst a == p]

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos bd l = [fst a | a <- bd , snd a == l]

emprestado :: BancoDados ->Livro -> Bool
emprestado bd l = length (emprestimos bd l) > 0 -- se tiver algum , significa que é true

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos bd p = length(livros bd p)


devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver bd l = [(pp , ll) | (pp , ll) <- bd , p/=pp || l/=ll]
