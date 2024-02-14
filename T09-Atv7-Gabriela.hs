--Q1
posi_rel::Int->[Int]->Int
posi_rel g [] = 0
posi_rel g (a:x) | g == a = 1
                 | otherwise = 1 + posi_rel g x

maior_val::[Int]->Int
maior_val[a] = a
maior_val(a:x) | a > maior_val x = a
               |otherwise = maior_val x


tupla_2::[Int]->(Int,Int)
tupla_2 (a:x) = (maior_val(a:x), posi_rel (maior_val (a:x)) (a:x))


--Q2
type Dicionario  = (Int,String)
type Dic_10 = [Dicionario]
lista_tradutor::Dic_10
lista_tradutor=[(0,"zero"),(1,"um"),(2,"dois"),(3,"tres"),(4,"quatro"),(5,"cinco"),(6,"seis"),(7,"sete"),(8,"oito"),(9,"nove")]

tradutor :: [Int]->[String]
tradutor  []      =  []
tradutor (x : xs) = snd (lista_tradutor!!x) : tradutor xs 

--Q3
type Pessoa = (String,Int)
type Cadastro = [Pessoa]


lista_pessoas::Cadastro
lista_pessoas=[("Joao",21),("Ana",40), ("Alex",32), ("Carla",7),("Aloisio",12),("Paula",80), ("Gabi",20)]

maisnova::Cadastro->[Char]
maisnova [(a,i)]=a
maisnova ((a,x):((b,y):lista_pessoas))  | x < y = maisnova ((a,x):lista_pessoas)
                                | otherwise = maisnova ((b,y):lista_pessoas)


maisvelha::Cadastro->[Char]
maisvelha [(a,i)]=a
maisvelha ((a,x):((b,y):lista_pessoas))  | x > y = maisvelha ((a,x):lista_pessoas)
                                 | otherwise = maisvelha ((b,y):lista_pessoas)

--Q4
del_posicao::[Int]->Int -> [Int]
del_posicao (a:b) 1 = b
del_posicao (a:b) g = a: del_posicao b (g-1)

--Q5
lista_impares :: [Int]->[Int]
lista_impares [] = []
lista_impares (b:c) | mod b 2 == 0 = lista_impares c
              | otherwise    = b:(lista_impares c)


menor [a] = a
menor (a:b:c)  |a<b = menor (a:c)
               |otherwise = menor (b:c)


remove _ [] = []
remove g [a]  |g==a = []
              |otherwise = error "O valor nao esta na lista!"

remove g (b:c)  |g==b = c
                |otherwise = b:(remove g c)

ordena:: [Int]->[Int]
ordena [] = []
ordena [a] = [a]
ordena x = (menor x) : (ordena (remove (menor x) x))

ordenaImpares ::[Int]->[Int]
ordenaImpares g =ordena (lista_impares g)

--Q6
inserir_posicao::Int->Int->[Int]->[Int]
inserir_posicao c 1 x = [c] ++ x
inserir_posicao c g (a:x) | g > length (a:x) = (a:x) ++ [c]
                          |otherwise = a: inserir_posicao c (g-1) x

--Q7
valor_pos::[Int]->Int-> Int
valor_pos a b | b > length a = error "a posicao excede o tamanho da lista"
              | otherwise = head(drop (b-1) a)

--Q8
repete::Int->[[Int]]
repete 1 = [[1]]
repete x = replicate x x:repete (x-1)

--Q9
palindromo :: (Eq x) => [x] -> Bool
palindromo []  = True
palindromo [_] = True
palindromo g  = (head g) == (last g) && (palindromo ( init ( tail g)))