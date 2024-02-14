--Q1
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

--Q2
fibo :: Int -> Int
fibo n  | n == 0 = 0
        | n == 1 = 1
        | n > 1 = fibo (n-2) + fibo(n-1)
        |otherwise= error"valor invalido"


lista_fibo::Int->[Int]
lista_fibo n |n==0=[]
             |n== 1 = [0]
             |n==2 = [0,1]
             |n>1= lista_fibo (n-1)++[fibo n]
             |otherwise= error"valor invalido"

--Q3
ordenar_listas :: [Int] -> [Int] -> [Int]
ordenar_listas  [] [] =[]
ordenar_listas  [] (y:ys) =(y:ys)
ordenar_listas  (x:xs) [] =(x:xs)
ordenar_listas (x:xs)(y:ys) |(x== y) = x:y:ordenar_listas  xs ys 
                        |(x<y)= x:ordenar_listas  xs (y:ys)
                        |otherwise = y: ordenar_listas  ys (x:xs)

--Q4
busca_sub:: String->[String]->[String]
busca_sub _[]=[]
busca_sub su (x:xs) |verifica_sub su x == " " = busca_sub su xs
                    |otherwise = x:busca_sub su xs

verifica_sub:: String->String->String
verifica_sub xs ys = if xs==take(length xs) ys  then ys  else " "

--Q5
div_listas :: [Int] -> [Int] -> [Int]
div_listas []_=[]
div_listas _[]=[]
div_listas xs (y:ys) | divisor_lista xs y == True = y: div_listas xs ys
                         | otherwise = div_listas xs ys



divisor_lista :: [Int] -> Int -> Bool
divisor_lista [] _ = False
divisor_lista [x] y = if mod y x == 0 then True else False
divisor_lista (x:xs) y  |mod y x == 0 = divisor_lista xs y
                        |otherwise = False

--Q6
inter_listas:: Eq a => [a] -> [a] -> [a]
inter_listas [] _ = []
inter_listas (x:ab) g |elem x g = x : inter_listas ab g
                      |otherwise = inter_listas ab g

--Q7
rodar_esquerda :: Int->[Char] -> [Char]
rodar_esquerda  g as | g >= 0 = drop g as ++ take g as
                      | g < 0 = drop tam as ++ take tam as
                      where tam = g+length as

--Q8
conta_consecIguais :: String -> Int
conta_consecIguais [] = 0
conta_consecIguais [x] = 1
conta_consecIguais (x:xs)
    | x == head xs = 1 + conta_consecIguais xs
    | otherwise = 1

format :: Int->Char -> String 
format n ch = '!':show n ++ (ch:"")

comprime :: String -> String
comprime [] = []
comprime (x:xs)
    | qtsConsecutivos > 3 = format qtsConsecutivos x ++ comprime (drop qtsConsecutivos (x:xs))
    | otherwise = x:comprime xs
    where qtsConsecutivos = conta_consecIguais (x:xs)


--Q9
ehDigito :: Char -> Bool
ehDigito x
 |elem x ['0'..'9'] = True
 |otherwise = False



descomprime :: String -> String
descomprime [] = []
descomprime (x:xs)
    | x=='!' = replicate (read(funcao_auxiliar xs))chr ++ descomprime (drop (tam +1)xs)
    | otherwise = x:descomprime xs
    where
        tam=length(funcao_auxiliar xs)
        chr=head(drop tam xs)

funcao_auxiliar:: String -> String 
funcao_auxiliar (x:xs) 
        | ehDigito x = x:funcao_auxiliar xs 
        | otherwise = []

