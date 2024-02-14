import Data.Char
--Q1
mapear :: (a -> x) -> [a] -> [x]
mapear f [] = []
mapear f (a:x) = (f a) : (mapear f x)


converte  :: Char -> Char
converte ch  | ch>= 'a' && ch<='z' =ch
             | ch>= 'A' && ch<='Z' = chr ((ord ch)+32)
             |otherwise=error"não é uma letra entre A e Z!!!" 
                
converte2  :: Char -> Char
converte2 ch | ch>= 'a' && ch<='z' =chr ((ord ch)-32)
             | ch>= 'A' && ch<='Z' = ch
             |otherwise=error"não é uma letra entre A e Z!!!" 


transforma_all_to_min :: [Char] -> [Char]
transforma_all_to_min str = mapear converte str

transforma_all_to_mai :: [Char] -> [Char]
transforma_all_to_mai str = mapear converte2 str

num_to_string::Int->String
num_to_string 0 ="zero"
num_to_string 1 ="um"
num_to_string 2 ="dois"
num_to_string 3 ="três"
num_to_string 4 ="quatro"
num_to_string 5 ="cinco"
num_to_string 6 ="seis"
num_to_string 7 ="sete"
num_to_string 8 ="oito"
num_to_string 9 ="nove"

transforma_num_to_string::[Int]->[String]
transforma_num_to_string g = mapear num_to_string g

--Q2
total ::(Int->Int)-> Int -> Int
total f n = foldr (+)0 (mapear f[0..n])


--Q3
eh_par g = mod g 2 ==0
vezes_3 g = 3*g

mapfiltrar :: (a -> Bool) -> (a -> b) -> [a] -> [b] 
mapfiltrar p f (x:xs) = mapear f(filtrar p xs)



--Q4
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar f [] = []
filtrar f (a:x)
  |f a = a : (filtrar f x)
  |otherwise = filtrar f x


num_primo :: Int -> Bool
num_primo 0 = False
num_primo 1 = False
num_primo 2 = True
num_primo n = (filtrar p [2..n-1]) == [] 
        where p x = mod n x == 0
