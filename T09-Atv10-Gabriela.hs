--ativ 1
ler_double::IO Double
ler_double = do linha<-getLine
                return(read linha::Double)

bhaskara :: IO ()
bhaskara = do 
    putStr "Entre com o valor de a,b e c:\n"
    a<-ler_double
    b<-ler_double
    c<-ler_double
    let d= (b^2) - (4*a*c)
    if (d<0)
    then putStr "Raizes negativas\n"
    else 
         putStr "O valor de x_menor e x_maior é:\n "
    print(((-b) - (sqrt d)) / (2*a))
    print(((-b) + (sqrt d)) / (2*a))

--ativ 2
ler_int::IO Int
ler_int = do linha<-getLine
             return(read linha::Int)

soma_mult :: IO()
soma_mult = do
        putStr "Entre com dois valores:\n"
        n1<-ler_int
        n2<-ler_int
        putStr "A soma é igual a:\n"
        print(soma1 n1 n2)
        putStr "A multiplicacao é igual a:\n"
        print(mult1 n1 n2) 

soma1 :: Int -> Int -> Int
soma1 n1 n2 = n1 + n2

mult1 :: Int -> Int -> Int
mult1 n1 0 = 0
mult1 n1 n2
 |(n1 < 0) && (n2 < 0) = (mult1 (abs n1) (abs n2))
 |n2 >= 0 = soma1 n1  (mult1 n1 (n2-1))
 |otherwise = mult1 n2 n1

 --ativ 3
ler_float::IO Float
ler_float = do linha<-getLine
               return(read linha::Float)

funcao_an_soma_termos::IO()
funcao_an_soma_termos = do
             putStr "Entre com um numero:\n"
             n<-ler_float
             putStr "O valor dele na funcao é:\n"
             print(funcao_an1 n)
             putStr "A soma dos termos até esse valor é:\n"
             print(soma_termos1 n)

funcao_an1::Float->Float
funcao_an1 n
 |n==0=0
 |n==1=sqrt 6
 |otherwise=sqrt(6+funcao_an1(n-1))

soma_termos1::Float->Float
soma_termos1 n
 |n==0=0
 |n==1=sqrt 6
 |otherwise = funcao_an1 n + soma_termos1(n-1)

 --ativ 4
ler_tupla :: IO (Int,Int)
ler_tupla = do linha<-getLine
               return (read linha::(Int,Int))

ackermann::IO()
ackermann = do 
        putStr "Entre com dois valores:\n"
        (m,n)<-ler_tupla
        putStr "O resultado é:\n"
        print(ackermann1 (m,n))
        
ackermann1 ::(Int,Int)->Int
ackermann1 (m,n)
    | m == 0 = n+1
    | n == 0 = ackermann1 ((m-1),1)
    | otherwise = ackermann1 (m-1 ,(ackermann1 (m,n-1)))

--ativ 5
ord_crescente_decrescente::IO()
ord_crescente_decrescente = do 
        putStr "Entre com quatro valores:\n"
        a<-ler_int
        b<-ler_int
        c<-ler_int
        d<-ler_int
        putStr "Ordem Crescente:\n"
        print(ord_crescente1 a b c d)
        putStr "Ordem Decrescente:\n"
        print(ord_decrescente1 d c b a)

ord_crescente1 :: Int -> Int-> Int -> Int -> (Int,Int,Int,Int)
ord_crescente1 a b c d   |b<a = ord_crescente1  b a c d
                        |c<b= ord_crescente1  a c b d 
                        |d<c = ord_crescente1 a b d c
                        |otherwise = (a,b,c,d)

ord_decrescente1 :: Int -> Int-> Int -> Int -> (Int,Int,Int,Int)
ord_decrescente1 d c b a |c>d = ord_decrescente1  c d b a
                        |b>c= ord_decrescente1   d b c a
                        |a>b = ord_decrescente1  d c a b
                        |otherwise = (d,c,b,a)

--ativ 6
base_dados ::IO ()
base_dados = do 
        putStrLn "digite a quantidade de pessoas do banco de dados:\n"
        x<-ler_int
        putStr "quantidade de doutores:\n"
        print (quant_D x)
        putStr "quantidade de mulheres:\n"
        print (quantM x)
        putStr "quantidade de mestres homens:\n"
        print (quantH_M x)
        putStr "nome da pessoa de menor matricula:\n"
        print (menor_matricula x)

type Matricula = Int
type Base = (Int, String, String, Char)
base :: Int -> Base
base x
         |x==0 = (1793, "Pedro Paulo", "MESTRE", 'M')
         |x==1 = (1797, "Joana Silva Alencar", "MESTRE", 'F')
         |x==2 = (1534, "Joao De Medeiros", "DOUTOR",'M')
         |x==3 = (1267, "Claudio Cesar de Sa", "DOUTOR", 'M')
         |x==4 = (1737, "Paula de Medeiros", "MESTRE", 'F')
         |x==5 = (1888, "Rita de Matos", "MESTRE", 'F')
         |x==6 = (1999, "Jonas Souza", "DOUTOR",'M')
         |x==7 = (1450, "Gabi Mota","DOUTOR",'F') 
         |x==8 = (1523, "Luiz Caldas","MESTRE",'M') 
         |x==9 = (1698, "Tereza Cristina Andrade", "MESTRE", 'F')
         |x==10 = (1234,"Nataly Santos", "DOUTOR", 'F')
         |x==11= (1952,"Jailto Gomes", "DOUTOR",'M')
         |x==12= (1964,"Maria Isabel", "MESTRE",'F')
         |otherwise = (0, " nao ha ninguem" , " nenhuma profissao ", '0')

selec_matricula (m,n,p,s)=m
selec_nome (m,n,p,s)=n
selec_profissao (m,n,p,s)=p
selec_sexo (m,n,p,s)=s

quant_D ::Int->Int
quant_D g
    |g==0 && selec_profissao (base g) == "DOUTOR" = 1
    |g==0 = 0
    |g>12= error " digite outro valor"
    |g>0 && selec_profissao (base g) == "DOUTOR" = 1 + quant_D (g-1)
    |otherwise = quant_D (g-1)

quantM ::Int->Int
quantM g
    |g==0 && selec_sexo (base g) == 'F' = 1
    |g==0 = 0
    |g>12= error " digite outro valor"
    |g>0 && selec_sexo (base g) == 'F' = 1 + quantM (g-1)
    |otherwise = quantM (g-1)

quantH_M::Int->Int
quantH_M g
    |g==0 && selec_sexo(base g) == 'M' && selec_profissao (base g)=="MESTRE" = 1
    |g==0 = 0
    |g>12= error " digite outro valor"
    |g>0 && selec_sexo(base g) == 'M' && selec_profissao (base g)=="MESTRE" = 1 + quantH_M (g-1)
    |otherwise = quantH_M (g-1)

menor(m,n,p,s) (m2,n2,p2,s2) | m <= m2 = (m,n,p,s)
                             | otherwise = (m2,n2,p2,s2)

menor_matricula :: Int -> Base
menor_matricula g | g == 0 = base 0
                  |g>12= error " digite outro valor"
                  | otherwise = menor (base g) (menor_matricula (g-1))

--ativ 7
ler_lista_string::IO [String]
ler_lista_string = do linha<-getLine
                      return(read linha::[String])

ler_lista_int::IO [Int]
ler_lista_int = do linha<-getLine
                   return(read linha::[Int])

palindromo_string::IO()
palindromo_string = do
           putStr "digite uma lista de caracteres:\n"
           g<-ler_lista_string
           if palindromo1 g
           then putStr ("é palindromo\n" ++show (palindromo1 g))
           else putStr ("não é palindromo\n"++show (palindromo1 g))

palindromo_int::IO()
palindromo_int = do
           putStr "digite uma lista de inteiros:\n"
           g<-ler_lista_int
           if palindromo1 g
           then putStr ("é palindromo\n" ++show (palindromo1 g))
           else putStr ("não é palindromo\n"++show (palindromo1 g))

palindromo1 :: (Eq x) => [x] -> Bool
palindromo1 []  = True
palindromo1 [_] = True
palindromo1 g  = (head g) == (last g) && (palindromo1 ( init ( tail g)))

--ativ 8
ler_string::IO String
ler_string = do linha<-getLine
                return(read linha::String)

descomprime::IO()
descomprime = do
           putStr "digite a codificacao:\n"
           x<-ler_string
           putStr "decodificação:\n"
           print(descomprime1 x)
        
ehDigito :: Char -> Bool
ehDigito x
 |elem x ['0'..'9'] = True
 |otherwise = False

descomprime1 :: String -> String
descomprime1 [] = []
descomprime1 (x:xs)
    | x=='!' = replicate (read(funcao_auxiliar xs))chr ++ descomprime1 (drop (tam +1)xs)
    | otherwise = x:descomprime1 xs
    where
        tam=length(funcao_auxiliar xs)
        chr=head(drop tam xs)

funcao_auxiliar:: String -> String 
funcao_auxiliar (x:xs) 
        | ehDigito x = x:funcao_auxiliar xs 
        | otherwise = []

--ativ 9
mapfiltrar_primos_mult3:: IO()
mapfiltrar_primos_mult3 = do  putStr "digite a lista:\n"
                              (x:xs)<-ler_lista_int
                              putStr "filtragem dos numeros primos:\n"
                              print(filtrar(num_primo)  (x:xs))
                              putStr "filtragem dos numeros primos multiplicados por 3:\n"
                              print(mapfiltrar (num_primo) (vezes_3)(x:xs))
        
mapear :: (a -> x) -> [a] -> [x]
mapear f [] = []
mapear f (a:x) = (f a) : (mapear f x)

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

vezes_3 g = 3*g

mapfiltrar :: (a -> Bool) -> (a -> b) -> [a] -> [b] 
mapfiltrar p f (x:xs) = mapear f(filtrar p xs)