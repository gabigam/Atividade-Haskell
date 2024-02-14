--Q1
import Data.Char
converte  :: Char -> (Char, Char, Int)
converte ch | ch>= 'a' && ch<='z' =(ch, minusc_Mai ch, ord ch)
                | ch>= 'A' && ch<='Z' = (ch, maiusc_Min ch, ord ch)
                |otherwise=error"não é uma letra entre A e Z!!!" 
                where 
                 maiusc_Min::Char->Char
                 maiusc_Min ch = chr ((ord ch)+32)

                 minusc_Mai::Char->Char
                 minusc_Mai ch = chr ((ord ch)-32)
--Q2
type Idade=Float
type Pessoa = (String, Float, Char)

pessoa :: Float -> Pessoa
pessoa rg  |rg == 1 = ("Joao Silva", 12, 'm')
           |rg == 2 = ("Jonas Souza", 51, 'm')
           |rg == 3 = ("Gabi Mota",20,'f') 
           |rg == 4 = ("Luiz Caldas",6,'m') 
           |rg == 5 = ("Jocileide Strauss", 21, 'f')
           |rg == 6= ("Joao Victor",27,'m')
           |rg == 7 = ("Nataly Santos", 32, 'f')
           |otherwise = ("Nao ha ninguem mais", 9999, 'x')

selec_idade(n,i,s)=i
selec_sexo(n,i,s)=s

menor (n, i, s) (n2, i2, s2) | i <= i2 = (n, i, s)
                             | otherwise = (n2, i2, s2)

menor_idade :: Float -> Pessoa
menor_idade g | g == 1 = pessoa 1
              |g>7= error "digite outro valor"
              | otherwise = menor (pessoa g) (menor_idade (g-1))


media_idade :: Float -> Float
media_idade g = (soma_idade g)/g

soma_idade g  |g == 1 = selec_idade (pessoa 1)
              |g>7= error "digite outro valor"
              |otherwise = selec_idade (pessoa g) + (soma_idade (g-1))


quantM ::Float->Float
quantM g
    |g==1 && selec_sexo(pessoa g) == 'm' = 1
    |g==1 = 0
    |g>7= error "digite outro valor"
    |g>1 && selec_sexo(pessoa g) == 'm' = 1 + quantM (g-1)
    |otherwise = quantM (g-1)

maior rg rg2
    | i >= i2 = rg
    | otherwise = rg2
    where 
        (_, i, _) = pessoa rg
        (_, i2, _) = pessoa rg2

maior_idade :: Float -> Float
maior_idade g | g == 1 = 1
              | g>7= error "digite outro valor"
              | otherwise = maior (g) (maior_idade (g-1))

--Q3

analisaLetra :: Char -> (Char, Char, Int)
analisaLetra ch
    | isUpper ch = (ch, toLower ch, ord ch)
    | isLower ch = (ch, toUpper ch, ord ch)
    |otherwise=error"não é uma letra entre A e Z!!!"
                 

--Q4
ord_crescente :: Int -> Int-> Int -> Int -> (Int,Int,Int,Int)
ord_crescente a b c d   |b<a = ord_crescente  b a c d
                        |c<b= ord_crescente  a c b d 
                        |d<c = ord_crescente a b d c
                        |otherwise = (a,b,c,d)

ord_decrescente :: Int -> Int-> Int -> Int -> (Int,Int,Int,Int)
ord_decrescente d c b a |c>d = ord_decrescente  c d b a
                        |b>c= ord_decrescente   d b c a
                        |a>b = ord_decrescente  d c a b
                        |otherwise = (d,c,b,a)