--Q1
--a)
expo::Float->Float-> Float
expo x n
 |n==0=1.0
 |otherwise = x**n

fatorial::Float->Float
fatorial n |n==0 = 1.0
           |otherwise = fatorial(n-1) * n

soma_elementos:: Float->Float-> Float
soma_elementos x n
 |n==0 = 1.0
 |otherwise = ((expo x n) / (fatorial n)) + soma_elementos x (n-1)
--b
qual_n x = qualAux x 1

qualAux x n
 |((exp x) - (expo x n))< 0.001=n
 |otherwise=qualAux x (n+1)

--Q2
resto_divisao :: Int -> Int -> Int
resto_divisao x y |x<y=x
                  |x>y= x-((div x y) * y)
                  |otherwise = 0

--Q3
--a)
funcao_an::Float->Float
funcao_an n
 |n==0=0
 |n==1=sqrt 6
 |otherwise=sqrt(6+funcao_an(n-1))
--b)
soma_termos::Float->Float
soma_termos n
 |n==0=0
 |n==1=sqrt 6
 |otherwise = funcao_an n + soma_termos(n-1)
