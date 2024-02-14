--Q1
somatorio n1 |n1 == 1 = 1
        |otherwise = (somatorio(n1 -1)) + n1

soma_Intervalo n1 n2 = (somatorio n2) - (somatorio n1)+n1

soma_Intervalo2 n1 n2 = (somatorio (n2-1)) - (somatorio n1)


--Q2
multiplo:: Int -> Int -> Int
multiplo n3 n1 |n3==n1=1
           |n1>=1&&n1<=(n3-1)=0
           |otherwise = 1+multiplo n3(n1-n3)

multIntervalo:: Int -> Int -> Int->Int
multIntervalo n3 n1 n2
  |n1>n2=multiplo n3 n1 - multiplo n3 (n2-1) 
  |n2>n1=multiplo n3 n2 - multiplo n3 (n1-1)


--Q3
soma :: Int -> Int -> Int
soma n1 n2 = n1 + n2

mult :: Int -> Int -> Int
mult n1 0 = 0
mult n1 n2
 |(n1 < 0) && (n2 < 0) = (mult (abs n1) (abs n2))
 |n2 >= 0 = soma n1  (mult n1 (n2-1))
 |otherwise = mult n2 n1
