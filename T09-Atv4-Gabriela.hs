--Q1
fat::Float->Float
fat m = fatorial m 1

fatorial::Float->Float->Float
fatorial m n |m==0=1
             |m==1 =n
             |otherwise = fatorial(m-1) (n*m)


qtd_objts:: Float->Float->Float
qtd_objts m n |m==0 = 1.0
               |m>=n = fat m / ((fat n)*fat(m-n)) 
               |otherwise =error"digite outro valor" 

--Q2
mdc::Int->Int->Int
mdc x y |x==0&&y==0=0
         |x < y = mdc y x
         |y == 0 = x
         |otherwise = mdc y (mod x y)

--Q3
mmc::Int->Int->Int
mmc a b = (a * b) `div` (mdc a b)

mmc_3 ::Int->Int->Int->Int
mmc_3 a b c = mmc a (mmc b c)

--Q4

raiz_inteira n = truncate(sqrt(n))

--Q5
ackermann ::(Int,Int)->Int
ackermann (m,n)
    | m == 0 = n+1
    | n == 0 = ackermann ((m-1),1)
    | otherwise = ackermann (m-1 ,(ackermann (m,n-1)))
