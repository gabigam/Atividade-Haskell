--Q1
elementos a b c | a==b && a==c = 3
                | a==b =2
                | a==c = 2
                | c==b = 2
                |otherwise =0
--Q2
media a b c = (a+b+c)/3

maior_media a b c | a> (media a b c)&& b> (media a b c) && c> (media a b c) = 3
                | a>(media a b c) && b> (media a b c)=2
                | a> (media a b c) && c> (media a b c)= 2
                | b> (media a b c) && c> (media a b c)=2
                | a> (media a b c)=1
                | b> (media a b c)=1
                | c>(media a b c)=1
                |otherwise =0 

--Q3
potencia_2 x | x==1=1
             |x/=1=x^2

--Q4
potencia_4 x | x==1=1
              |x/=1=(potencia_2 x)^2

--Q5

ou_exclusivo a b = (a || b) && not (a && b)
 

--Q6

bhaskara :: Double -> Double -> Double -> (Double, Double)
bhaskara a b c 
 |delta < 0 = error "Raízes negativas!"
 |otherwise = (x_menor, x_maior)
 where x_menor = ((-b) - (sqrt delta)) / (2*a)
       x_maior = ((-b) + (sqrt delta)) / (2*a)
       delta   = (b^2) - (4*a*c)
