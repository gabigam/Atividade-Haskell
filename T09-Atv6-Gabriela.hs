--Q1
type Data = (Int, Int, Int)

type Ano = Int
type Mes = Int
type Dia = Int


data_dias :: Data -> Int
data_dias (d, m, a)  |mod a 4 == 0 && (mod a 100 /= 0 || mod a 400 == 0)= dia d + mes_dias m
                     |otherwise = dia d + mes_dias1 m

dia :: Dia -> Dia
dia d1 
    |d1 <= 31  = d1
    |otherwise = error "digite outro dia"

ano_dias :: Ano -> Ano -> Dia
ano_dias ano1 ano2 
    |ano1 == ano2 = 0
    |ano1 > ano2 = error "digite outro ano"
    |ano1 < ano2 && bissexto ano1 = 366 + ano_dias (ano1+1) ano2 
    |otherwise    = 365 + ano_dias (ano1+1) ano2 

mes_dias :: Mes -> Dia
mes_dias mes
     |mes == 1 = 0
     |mes == 2 = 31
     |mes == 3 = 60
     |mes == 4 = 91
     |mes == 5 = 121
     |mes == 6 = 152
     |mes == 7 = 182
     |mes == 8 = 213
     |mes == 9 = 244
     |mes == 10 = 274
     |mes == 11 = 305
     |mes == 12 = 335
     |otherwise = error "digite outro mes"

mes_dias1 :: Mes -> Dia
mes_dias1 mes
     |mes == 1 = 0
     |mes == 2 = 31
     |mes == 3 = 59
     |mes == 4 = 90
     |mes == 5 = 120
     |mes == 6 = 151
     |mes == 7 = 181
     |mes == 8 = 212
     |mes == 9 = 243
     |mes == 10 = 273
     |mes == 11 = 304
     |mes == 12 = 334
     |otherwise = error "digite outro mes"

bissexto :: Ano -> Bool
bissexto ano = mod ano 4 == 0 && (mod ano 100 /= 0 || mod ano 400 == 0)

dif_dia_data :: (Int,Int,Int) -> (Int,Int,Int) -> Int
dif_dia_data (d1,m1,a1) (d2,m2,a2) |a1<a2 || (a1==a2 && m1<m2) || (a1==a2 && m1==m2 && d1<=d2) = data_dias(d2,m2,a2) - data_dias(d1,m1,a1) + ano_dias a1 a2
                                   |otherwise = error "digite outra data"


--Q2
raizes_reais :: (Float,Float,Float) -> (Float,Float)
raizes_reais (a,b,c) | delta < 0 = error "Raizes Imaginarias"
                     | otherwise = (r1,r2)
 where r1 = (-b+sqrt(delta))/(2*a)
       r2 = (-b-sqrt(delta))/(2*a)
       delta = (b^2) - (4*a*c)


--Q3
classificar :: (Int,Int,Int) -> String
classificar (x,y,z) | (x+y<z) || (x+z <y) || (y+z < x) = "Nao eh triangulo"
                    | (x==y) && (y==z) = "Equilatero"
                    | (x==y) || (y==z) = "Isosceles"
                    | otherwise = "Escaleno"
         
perimetro::(Int,Int,Int) -> Int
perimetro (x,y,z) = x + y + z

triangulo:: (Int,Int,Int) -> (String, Int)
triangulo (x,y,z)  = (classificar (x,y,z),perimetro (x,y,z)) 

--Q4
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
