--Funcion 1 //calcular promedio aritmetica de 3 calificaciones

promedioAritmetica x y z = (x + y + z ) / 3

--Funcion 2//calcular sumar monedas 

sumaMonedas a b c d e = a + (b*2) + (c*5) + (d*10) + (e*20)

--Funcion 3//calcular volumen de una esfera 4/3 * pi * r^3

volumeEsfera r = (4/3) * pi * (r ^ 3)

--Funcion 4//area de una corona

areaCoronaCircular r1 r2 = pi *(r2^2 - r1^2)

--Funcion 5//obtener la ultima cifra

ultimaCifra x = rem x 10

--Funcion 6//max de tres 

maximoDeTres x y z = max x (max y z)

--Funcion 7//rotar lista 

rotar xs = tail xs ++ [head xs]

--Funcion 8//rotar n elementos 

rotaNElem n xs = drop n xs ++ take n xs

--Funcion 9//nueva lista con el rango min y max

rangoMinMax xs = [min xs , max xs]

--Funcion 10//palindromo de lista

palindromo xs = xs == reverse xs

--Funcion 11//(interior xs) es la
-- lista obtenida eliminando los extremos de la lista xs.

interior xs = init (tail xs)

--Funcion 13//Elementos comprendidos
 
segmento a b  xs = take ( b-a + 1) (drop (a-1) xs)

--Funcion 14//Definir extremos

extremos a xs = take a xs ++ (take a (drop a xs)) 

--Funcion 15//Obtener el numero mediano 

mediano x y z = x + y + z - maximum [ x , y , z ] - minimum [ x , y , z ]

--Funcion 16//Funcion 3 iguales

iguales x y z = x == y && y == z

--Funcion 17//Funcion 3 diferentes

diferentes x y z = x /= y && y /= z && x /= z

--Funcion 18//Funcion 4 iguales

cuatroIguales x y z u = iguales x y z && z == u

------------------------- Guardas y Patrones ------------------------

--Funcion 1 p2//division segura
divisionSegura :: Double -> Double -> Double
divisionSegura x y 
 | x /= 0 && y /= 0 = x / y
 | otherwise = 9999.0

--Funcion 2 p2//disyuncion excluyente 

xor1 :: Bool -> Bool -> Bool
xor1 x  y 
 | x && not y = True
 | not x && y = True
 | otherwise = False

--Funcion 3 p2//EL mayor rectangulo de 2 dados

mayorRectangulo :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
mayorRectangulo r1 r2
    | area r1 == area r2 = r1
    | area r1 > area r2 = r1
    | otherwise         = r2
  where
    area (a,b) = a * b

--Funcion 4 p2//intercambiar cords

intercambia :: (a,b) -> (b,a)
intercambia (a,b) = (b,a)

--Funcion 5 p2//distancia entre 2 puntos

distancia :: (Double,Double) -> (Double,Double) -> Double
distancia (a,b) (c,d) 
 | a == d = 0
 | d > a = d - a 

--Funcion 6 p2//

--ciclo :: [a] -> [a]
--ciclo (x:xs)
-- | 

--Funcion 7 p2//

numeroMayor :: (Num a, Ord a) => a -> a -> a
numeroMayor a b 
 |a < b = (b*10)+a
 |a > b = (a*10)+b

--Funcion 8 p2//Numero de reices reales

numeroDeRaices :: (Floating t, Ord t) => t -> t -> t -> Int
numeroDeRaices a b c 
 | discriminante > 0 = 2
 | discriminante == 0 = 1
 | otherwise = 0
 where
    discriminante = b^2 - 4*a*c

--Funcion 9 p2// lista de raices reales //formula general

raices :: Double -> Double -> Double -> [Double]
raices a b c 
 | discriminante > 0 = [r1,r2]
 | discriminante == 0 = [r1]
 | otherwise = []
 where
    discriminante = b^2 -4*a*c
    r1 = (-b + sqrt discriminante) / (2*a)
    r2 = (-b - sqrt discriminante) / (2*a)

--Funcion 10 p2// area triangulo 

area :: Double -> Double -> Double -> Double
area a b c = sqrt s * (s - a)*(s - b)*(s - c)
 where 
    s = ( a + b + c ) / 2

--Funcion 11 p2//Funcion calcular interseccion max a, min b

interseccion :: Ord a => [a] -> [a] -> [a]
interseccion [] _ = []
interseccion _ [] = []
interseccion [a1 , b1][a2 , b2] 
 | max a1 a2 <= min b1 b2 = [max a1 a2 , min b1 b2]
 | otherwise = []

--Funcion 12 p2//linea n de un trinagulo aritmetico
--inicio = (n(n-1)/2)+1
--fin = inicio + n -1

linea :: Integer -> [Integer]
linea n = [inicio..fin]
  where
    inicio = (n * (n - 1)) `div` 2 + 1
    fin = inicio + n - 1

---------------------------Recursividad -----------------------

--Funcion 1 p3//Potencia de x
--x^n= x * x ^ n-1

potencia :: Integer -> Integer -> Integer
potencia _ 0 = 1
potencia x n = x * potencia x ( n - 1 )

--Funcion 2 p3//