------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------Guardas y Patrones -----------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

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

--Funcion 6 p2//permuta los elementos cilclicamente 
--de una lista pasado el ultimo elemento al principio
--last es al último elemento de la lista xs.init es la lista 
--obtenida eliminando el ultimo elemento de xs 

ciclo :: [a] -> [a]
ciclo [] = []                     
ciclo [x] = [x]                   
ciclo xs = last xs : init xs    

--Funcion 7 p2//el numero mayor construido por los 2 digitos 

numeroMayor :: (Num a, Ord a) => a -> a -> a
numeroMayor a b 
 |a < b = (b*10)+a
 |a > b = (a*10)+b

--Funcion 8 p2//Numero de raices reales
--se ocupa lo que esta dentro de la raiz cuadrada de la formula general

numeroDeRaices :: (Floating t, Ord t) => t -> t -> t -> Int
numeroDeRaices a b c 
 | discriminante > 0 = 2
 | discriminante == 0 = 1
 | otherwise = 0
 where
    discriminante = b^2 - 4*a*c

--Funcion 9 p2// lista de raices reales //Usando formula general

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