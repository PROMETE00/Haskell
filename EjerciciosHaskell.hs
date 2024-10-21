--Funcion 1 //calcular promedio aritmetica de 3 calificaciones
-- Parámetros:
--  x, y, z :: Fractional a => a  -- Las tres calificaciones o números.
promedioAritmetica :: Fractional a => a -> a -> a -> a
promedioAritmetica x y z = (x + y + z ) / 3
-- Valor de retorno:
--  :: Fractional a => a          -- El promedio aritmético de los tres números.

---------------------------------------------------------------------------------
--Funcion 2//calcular sumar monedas 
-- Parámetros:
--  a, b, c, d, e :: Num a => a  -- Cantidades de monedas de 1, 2, 5, 10 y 20 unidades.
sumaMonedas :: Num a => a -> a -> a -> a -> a -> a
sumaMonedas a b c d e = a + (b*2) + (c*5) + (d*10) + (e*20)
-- Valor de retorno:
--  :: Num a => a                -- El valor total sumado.

---------------------------------------------------------------------------------
--Funcion 3//calcular volumen de una esfera 4/3 * pi * r^3
-- Parámetros:
--  r :: Floating a => a  -- El radio de la esfera.
volumeEsfera :: Floating a => a -> a
volumeEsfera r = (4/3) * pi * (r ^ 3)
-- Valor de retorno:
--  :: Floating a => a    -- El volumen de la esfera.

---------------------------------------------------------------------------------
--Funcion 4//area de una corona
--r1 es el radio de la circunferencia interior y r2 de la exterior
-- Parámetros:
--  r1, r2 :: Floating a => a  -- Radios de la circunferencia interior y exterior.
areaCoronaCircular :: Floating a => a -> a -> a
areaCoronaCircular r1 r2 = pi *(r2^2 - r1^2)
-- Valor de retorno:
--  :: Floating a => a         -- El área de la corona circular.

---------------------------------------------------------------------------------
--Funcion 5//obtener la ultima cifra//rem es el resto de
--una división entera
-- Parámetros:
--  x :: Integral a => a  -- El número entero.
ultimaCifra :: Integral a => a -> a
ultimaCifra x = rem x 10
-- Valor de retorno:
--  :: Integral a => a    -- La última cifra del número.

---------------------------------------------------------------------------------
--Funcion 6//maximo de tres numeros 
-- Parámetros:
--  x, y, z :: Ord a => a  -- Los tres números.
maximoDeTres :: Ord a => a -> a -> a -> a
maximoDeTres x y z = max x (max y z)
-- Valor de retorno:
--  :: Ord a => a          -- El mayor de los tres números.

---------------------------------------------------------------------------------
--Funcion 7//rotar lista//head devuelve el primer elemento
--tail devuelve todos menos el primer elemento
-- Parámetros:
--  xs :: [a]  -- La lista de elementos.
rotar :: [a] -> [a]
rotar xs = tail xs ++ [head xs]
-- Valor de retorno:
--  :: [a]     -- La lista rotada.

---------------------------------------------------------------------------------
--Funcion 8//rotar n elementos //take devuelve una lista 
--compuesta por los primeros n elementos de una lista 
--original.drop devuelve una lista después de eliminar 
--los primeros n elementos.
-- Parámetros:
--  n :: Int    -- Número de elementos a rotar.
--  xs :: [a]   -- La lista de elementos.
rotaNElem :: Int -> [a] -> [a]
rotaNElem n xs = drop n xs ++ take n xs
-- Valor de retorno:
--  :: [a]      -- La lista rotada.

---------------------------------------------------------------------------------
--Funcion 9//nueva lista con el rango min y max
-- Parámetros:
--  xs :: Ord a => [a]  -- Lista de elementos.
rangoMinMax :: Ord a => a -> [a -> a]
rangoMinMax xs = [min xs , max xs]
-- Valor de retorno:
--  :: Ord a => [a]     -- Lista con el valor mínimo y máximo.

---------------------------------------------------------------------------------
--Funcion 10//verificar si una lista es palindromo 
-- Parámetros:
--  xs :: Eq a => [a]  -- La lista de elementos.
palindromo :: Eq a => [a] -> Bool
palindromo xs = xs == reverse xs
-- Valor de retorno:
--  :: Eq a => Bool    -- Verdadero si la lista es un palíndromo, falso en caso contrario.

---------------------------------------------------------------------------------
--Funcion 11//(interior xs) es la
-- lista obtenida eliminando los extremos de la lista xs.
--init es la lista obtenida aliminando el ultimo elemento de xs.
--tail lista obtenida eliminando el primer elemento.
-- Parámetros:
--  xs :: [a]  -- La lista de elementos.
interior :: [a] -> [a]
interior xs = init (tail xs)
-- Valor de retorno:
--  :: [a]     -- La lista sin el primer y último elemento.

---------------------------------------------------------------------------------
--Funcion 13//Extrae un segmento de una lista, comprendido entre los índices a , b .
--take es la lista de los n primeros elementos de xs.
--drop borra los n primeros elementos de xs.
-- Parámetros:
--  a, b :: Int  -- Índices de inicio y fin (inclusive).
--  xs :: [a]    -- La lista de elementos. 
segmento :: Int -> Int -> [a] -> [a]
segmento a b  xs = take ( b-a + 1) (drop (a-1) xs)
-- Valor de retorno:
--  :: [a]       -- La sublista entre los índices a y b.

---------------------------------------------------------------------------------
--Funcion 14//Obtener lista por n extremos
--length xs es el número de elementos de la lista xs.
--drop n xs borra los n primeros elementos de xs.
--take n xs es la lista de los n primeros elementos de xs.
-- Parámetros:
--  a :: Int    -- Número de elementos de los extremos.
--  xs :: [a]   -- La lista de elementos.
extremos :: Int -> [a] -> [a]
extremos a xs = take a xs ++ take a (drop (length xs - a) xs) 
-- Valor de retorno:
--  :: [a]      -- Lista con los n primeros y últimos elementos.

---------------------------------------------------------------------------------
--Funcion 15//Obtener el numero mediano 
-- Parámetros:
--  x, y, z :: Ord a => a  -- Los tres números.
mediano :: (Num a, Ord a) => a -> a -> a -> a
mediano x y z = x + y + z - maximum [ x , y , z ] - minimum [ x , y , z ]
-- Valor de retorno:
--  :: Ord a => a          -- El número mediano.

---------------------------------------------------------------------------------
--Funcion 16//Funcion 3 iguales
-- Parámetros:
--  x, y, z :: Eq a => a  -- Los tres valores.
iguales :: Eq a => a -> a -> a -> Bool
iguales x y z = x == y && y == z
-- Valor de retorno:
--  :: Eq a => Bool       -- Verdadero si los tres valores son iguales.

---------------------------------------------------------------------------------
--Funcion 17//Funcion 3 diferentes
-- Parámetros:
--  x, y, z :: Eq a => a  -- Los tres valores.
diferentes :: Eq a => a -> a -> a -> Bool
diferentes x y z = x /= y && y /= z && x /= z
-- Valor de retorno:
--  :: Eq a => Bool       -- Verdadero si los tres valores son diferentes.

---------------------------------------------------------------------------------
--Funcion 18//Funcion 4 iguales
-- Parámetros:
--  x, y, z, u :: Eq a => a  -- Los cuatro valores.
cuatroIguales :: Eq a => a -> a -> a -> a -> Bool
cuatroIguales x y z u = iguales x y z && z == u
-- Valor de retorno:
--  :: Eq a => Bool          -- Verdadero si los cuatro valores son iguales.

---------------------------------------------------------------------------------