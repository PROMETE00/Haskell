------------------------------------------------------------------------------
------------------------------------------------------------------------------
---------------------------------Recursividad --------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--Funcion 1 p3//Potencia de x
--x^n= x * x ^ n-1
-- Parámetros:
--  x :: Integer  -- La base.
--  n :: Integer  -- El exponente.
potencia :: Integer -> Integer -> Integer
potencia _ 0 = 1
potencia x n = x * potencia x (n - 1)
-- Valor de retorno:
--  :: Integer  -- Resultado de x elevado a la n.

---------------------------------------------------------------------------------------
--Funcion 2 p3//Maximo comun divisor
-- mod a/b //hasta b->0
--mod x y es el resto de x entre y 
-- Parámetros:
--  a, b :: Integer  -- Los números para calcular el MCD.
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (a `mod` b)
-- Valor de retorno:
--  :: Integer  -- El máximo común divisor de a y b.

---------------------------------------------------------------------------------------
--Funcion 3 p3//verificar si el elemento pertenece a la lista
-- Parámetros:
--  x :: Eq a => a  -- El elemento a buscar.
--  ys :: [a]       -- La lista en la que buscar.
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys) 
  | x == y = True
  | otherwise = pertenece x ys
-- Valor de retorno:
--  :: Bool  -- True si x pertenece a la lista, False en caso contrario.

---------------------------------------------------------------------------------------
--Funcion 4 p3//tomar x elementos de una lista
-- Parámetros:
--  n :: Int       -- Número de elementos a tomar.
--  xs :: [a]     -- La lista de la que tomar elementos.
tomar :: Int -> [a] -> [a]
tomar 0 xs = []
tomar _ [] = []
tomar n (x:xs) = x : tomar (n - 1) xs
-- Valor de retorno:
--  :: [a]  -- Lista con los primeros n elementos de xs.

---------------------------------------------------------------------------------------
--Funcion 5 p3//numero a lista de digitos
--read c es la expresion representada por la cadena c
--abs x es el valor absoluto de x
-- Parámetros:
--  n :: Integer  -- El número a convertir en lista de dígitos.
digitosC :: Integer -> [Integer]
digitosC n = [read [c] | c <- show (abs n)]
-- Valor de retorno:
--  :: [Integer]  -- Lista de dígitos del número n.

---------------------------------------------------------------------------------------
--Funcion 6 p3//suma de digitos 
--mod x y es el resto de x entre y 
--div x y es la division entera  de x entre y 
-- Parámetros:
--  n :: Integer  -- El número del que se sumarán los dígitos.
sumaDigitosR :: Integer -> Integer
sumaDigitosR n = (n `mod` 10) + sumaDigitosR (n `div` 10)
-- Valor de retorno:
--  :: Integer  -- Suma de los dígitos de n.

-----------------Ejercicio lista-----------------------------

--Funcion 2.1//Funcion quick sort
-- Parámetros:
--  xs :: Ord a => [a]  -- La lista a ordenar.
ordenaRapida :: Ord a => [a] -> [a]
ordenaRapida [] = []
ordenaRapida (x:xs) = ordenaRapida [y | y <- xs, y <= x] ++ [x] ++ ordenaRapida [y | y <- xs, y > x]
-- Valor de retorno:
--  :: Ord a => [a]  -- Lista ordenada.