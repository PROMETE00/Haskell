------------------------------------------------------------------------------
------------------------------------------------------------------------------
---------------------------------Recursividad --------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

--Funcion 1 p3//Potencia de x
--x^n= x * x ^ n-1

potencia :: Integer -> Integer -> Integer
potencia _ 0 = 1
potencia x n = x * potencia x ( n - 1 )

--Funcion 2 p3//Maximo comun divisor
-- mod a/b //hasta b->0
--mod x y es el resto de x entre y 

mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b =  mcd b (a `mod` b) 

--Funcion 3 p3//verificar si el elemento pertenece a la lista

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys) 
 | x == y = True
 | otherwise = pertenece x ys

--Funcion 4 p3//tomar x elementos de una lista

tomar :: Int -> [a] -> [a]
tomar 0 xs = []
tomar _ [] = []
tomar n (x:xs) = x : tomar ( n-1 ) xs

--Funcion 5 p3//numero a lista de digitos
--read c es la expresion representada por la cadena c
--abs x es el valor absoluto de x

digitosC :: Integer -> [Integer]
digitosC n = [read [c] | c <- show (abs n)]

--Funcion 6 p3//suma de digitos 
--mod x y es el resto de x entre y 
--div x y es la division entera  de x entre y 

sumaDigitosR :: Integer -> Integer
sumaDigitosR n = (n `mod` 10) + sumaDigitosR (n `div`10)

-----------------Ejercicio lista-----------------------------

--Funcion 2.1//

ordenaRapida [] = []
ordenaRapida (x:xs) = ordenaRapida [y | y <- xs , y <= x] ++ [x] ++ ordenaRapida [y | y <- xs , y > x]
