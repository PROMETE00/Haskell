------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------Guardas y Patrones -----------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--Funcion 1 p2//division segura
-- Parámetros:
--  x, y :: Double  -- Los números a dividir.
divisionSegura :: Double -> Double -> Double
divisionSegura x y 
  | x /= 0 && y /= 0 = x / y
  | otherwise = 9999.0
-- Valor de retorno:
--  :: Double  -- Resultado de la división o 9999.0 si y es 0.

---------------------------------------------------------------------------------------
--Funcion 2 p2//disyuncion excluyente 
-- Parámetros:
--  x, y :: Bool  -- Los valores booleanos a comparar.
xor1 :: Bool -> Bool -> Bool
xor1 x y 
  | x && not y = True
  | not x && y = True
  | otherwise = False
-- Valor de retorno:
--  :: Bool  -- Resultado de la disyunción excluyente.

---------------------------------------------------------------------------------------
--Funcion 3 p2//EL mayor rectangulo de 2 dados
-- Parámetros:
--  r1, r2 :: (Integer, Integer)  -- Dos rectángulos representados por sus dimensiones.
mayorRectangulo :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
mayorRectangulo r1 r2
  | area r1 == area r2 = r1
  | area r1 > area r2 = r1
  | otherwise         = r2
  where
    area (a, b) = a * b
-- Valor de retorno:
--  :: (Integer, Integer)  -- El rectángulo con mayor área.

---------------------------------------------------------------------------------------
--Funcion 4 p2//intercambiar cords
-- Parámetros:
--  a, b :: (a, b)  -- Tupla de dos elementos.
intercambia :: (a, b) -> (b, a)
intercambia (a, b) = (b, a)
-- Valor de retorno:
--  :: (b, a)  -- Tupla con los elementos intercambiados.

---------------------------------------------------------------------------------------
--Funcion 5 p2//distancia entre 2 puntos
-- Parámetros:
--  p1, p2 :: (Double, Double)  -- Dos puntos en el plano.
distancia :: (Double, Double) -> (Double, Double) -> Double
distancia (a, b) (c, d) 
  | a == d = 0
  | d > a = d - a 
-- Valor de retorno:
--  :: Double  -- Distancia entre los puntos (aunque la implementación parece incorrecta).

---------------------------------------------------------------------------------------
--Funcion 6 p2//permuta los elementos cilclicamente 
--de una lista pasado el ultimo elemento al principio
--last es al último elemento de la lista xs.init es la lista 
--obtenida eliminando el ultimo elemento de xs 
-- Parámetros:
--  xs :: [a]  -- Lista de elementos.
ciclo :: [a] -> [a]
ciclo [] = []                     
ciclo [x] = [x]                   
ciclo xs = last xs : init xs    
-- Valor de retorno:
--  :: [a]  -- Lista con el último elemento al principio.

---------------------------------------------------------------------------------------
--Funcion 7 p2//el numero mayor construido por los 2 digitos 
-- Parámetros:
--  a, b :: (Num a, Ord a) => a  -- Dos dígitos.
numeroMayor :: (Num a, Ord a) => a -> a -> a
numeroMayor a b 
  | a < b = (b * 10) + a
  | a > b = (a * 10) + b
-- Valor de retorno:
--  :: (Num a, Ord a) => a  -- El número mayor construido a partir de los dos dígitos.

---------------------------------------------------------------------------------------
--Funcion 8 p2//Numero de raices reales
--se ocupa lo que esta dentro de la raiz cuadrada de la formula general
-- Parámetros:
--  a, b, c :: (Floating t, Ord t) => t  -- Coeficientes de la ecuación cuadrática.
numeroDeRaices :: (Floating t, Ord t) => t -> t -> t -> Int
numeroDeRaices a b c 
  | discriminante > 0 = 2
  | discriminante == 0 = 1
  | otherwise = 0
  where
    discriminante = b^2 - 4 * a * c
-- Valor de retorno:
--  :: Int  -- Número de raíces reales.

---------------------------------------------------------------------------------------
--Funcion 9 p2// lista de raices reales //Usando formula general
-- Parámetros:
--  a, b, c :: Double  -- Coeficientes de la ecuación cuadrática.
raices :: Double -> Double -> Double -> [Double]
raices a b c 
  | discriminante > 0 = [r1, r2]
  | discriminante == 0 = [r1]
  | otherwise = []
  where
    discriminante = b^2 - 4 * a * c
    r1 = (-b + sqrt discriminante) / (2 * a)
    r2 = (-b - sqrt discriminante) / (2 * a)
-- Valor de retorno:
--  :: [Double]  -- Lista de raíces reales.

---------------------------------------------------------------------------------------
--Funcion 10 p2// area triangulo 
-- Parámetros:
--  a, b, c :: Double  -- Lados del triángulo.
area :: Double -> Double -> Double -> Double
area a b c = sqrt s * (s - a) * (s - b) * (s - c)
  where 
    s = (a + b + c) / 2
-- Valor de retorno:
--  :: Double  -- Área del triángulo.

---------------------------------------------------------------------------------------
--Funcion 11 p2//Funcion calcular interseccion max a, min b
-- Parámetros:
--  lista1, lista2 :: Ord a => [a]  -- Dos listas de elementos ordenables.
interseccion :: Ord a => [a] -> [a] -> [a]
interseccion [] _ = []
interseccion _ [] = []
interseccion [a1, b1] [a2, b2] 
  | max a1 a2 <= min b1 b2 = [max a1 a2, min b1 b2]
  | otherwise = []
-- Valor de retorno:
--  :: [a]  -- Lista de intersección.

---------------------------------------------------------------------------------------
--Funcion 12 p2//linea n de un trinagulo aritmetico
--inicio = (n(n-1)/2)+1
--fin = inicio + n -1
-- Parámetros:
--  n :: Integer  -- Número de la línea en el triángulo aritmético.
linea :: Integer -> [Integer]
linea n = [inicio..fin]
  where
    inicio = (n * (n - 1)) `div` 2 + 1
    fin = inicio + n - 1
-- Valor de retorno:
--  :: [Integer]  -- Lista de números en la línea n de un triángulo aritmético.

---------------------------------------------------------------------------------------