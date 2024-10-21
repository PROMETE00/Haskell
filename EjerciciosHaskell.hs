import Data.Array
--Funcion 1 //calcular promedio aritmetica de 3 calificaciones

promedioAritmetica x y z = (x + y + z ) / 3

--Funcion 2//calcular sumar monedas 

sumaMonedas a b c d e = a + (b*2) + (c*5) + (d*10) + (e*20)

--Funcion 3//calcular volumen de una esfera 4/3 * pi * r^3

volumeEsfera r = (4/3) * pi * (r ^ 3)

--Funcion 4//area de una corona
--r1 es el radio de la circunferencia interior y r2 de la exterior

areaCoronaCircular r1 r2 = pi *(r2^2 - r1^2)

--Funcion 5//obtener la ultima cifra//rem es el resto de
--una división entera

ultimaCifra x = rem x 10

--Funcion 6//maximo de tres numeros 

maximoDeTres x y z = max x (max y z)

--Funcion 7//rotar lista//head devuelve el primer elemento
--tail devuelve todos menos el primer elemento

rotar xs = tail xs ++ [head xs]

--Funcion 8//rotar n elementos //take devuelve una lista 
--compuesta por los primeros n elementos de una lista 
--original.drop devuelve una lista después de eliminar 
--los primeros n elementos.

rotaNElem n xs = drop n xs ++ take n xs

--Funcion 9//nueva lista con el rango min y max

rangoMinMax xs = [min xs , max xs]

--Funcion 10//verificar si una lista es palindromo 

palindromo xs = xs == reverse xs

--Funcion 11//(interior xs) es la
-- lista obtenida eliminando los extremos de la lista xs.
--init es la lista obtenida aliminando el ultimo elemento de xs.
--tail lista obtenida eliminando el primer elemento.

interior xs = init (tail xs)

--Funcion 13//Extrae un segmento de una lista, comprendido entre los índices a , b .
--take es la lista de los n primeros elementos de xs.
--drop borra los n primeros elementos de xs.
 
segmento a b  xs = take ( b-a + 1) (drop (a-1) xs)

--Funcion 14//Obtener lista por n extremos
--length xs es el número de elementos de la lista xs.
--drop n xs borra los n primeros elementos de xs.
--take n xs es la lista de los n primeros elementos de xs.

extremos a xs = take a xs ++ take a (drop (length xs - a) xs) 

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

---------------------------Recursividad -----------------------

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

---------------Nuevos tipos de datos ------------------------

data Estudiante = Estudiante {
    nombre :: String,
    apellido :: String,
    edad :: Int,
    numControl :: Int
} deriving (Show, Eq)

--lista de estudiantes

listaEstudiantes :: [Estudiante]
listaEstudiantes = [
    Estudiante "Juan" "Pérez" 20 21160720,
    Estudiante "María" "Gómez" 22 21160721,
    Estudiante "Carlos" "López" 19 21160722,
    Estudiante "Ana" "Martínez" 23 211607723,
    Estudiante "Luis" "Rodriguez" 21 2160724,
    Estudiante "Sofia" "Jiménez" 18 21160725,
    Estudiante "Pedro" "Torres" 24 21160726,
    Estudiante "Laura" "Hernández" 21 21160727,
    Estudiante "Daniel" "Morales" 20 21160728,
    Estudiante "Paula" "Ortiz" 22 21160729
    ]



---------------------Árboles---------------------------------

data Arbol a = Hoja | Nodo a (Arbol a)(Arbol a) deriving(Show, Eq)

generarNodo :: a -> Arbol a 
generarNodo x = Nodo x Hoja Hoja 

insertar x  Hoja = generarNodo x
insertar x (Nodo a izq der)
 | x<a = Nodo a (insertar x izq) der
 | x>a = Nodo a izq (insertar x der)

-- Función para insertar los elementos de un arreglo en el árbol
insertarArr :: (Ord a) => Array Int a -> Arbol a -> Arbol a
insertarArr arr arbol = insertarElementos (indices arr) arbol
  where
    insertarElementos [] arbolActual = arbolActual
    insertarElementos (i:is) arbolActual = insertarElementos is (insertar (arr ! i) arbolActual)

--buscar elementos en arbol

buscar _ Hoja = False  
buscar x (Nodo a izq der)
  | x == a = True     
  | x < a  = buscar x izq  
  | x > a  = buscar x der 

--Recorrido InOrden

inOrden :: Arbol a -> [a]
inOrden Hoja = []
inOrden (Nodo a izq der) = inOrden izq ++[a]++inOrden der

--Recorrido PreOrden

preOrden :: Arbol a -> [a]
preOrden Hoja = []
preOrden (Nodo a izq der) = [a] ++ preOrden izq ++ preOrden der

--Recorrido PosOrden

posOrden :: Arbol a -> [a]
posOrden Hoja = []
posOrden (Nodo a izq der) = posOrden izq ++ posOrden der ++ [a]