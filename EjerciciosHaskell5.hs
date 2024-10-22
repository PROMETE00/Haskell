------------------------------------------------------------------------------
------------------------------------------------------------------------------
-------------------------------Árboles----------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
import Data.Array

data Arbol a = Hoja | Nodo a (Arbol a)(Arbol a) deriving(Show, Eq)

----------------------------Generar Nodo----------------------------

-- Parámetros:
-- x :: a - Valor para el nodo
generarNodo :: a -> Arbol a 
generarNodo x = Nodo x Hoja Hoja 
-- Valor de retorno:
-- Nodo con valor `x` y hojas vacías.

----------------------------Insertar desde un arreglo-------------------------

--Insertar Elemento
-- Parámetros:
-- x :: Ord a => a - Valor a insertar.
-- Arbol a - Árbol en el que se insertará el valor.
insertar :: Ord a => a -> Arbol a -> Arbol a
insertar x  Hoja = generarNodo x
insertar x (Nodo a izq der)
  | x < a = Nodo a (insertar x izq) der
  | x > a = Nodo a izq (insertar x der)
-- Valor de retorno:
-- Arbol a - Árbol resultante con el valor insertado.

-- Función para insertar los elementos de un arreglo en el árbol
-- Parámetros:
-- arr :: Ord a => Array Int a - Arreglo con elementos a insertar.
-- arbol :: Arbol a - Árbol en el que se insertarán los elementos.
insertarArr :: (Ord a) => Array Int a -> Arbol a -> Arbol a
insertarArr arr arbol = insertarElementos (indices arr) arbol
  where
    insertarElementos [] arbolActual = arbolActual
    insertarElementos (i:is) arbolActual = insertarElementos is (insertar (arr ! i) arbolActual)
-- Valor de retorno:
-- Arbol a - Árbol resultante con los elementos del arreglo insertados.

---------------------------Buscar un elemento en un árbol--------------------    

--buscar elementos en arbol
-- Parámetros:
-- x :: Ord a => a - Valor a buscar.
-- Arbol a - Árbol en el que se buscará el valor.
buscar :: Ord a => a -> Arbol a -> Bool
buscar _ Hoja = False  
buscar x (Nodo a izq der)
  | x == a = True     
  | x < a  = buscar x izq  
  | x > a  = buscar x der 
-- Valor de retorno:
-- Bool - True si el valor está en el árbol, False si no lo está.

----------------Recorridos (Inorden, posorden, preorden)---------------------

----------------------------Recorrido InOrden----------------------------

-- Parámetros:
-- Arbol a - Árbol a recorrer.
inOrden :: Arbol a -> [a]
inOrden Hoja = []
inOrden (Nodo a izq der) = inOrden izq ++ [a] ++ inOrden der
-- Valor de retorno:
-- [a] - Lista de los valores en orden ascendente (inorden).

----------------------------Recorrido PreOrden----------------------------

-- Parámetros:
-- Arbol a - Árbol a recorrer.
preOrden :: Arbol a -> [a]
preOrden Hoja = []
preOrden (Nodo a izq der) = [a] ++ preOrden izq ++ preOrden der
-- Valor de retorno:
-- [a] - Lista de los valores en preorden (raíz primero).

----------------------------Recorrido PosOrden----------------------------

-- Parámetros:
-- Arbol a - Árbol a recorrer.
posOrden :: Arbol a -> [a]
posOrden Hoja = []
posOrden (Nodo a izq der) = posOrden izq ++ posOrden der ++ [a]
-- Valor de retorno:
-- [a] - Lista de los valores en posorden (subárboles primero y raíz al final).

------------------------------------------------------------------------------