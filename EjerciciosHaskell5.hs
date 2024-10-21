------------------------------------------------------------------------------
------------------------------------------------------------------------------
-------------------------------Árboles----------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
import Data.Array

data Arbol a = Hoja | Nodo a (Arbol a)(Arbol a) deriving(Show, Eq)

generarNodo :: a -> Arbol a 
generarNodo x = Nodo x Hoja Hoja 

----------------------------Insertar desde un arreglo-------------------------
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

---------------------------Buscar un elemento en un árbol--------------------    
--buscar elementos en arbol

buscar _ Hoja = False  
buscar x (Nodo a izq der)
  | x == a = True     
  | x < a  = buscar x izq  
  | x > a  = buscar x der 

----------------Recorridos (Inorden, posorden, preorden)---------------------

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