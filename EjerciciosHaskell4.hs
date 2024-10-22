------------------------------------------------------------------------------
------------------------------------------------------------------------------
---------------------------Nuevos tipos de datos -----------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--Definicion del tipo estudiante
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

-------------Lista ordenada de los estudiantes de acuerdo a la edad------------

-- Funciónes para insertar un estudiante en una lista ordenada por edad.
-- Parámetros:
--  est :: Estudiante                -- El estudiante a insertar.
--  estudiantes :: [Estudiante]      -- La lista de estudiantes ordenada.
insertaEstudiante :: Estudiante -> [Estudiante] -> [Estudiante]
insertaEstudiante est [] = [est]
insertaEstudiante est (x:xs)
  | edad est <= edad x = est : x : xs
  | otherwise          = x : insertaEstudiante est xs
-- Valor de retorno:
--  :: [Estudiante]  -- Lista de estudiantes con el nuevo estudiante insertado.

-- Función para ordenar estudiantes por edad.
-- Parámetros:
--  estudiantes :: [Estudiante]  -- Lista de estudiantes a ordenar.
ordenaEstudiantesPorEdad :: [Estudiante] -> [Estudiante]
ordenaEstudiantesPorEdad [] = []
ordenaEstudiantesPorEdad (x:xs) = insertaEstudiante x (ordenaEstudiantesPorEdad xs)
-- Valor de retorno:
--  :: [Estudiante]  -- Lista de estudiantes ordenada por edad.

-------------Obtener al estudiante menor y mayor -----------------------------

--Obtener el estudiante con la menor edad de la lista.
-- Parámetros:
--  estudiantes :: [Estudiante]  -- Lista de estudiantes.
estudianteMenor :: [Estudiante] -> Estudiante
estudianteMenor [est] = est  
estudianteMenor (x:xs)
  | edad x <= edad menorRestante = x
  | otherwise = menorRestante
  where
    menorRestante = estudianteMenor xs
-- Valor de retorno:
--  :: Estudiante  -- Estudiante con la menor edad.

-- Obtener el estudiante con la mayor edad de la lista.
-- Parámetros:
--  estudiantes :: [Estudiante]  -- Lista de estudiantes.

--Obtener el estudiante con la mayor edad de la lista.
estudianteMayor :: [Estudiante] -> Estudiante
estudianteMayor [est] = est 
estudianteMayor (x:xs)
  | edad x >= edad mayorRestante = x
  | otherwise = mayorRestante
  where
    mayorRestante = estudianteMayor xs
-- Valor de retorno:
--  :: Estudiante  -- Estudiante con la mayor edad.

------------Obtener el promedio de edades-------------------------------------

--Calcular el promedio de las edades de los estudiantes.
-- Parámetros:
--  estudiantes :: [Estudiante]  -- Lista de estudiantes.
promedioEdades :: [Estudiante] -> Double
promedioEdades [] = 0  
promedioEdades estudiantes = fromIntegral (sumaEdades estudiantes) / fromIntegral (length estudiantes)
  where
    -- Sumar las edades de los estudiantes.
    -- Parámetros:
    --  estudiantes :: [Estudiante]  -- Lista de estudiantes.
    sumaEdades :: [Estudiante] -> Int
    sumaEdades [] = 0
    sumaEdades (x:xs) = edad x + sumaEdades xs
    -- Valor de retorno:
--  :: Double  -- Promedio de las edades de los estudiantes.
