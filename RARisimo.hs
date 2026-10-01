module RARisimo (
    frecuencias,
    ganadores,
    hoffman,
    rarisimo
) where 

import Frecuencia 
import Hoffman
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromJust, isJust)
-- Dada una cadena de caracteres, devuelve una lista de Frecuencia con 
-- la cantidad de ocurrencias de cada caracter presente en la cadena
-- El valor asociado a la fecuencia es una hoja de tipo Hoffman del caracter
frecuencias :: String -> [Frecuencia Hoffman]
frecuencias s = aux_frecuencias s (letras_diferentes s [])

-- Funciones auxiliares para la funcion frecuencias 

-- Dado una lista de caracteres, devuelve una lista con los 
-- mismo caracteres sin repeticion
letras_diferentes :: [Char] -> [Char] -> [Char]
letras_diferentes [] d = d
letras_diferentes (x: xs) d
    | elem x d = letras_diferentes xs d 
    | otherwise = letras_diferentes xs (x : d)

-- Dadas dos listas de caracteres l1 l2, devuelve una lista
-- de Frecuencia con la cantidad de ocurrencias de cada caracter de l2
-- en l1
-- El valor asociado a la fecuencia es una hoja de tipo Hoffman del caracter
aux_frecuencias :: [Char] -> [Char] -> [Frecuencia Hoffman]
aux_frecuencias l1 l2 = [ fh | x <- l2, 
                               fc <- [contar x l1],
                               fh <- [Frecuencia ((Hoja x), (frecuencia fc))]
                        ]
                        
--------

-- Dada una lista de frecuencias, devuelve una tripleta que contiene:
-- 1. El objeto con la menor frecuencia
-- 2. El objeto con la segunda menor frecuencia
-- 3. El resto de los elementos de la lista, menos los dos seleccionados.
-- -- Si la lista no tiene al menos dos elementos, debe devolver Nothing.
ganadores :: [Frecuencia a] -> Maybe (Frecuencia a, Frecuencia a, [Frecuencia a])
ganadores list 
    | length list < 2 = Nothing
    | otherwise = Just (min1, min2, resto) where
        (min1 : (min2 : resto)) = qsort list 

-- Funcion auxiliar para ganadores 
-- Implementacion de quicksort
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y < x] ++ [x] ++ qsort [y | y <- xs, y >= x]

--------

-- Dada una cadena de caracteres no vacıa, construye su representacion
-- como arbol de Hoffman
-- Si se aplica hoffman sobre una cadena de caracteres vacıa, devuelve Nothing.
hoffman :: String -> Maybe Hoffman
hoffman "" = Nothing
hoffman s = aux_hoffman (frecuencias s)

-- Funcion auxiliar para hoffman 
aux_hoffman :: [Frecuencia Hoffman] -> Maybe Hoffman
aux_hoffman [f] = Just $ valor f
aux_hoffman bosque = 
    case ganadores bosque of
    Just (min1, min2, resto_del_bosque) -> 
        let 
            nuevoArbol = fusionHoffman (valor min1) (valor min2)
            suma = (frecuencia min1) + (frecuencia min2)
            nuevaFrecuencia = Frecuencia (nuevoArbol, suma)
        in aux_hoffman (nuevaFrecuencia:resto_del_bosque)

    Nothing -> Nothing

--------

-- Dada una cadena de caracteres, construye la “Codificacion de Hoffman”
-- asociada a cada caracter, basado en el algoritmo propuesto para la 
-- funcion hoffman
rarisimo :: String -> Map Char String
rarisimo palabra = 
    let maybeHoffman = hoffman palabra
    in if isJust maybeHoffman
       then codificacion (fromJust maybeHoffman)
       else Map.empty