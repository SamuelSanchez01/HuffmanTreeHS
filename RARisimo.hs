module RARisimo (
    frecuencias,
    ganadores,
    huffman,
    rarisimo
) where 

import Frecuencia 
import Huffman
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromJust, isJust)
-- Dada una cadena de caracteres, devuelve una lista de Frecuencia con 
-- la cantidad de ocurrencias de cada caracter presente en la cadena
-- El valor asociado a la fecuencia es una hoja de tipo Huffman del caracter
frecuencias :: String -> [Frecuencia Huffman]
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
-- El valor asociado a la fecuencia es una hoja de tipo Huffman del caracter
aux_frecuencias :: [Char] -> [Char] -> [Frecuencia Huffman]
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
-- como arbol de Huffman
-- Si se aplica huffman sobre una cadena de caracteres vacıa, devuelve Nothing.
huffman :: String -> Maybe Huffman
huffman "" = Nothing
huffman s = aux_huffman (frecuencias s)

-- Funcion auxiliar para huffman 
aux_huffman :: [Frecuencia Huffman] -> Maybe Huffman
aux_huffman [f] = Just $ valor f
aux_huffman bosque = 
    case ganadores bosque of
    Just (min1, min2, resto_del_bosque) -> 
        let 
            nuevoArbol = fusionHuffman (valor min1) (valor min2)
            suma = (frecuencia min1) + (frecuencia min2)
            nuevaFrecuencia = Frecuencia (nuevoArbol, suma)
        in aux_huffman (nuevaFrecuencia:resto_del_bosque)

    Nothing -> Nothing

--------

-- Dada una cadena de caracteres, construye la “Codificacion de Huffman”
-- asociada a cada caracter, basado en el algoritmo propuesto para la 
-- funcion huffman
rarisimo :: String -> Map Char String
rarisimo palabra = 
    let maybeHuffman = huffman palabra
    in if isJust maybeHuffman
       then codificacion (fromJust maybeHuffman)
       else Map.empty