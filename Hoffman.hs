module Hoffman(
    Hoffman(..),
    nuevoHoffman,
    fusionHoffman,
    obtenerCaracter,
    arbolIzquierdo,
    arbolDerecho,
    codificacion
) where

import qualified Data.Map as Map
import Data.Map (Map)

-- Creamos el tipo de dato Hoffman
-- Donde puede ser una Hoja o una Rama con dos Hoffmans
data Hoffman =  Hoja Char  | Rama Hoffman Hoffman

-- Instancia de Show para Hoffman  
-- Convierte de Hoffman a String  
instance Show Hoffman where
    show :: Hoffman -> String
    show (Hoja c) = "<>" ++ show c
    show (Rama a b) = "=>(" ++ show a ++ " " ++ show b ++")"

-- Instancia de Read para Hoffman
-- Convierte de String a Hoffman, es necesario indicar el tipo durante su ejecucion
instance Read Hoffman where
    readsPrec _ str = parseHoffman (dropWhile (== ' ') str)

-- Función auxiliar que nos permitira recorrer 
-- el string elemento por elemento.
-- Ademas ira diferenciando los paréntesis

parseHoffman :: String -> [(Hoffman , String)]
parseHoffman str =
    case dropWhile (== ' ') str of
        -- Si encuentra un '('
        ('(':resto) ->
            parseHoffman $ dropWhile (== ' ') resto
        -- Si encuentra un ')'
        (')':resto) ->
            parseHoffman $ dropWhile (== ' ') resto
        -- Si encuentra una Hoja
        ('H':'o':'j':'a':resto) ->
            [(Hoja x, resto') | 
                (x, resto') <- reads resto
            ]
        -- Si encuentra una Rama
        ('R':'a':'m':'a':resto) ->
            [(Rama izq der, resto'') |
                (izq, resto1) <- parseHoffman resto,
                (der, resto2) <- parseHoffman resto1,
                (')':resto'') <- [dropWhile (== ' ') resto2]
            ]
        -- Si encuentra una Hoja pero version Show
        ('<':'>':resto) ->
            [(Hoja x, resto') | (x, resto') <- reads resto]
        -- Si encuentra una Rama pero version Show
        ('=':'>':'(':resto) ->
            [(Rama izq der, resto'') |
                (izq, resto1) <- parseHoffman resto,
                (der, resto2) <- parseHoffman resto1,
                (')':resto'') <- [dropWhile (== ' ') resto2]
            ]
        _ -> [] -- Si no reconoce la entrada, retorna vacio

-- Función que recibe un caracter y devuelve un arbol de Hoffman 
-- con una unica hoja con el caracter
nuevoHoffman :: Char -> Hoffman 
nuevoHoffman = Hoja 

-- Función que fusiona dos arboles de Hoffman, ya sean hojas o ramas
fusionHoffman :: Hoffman -> Hoffman -> Hoffman
fusionHoffman = Rama

-- Función que recibe un arbol de Hoffman y devuelve el caracter de la hoja
-- Si se recibe una rama, se lanza un error
obtenerCaracter :: Hoffman -> Char
obtenerCaracter (Hoja c) = c
obtenerCaracter (Rama _ _) = error "No se puede obtener caracter de una rama"

-- Función que recibe un arbol de Hoffman y devuelve el arbol izquierdo
-- Si se recibe una hoja, se lanza un error
arbolIzquierdo :: Hoffman -> Hoffman
arbolIzquierdo (Rama a _) = a
arbolIzquierdo (Hoja _) = error "No se puede obtener arbol izquierdo de una hoja"

-- Función que recibe un arbol de Hoffman y devuelve el arbol derecho
-- Si se recibe una hoja, se lanza un error
arbolDerecho :: Hoffman -> Hoffman 
arbolDerecho (Rama _ b) = b
arbolDerecho (Hoja _)  = error "No se puede obtener arbol derecho de una hoja"

-- Función que recibe un arbol de Hoffman y devuelve un Map con la codificación de cada caracter
codificacion :: Hoffman -> Map Char String
-- Si se recibe una hoja, se devuelve un Map con el caracter y su codificación, es decir, [caracter, ""]
codificacion (Hoja c) = Map.singleton c ""

-- Si se recibe una rama, se fusionan los Map de los arboles izquierdo y derecho
-- Se agrega un '0' a los valores del arbol izquierdo y un '1' a los valores del arbol derecho
codificacion (Rama a b) = Map.union (Map.map ('0':) $ codificacion (arbolIzquierdo (Rama a b)) ) (Map.map ('1':) $ codificacion (arbolDerecho (Rama a b)) )
