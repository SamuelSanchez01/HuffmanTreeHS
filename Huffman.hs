module Huffman(
    Huffman(..),
    nuevoHuffman,
    fusionHuffman,
    obtenerCaracter,
    arbolIzquierdo,
    arbolDerecho,
    codificacion
) where

import qualified Data.Map as Map
import Data.Map (Map)

-- Creamos el tipo de dato Huffman
-- Donde puede ser una Hoja o una Rama con dos Huffmans
data Huffman =  Hoja Char  | Rama Huffman Huffman

-- Instancia de Show para Huffman  
-- Convierte de Huffman a String  
instance Show Huffman where
    show :: Huffman -> String
    show (Hoja c) = "<>" ++ show c
    show (Rama a b) = "=>(" ++ show a ++ " " ++ show b ++")"

-- Instancia de Read para Huffman
-- Convierte de String a Huffman, es necesario indicar el tipo durante su ejecucion
instance Read Huffman where
    readsPrec _ str = parseHuffman (dropWhile (== ' ') str)

-- Función auxiliar que nos permitira recorrer 
-- el string elemento por elemento.
-- Ademas ira diferenciando los paréntesis

parseHuffman :: String -> [(Huffman , String)]
parseHuffman str =
    case dropWhile (== ' ') str of
        -- Si encuentra un '('
        ('(':resto) ->
            parseHuffman $ dropWhile (== ' ') resto
        -- Si encuentra un ')'
        (')':resto) ->
            parseHuffman $ dropWhile (== ' ') resto
        -- Si encuentra una Hoja
        ('H':'o':'j':'a':resto) ->
            [(Hoja x, resto') | 
                (x, resto') <- reads resto
            ]
        -- Si encuentra una Rama
        ('R':'a':'m':'a':resto) ->
            [(Rama izq der, resto'') |
                (izq, resto1) <- parseHuffman resto,
                (der, resto2) <- parseHuffman resto1,
                (')':resto'') <- [dropWhile (== ' ') resto2]
            ]
        -- Si encuentra una Hoja pero version Show
        ('<':'>':resto) ->
            [(Hoja x, resto') | (x, resto') <- reads resto]
        -- Si encuentra una Rama pero version Show
        ('=':'>':'(':resto) ->
            [(Rama izq der, resto'') |
                (izq, resto1) <- parseHuffman resto,
                (der, resto2) <- parseHuffman resto1,
                (')':resto'') <- [dropWhile (== ' ') resto2]
            ]
        _ -> [] -- Si no reconoce la entrada, retorna vacio

-- Función que recibe un caracter y devuelve un arbol de Huffman 
-- con una unica hoja con el caracter
nuevoHuffman :: Char -> Huffman 
nuevoHuffman = Hoja 

-- Función que fusiona dos arboles de Huffman, ya sean hojas o ramas
fusionHuffman :: Huffman -> Huffman -> Huffman
fusionHuffman = Rama

-- Función que recibe un arbol de Huffman y devuelve el caracter de la hoja
-- Si se recibe una rama, se lanza un error
obtenerCaracter :: Huffman -> Char
obtenerCaracter (Hoja c) = c
obtenerCaracter (Rama _ _) = error "No se puede obtener caracter de una rama"

-- Función que recibe un arbol de Huffman y devuelve el arbol izquierdo
-- Si se recibe una hoja, se lanza un error
arbolIzquierdo :: Huffman -> Huffman
arbolIzquierdo (Rama a _) = a
arbolIzquierdo (Hoja _) = error "No se puede obtener arbol izquierdo de una hoja"

-- Función que recibe un arbol de Huffman y devuelve el arbol derecho
-- Si se recibe una hoja, se lanza un error
arbolDerecho :: Huffman -> Huffman 
arbolDerecho (Rama _ b) = b
arbolDerecho (Hoja _)  = error "No se puede obtener arbol derecho de una hoja"

-- Función que recibe un arbol de Huffman y devuelve un Map con la codificación de cada caracter
codificacion :: Huffman -> Map Char String
-- Si se recibe una hoja, se devuelve un Map con el caracter y su codificación, es decir, [caracter, ""]
codificacion (Hoja c) = Map.singleton c ""

-- Si se recibe una rama, se fusionan los Map de los arboles izquierdo y derecho
-- Se agrega un '0' a los valores del arbol izquierdo y un '1' a los valores del arbol derecho
codificacion (Rama a b) = Map.union (Map.map ('0':) $ codificacion (arbolIzquierdo (Rama a b)) ) (Map.map ('1':) $ codificacion (arbolDerecho (Rama a b)) )
