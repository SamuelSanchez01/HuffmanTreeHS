module Cliente (main) where
    
import RARisimo (rarisimo, hoffman)
import qualified Data.Map as Map
import System.IO
import System.Directory
import System.FilePath (dropExtension)
import qualified Data.ByteString as B
import Data.Word (Word8)
import Data.Bits (shiftL, shiftR, (.|.))
import Numeric (showIntAtBase)
import Data.Char (intToDigit, chr, ord) 
import Text.Read (readMaybe) 
import Hoffman

-- Función principal
main :: IO ()
main = do
    -- Imprimimos menu
    putStrLn "Bienvenido al cliente de compresion"
    menu

-- Opciones
menu :: IO ()
menu = do
    --opciones
    putStrLn "Seleccione una opcion:"
    putStrLn "1.- Codificar"
    putStrLn "2.- Decodificar"
    putStrLn "3.- Analizar"
    putStrLn "4.- Salir"
    opcion <- getLine
    --casos
    case opcion of
        "1" -> codificar >> menu
        "2" -> decodificarArchivo >> menu
        "3" -> analizar >> menu
        "4" -> putStrLn "Saliendo..."
        _   -> putStrLn "Opcion no válida" >> menu

--funcion para analzar
analizar :: IO ()
analizar = do
    --path y logica del archivo
    putStrLn "Ingrese el path del archivo a analizar:"
    path <- getLine
    content <- readFile path
    --imprimimos el contenido a encriptar
    putStrLn "Texto original leido:"
    putStrLn content
    let contenido = content ++ "ë"
    --creamos el arbol
    let maybeTree = hoffman contenido
    case maybeTree of
        --arbol vacio
        Nothing -> putStrLn "El archivo esta vacio, no se puede codificar."
        --a codifical
        Just tree -> do
            --logica de codificacion
            let treeString = show tree
            let treeLength = length treeString
            let codMap = rarisimo contenido
            let bits = mapToBits codMap contenido
            let ignoredBits = (8 - length bits `mod` 8) `mod` 8
            let paddedBits = bits ++ replicate ignoredBits '0'
            let byteString = bitsToByteString (intToBits 8 ignoredBits ++ intToBits 32 treeLength ++ stringToBits treeString ++ paddedBits)

            --logica para guardar el archivo
            let originalSizeBits = length content * 8
            let compressedSizeBits = B.length byteString * 8
            let savingPercentage = (1 - fromIntegral compressedSizeBits / fromIntegral originalSizeBits) * 100
            putStrLn $ "Tamaño original: " ++ show originalSizeBits ++ " bits"
            putStrLn $ "Tamaño comprimido: " ++ show compressedSizeBits ++ " bits"
            putStrLn $ "Porcentaje de ahorro: " ++ show savingPercentage ++ "%"

--funcion para Codificar
codificar :: IO ()
codificar = do
    --path y logica del archivo
    putStrLn "Ingrese el path del archivo a codificar:"
    path <- getLine
    content <- readFile path
    --imprimimos el contenido a encriptar
    putStrLn "Texto original leido:"
    putStrLn content
    --creamos el arbol
    let contenido = content ++ "ë"
    let maybeTree = hoffman contenido
    case maybeTree of
        --arbol vacio
        Nothing -> putStrLn "El archivo esta vacio, no se puede codificar."
        --codifical
        Just tree -> do
            --logica de codificacion
            let treeString = show tree
            let treeLength = length treeString
            let codMap = rarisimo contenido
            let bits = mapToBits codMap contenido
            let ignoredBits = (8 - length bits `mod` 8) `mod` 8
            let paddedBits = bits ++ replicate ignoredBits '0'
            let byteString = bitsToByteString (intToBits 8 ignoredBits ++ intToBits 32 treeLength ++ stringToBits treeString ++ paddedBits)
            --guardar el archivo
            let newFileName = dropExtension path ++ ".raro"
            putStrLn $ "Nuevo nombre de archivo: " ++ newFileName
            --putStrLn treeString
            B.writeFile newFileName byteString
            putStrLn "Contenido codificado en bits guardado."

--Implementacion de la funcion decodificar
decodificarArchivo :: IO ()
decodificarArchivo = do
    --logica del archivo
    putStrLn "Ingrese el path del archivo a decodificar:"
    path <- getLine
    content <- B.readFile path
    --obtener cada data
    let (ignoredBitsBits, restBits1) = splitAt 8 (byteStringToBits content)
    let ignoredBits = bitsToInt ignoredBitsBits
    let (treeLengthBits, restBits2) = splitAt 32 restBits1
    let treeLength = bitsToInt treeLengthBits
    let (treeBits, encodedBitsWithPadding) = splitAt (treeLength * 8) restBits2
    let encodedBits = take (length encodedBitsWithPadding - ignoredBits) encodedBitsWithPadding
    let treeString = bitsToString treeBits
    --putStrLn treeString
    --descodificar
    let tree = readMaybe treeString :: Maybe Hoffman
    case tree of
        Nothing -> putStrLn "Error al decodificar el arbol de Hoffman."
        Just t -> do
            let decodedString = decodeHoffman t encodedBits
            putStrLn "Contenido decodificado:"
            putStrLn decodedString

--Funcion auxiliar para convertir una lista de bits a una cadena de caracteres
bitsToString :: String -> String
bitsToString [] = []
bitsToString bits = map (chr . bitsToInt) (chunksOf 8 bits)

--Funcion auxiliar para convertir una cadena de caracteres a una lista de bits
stringToBits :: String -> String
stringToBits = concatMap (padTo8Bits . intToBits 8 . ord) -- Utilizamos `ord` aquí

--Funcion auxiliar para convertir un entero en una lista de bits
intToBits :: Int -> Int -> String
intToBits n x = let bits = showIntAtBase 2 intToDigit x "" in replicate (n - length bits) '0' ++ bits

--Funcion auxiliar para convertir una lista de bits a entero
bitsToInt :: String -> Int
bitsToInt = foldl (\acc bit -> acc `shiftL` 1 .|. (if bit == '1' then 1 else 0)) 0

--Funcion para convertir ByteString a una cadena de bits
byteStringToBits :: B.ByteString -> String
byteStringToBits = concatMap byteToBits . B.unpack

--Funcion auxiliar para convertir un byte a una cadena de bits
byteToBits :: Word8 -> String
byteToBits byte = padTo8Bits (showIntAtBase 2 intToDigit byte "")

--Asegurar que la representacion binaria tenga 8 bits
padTo8Bits :: String -> String
padTo8Bits bits = replicate (8 - length bits) '0' ++ bits

--Funcion para convertir el mapa de codificacion en una cadena de bits
mapToBits :: Map.Map Char String -> String -> String
mapToBits codMap = concatMap (\c -> Map.findWithDefault "" c codMap)

--Funcion para convertir una cadena de bits a ByteString
bitsToByteString :: String -> B.ByteString
bitsToByteString bits = B.pack $ map bitsToWord8 (chunksOf 8 bits)

--Funcion auxiliar para convertir un segmento de bits (String) a Word8
bitsToWord8 :: String -> Word8
bitsToWord8 = foldl (\acc bit -> acc `shiftL` 1 .|. (if bit == '1' then 1 else 0)) 0

--Funcion auxiliar para dividir una cadena en segmentos de longitud n
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

--Funcion auxiliar para decodificar una cadena de bits usando un arbol de Hoffman
decodeHoffman :: Hoffman -> String -> String
decodeHoffman tree bits = decodeHelper tree bits tree
  where
    decodeHelper _ [] _ = []
    decodeHelper (Hoja c) bits' _ = c : decodeHelper tree bits' tree
    decodeHelper (Rama l r) (b:bs) originalTree
        | b == '0' = decodeHelper l bs originalTree
        | b == '1' = decodeHelper r bs originalTree
        | otherwise = decodeHelper originalTree bs originalTree