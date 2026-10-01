module Frecuencia(
    Frecuencia(..),
    iniciarFrecuencia,
    contar,
    valor,
    frecuencia
) where 

-- tipo Frecuencia contiene las ocurrencia de un valor tipo a
newtype Frecuencia a = Frecuencia (a, Int)
    deriving Show

instance Eq (Frecuencia a) where
    (==) :: (Frecuencia a) -> (Frecuencia b) -> Bool 
    (Frecuencia (x, n)) == (Frecuencia (y, m)) = n == m 

    (/=) :: (Frecuencia a) -> (Frecuencia b) -> Bool 
    (Frecuencia (x, n)) /= (Frecuencia (y, m)) = n /= m 

instance Ord (Frecuencia a) where

    (<) :: (Frecuencia a) -> (Frecuencia b) -> Bool
    (Frecuencia (x, n)) < (Frecuencia (y, m)) = n < m

    (<=) :: (Frecuencia a) -> (Frecuencia b) -> Bool
    (Frecuencia (x, n)) <= (Frecuencia (y, m)) = n <= m
    
    (>) :: (Frecuencia a) -> (Frecuencia b) -> Bool
    (Frecuencia (x, n)) > (Frecuencia (y, m)) = n > m

    (>=) :: (Frecuencia a) -> (Frecuencia b) -> Bool
    (Frecuencia (x, n)) >= (Frecuencia (y, m)) = n >= m

-- Dado un valor, devuelve una frecuencia que representa 
-- el haber encontrado uno solo de estos valores
iniciarFrecuencia :: Eq a => a -> Frecuencia a
iniciarFrecuencia x = Frecuencia (x, 1)

-- Recibe un valor y una lista, 
-- devuelve la frecuencia con la que aparece dicho valor en la lista
contar :: Eq a => a -> [a] -> Frecuencia a
contar x l = Frecuencia (x, length $ filter ((==) x) l) 

-- Recibe una frecuencia y devuelve el valor que se esta contando
valor :: Frecuencia a -> a
valor (Frecuencia (x, _)) = x

-- Recibe una frecuencia y devuelve la cantidad que se ha contado
frecuencia :: Frecuencia a -> Int
frecuencia (Frecuencia (_, n)) = n

