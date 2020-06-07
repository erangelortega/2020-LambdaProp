module Lib where
import Text.Show.Functions

laVerdad = True
type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto { 
  ambientes :: Int,
  superficie :: Int,
  precio :: Int,
  barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
}

ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
  (ordenarSegun criterio . filter (not . criterio x)) xs ++ 
  [x] ++ 
  (ordenarSegun criterio . filter (criterio x)) xs

between :: Ord a => a -> a -> a -> Bool
between cotaInferior cotaSuperior valor =
  valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
  Depto 3 80 7500 "Palermo", 
  Depto 1 45 3500 "Villa Urquiza", 
  Depto 2 50 5000 "Palermo", 
  Depto 1 45 5500 "Recoleta"]

  -- PUNTO 1 --
  {-Definir las funciones mayor y menor que reciban una función y dos valores, y retorna true si el resultado de evaluar 
  esa función sobre el primer valor es mayor o menor que el resultado de evaluarlo sobre el segundo valor respectivamente.-}

mayor :: Ord b => (a -> b) -> a -> a -> Bool
mayor funcion numr1 numr2 = funcion numr1 > funcion numr2 

menor :: Ord b => (a -> b) -> a -> a -> Bool
menor funcion numr1 numr2 = funcion numr1 < funcion numr2

  {-Mostrar un ejemplo de cómo se usaría una de estas funciones para ordenar una lista de strings en base a su longitud usando ordenarSegun.-}

listaEjemplo = ["hola", "como", "estas", "hoy", "?"]
ordenarStrings :: [String] -> [String]
ordenarStrings listaEjemplo = ordenarSegun (mayor length) listaEjemplo

ordenarStrings' :: [String] -> [String]
ordenarStrings' listaEjemplo = ordenarSegun (menor length) listaEjemplo


-- PUNTO 2 --
{-ubicadoEn que dada una lista de barrios que le interesan al usuario, retorne verdadero si el 
departamento se encuentra en alguno de los barrios de la lista.-}

barriosEjemplo = ["Palermo", "Villa del parque", "Villa Urquiza", "Recoleta", "Belgrano"]

-- **************************--
--    version 1
-- **************************--
--ubicado :: [Barrio] -> (Dpto -> Bool)
ubicadoEn ::  [Barrio] -> Requisito
ubicadoEn barrios dpto = any (== barrio dpto) barrios


-- **************************--
--    Version 2
-- **************************--
ubicadoEn' :: [Barrio] -> Requisito
ubicadoEn' barriosDeInteres depto = elem (barrio depto) barriosDeInteres

{-cumpleRango que a partir de una función y dos números, indique si el valor retornado por la función 
al ser aplicada con el departamento se encuentra entre los dos valores indicados.-}

cumpleRango :: (Num a, Ord a) => (Depto -> a) -> a -> a -> Requisito
cumpleRango f numr1 numr2 dpto = between numr1 numr2 . f $ dpto