data Persona = Persona {
  nombrePersona :: String,
  suerte :: Int,
  inteligencia :: Int,
  fuerza :: Int
} deriving (Show, Eq)

data Pocion = Pocion {
  nombrePocion :: String,
  ingredientes :: [Ingrediente]
}

type Efecto = Persona -> Persona

data Ingrediente = Ingrediente {
  nombreIngrediente :: String,
  efectos :: [Efecto]
}

nombresDeIngredientesProhibidos :: [String]
nombresDeIngredientesProhibidos = [
 "sangre de unicornio",
 "veneno de basilisco",
 "patas de cabra",
 "efedrina"]

maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun _ [ x ] = x
maximoSegun  f ( x : y : xs)
  | f x > f y = maximoSegun f (x:xs)
  | otherwise = maximoSegun f (y:xs)

sumaDeNiveles :: Persona -> Int
sumaDeNiveles persona = suerte persona + inteligencia persona + fuerza persona

niveles :: Persona -> [Int]
niveles persona = [suerte persona, inteligencia persona, fuerza persona]

diferenciaDeNiveles :: Persona -> Int
diferenciaDeNiveles persona = (maximum.niveles) persona - (minimum.niveles) persona

nivelesMayoresA :: Int -> Persona -> Int
nivelesMayoresA piso = length.filter (piso <).niveles

efectosDePocion :: Pocion -> [Efecto]
efectosDePocion = concatMap efectos . ingredientes

--Los nombres de las pociones hardcore, que son las que tienen al menos 4 efectos.
pocionesHardcore :: [Pocion] -> [String]
pocionesHardcore = map nombrePocion . filter ((>=4) . length . efectosDePocion)

--La cantidad de pociones prohibidas, que son aquellas que tienen algún ingrediente cuyo nombre 
--figura en la lista de ingredientes prohibidos.
cantidadDePocionesProhibidas :: [Pocion] -> Int
cantidadDePocionesProhibidas = length . filter esPocionProhibida 

esPocionProhibida :: Pocion -> Bool --Nos dice si la pocion tiene algun ingrediente prohibido
esPocionProhibida = any ((`elem` nombresDeIngredientesProhibidos) . nombreIngrediente) . ingredientes


--Si son todas dulces, lo cual ocurre cuando todas las pociones de la lista tienen algún ingrediente llamado “azúcar”.
sonTodasDulces :: [Pocion] -> Bool
sonTodasDulces = all esPocionDulce

esPocionDulce :: Pocion -> Bool
esPocionDulce = any (("azucar" ==) . nombreIngrediente) . ingredientes

-- 4. Definir la función tomarPocion que recibe una poción y una persona, y devuelve como quedaría la 
-- persona después de tomar la poción. Cuando una persona toma una poción, se aplican 
-- todos los efectos de esta última, en orden. 

tomarPocion :: Pocion -> Persona -> Persona
tomarPocion pocion personainicial = foldl (\persona efecto -> efecto persona)  personainicial (efectosDePocion pocion)

esAntidotoDe :: Pocion -> Pocion -> Persona -> Bool
esAntidotoDe antidoto pocion personainicial = 
    ( (personainicial ==) . tomarPocion antidoto . tomarPocion pocion) personainicial