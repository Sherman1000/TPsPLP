module Tp where

import Data.List

type Texto = String
type Feature = Float
type Instancia = [Feature]
type Extractor = (Texto -> Feature)

type Datos = [Instancia]
type Etiqueta = String
type Modelo = (Instancia -> Etiqueta)
type Medida = (Instancia -> Instancia -> Float)

tryClassifier :: [Texto] -> [Etiqueta] -> Float
tryClassifier x y = let xs = extraerFeatures ([longitudPromedioPalabras, repeticionesPromedio] ++ frecuenciaTokens) x in
    nFoldCrossValidation 5 xs y

mean :: [Float] -> Float
mean xs = realToFrac (sum xs) / genericLength xs

split :: Eq a => a -> [a] -> [[a]]
split comparator = foldr f [[]]
		where f =(\x xss -> if x == comparator 
							then [] : xss 
							else (x : head(xss)) : tail(xss))
					
longitudPromedioPalabras :: Extractor
longitudPromedioPalabras xs =  mean (map (\x -> genericLength x) (split ' ' xs))

cuentas :: Eq a => [a] -> [(Int, a)]
cuentas xs = zip ([length (filter ((\z -> \x -> x==z) y) xs)| y <- sinRepetidos xs]) (sinRepetidos xs)

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos = nub

repeticionesPromedio :: Extractor
repeticionesPromedio xs = mean ( map (\tupla -> fromIntegral(fst tupla)) (cuentas (split ' ' xs)))

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

-- PREGUNTAR SI HAY QUE USAR REPETICIONES PROMEDIO!!!!
frecuenciaTokens :: [Extractor]
frecuenciaTokens = [ extractorAPartirDe token | token <- tokens ]

extractorAPartirDe :: Char -> [Char] -> Float
extractorAPartirDe token xs = fromIntegral (buscarValorEnTupla (filtrar token)) / fromIntegral( (sum (map (\tupla -> fst tupla) (cuentas xs))))
							where filtrar token = (filter ((\tok tupla -> (snd tupla)==tok) token) (cuentas xs))
						
buscarValorEnTupla:: [(Int, a)] -> Int
buscarValorEnTupla [] = 0
buscarValorEnTupla [x] = fst x 

normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor [] extractor = const 0
normalizarExtractor textos extractor = (\text ->  (extractor text) / (maximum (map (abs) [(extractor texto) | texto <- textos])))

extraerFeatures :: [Extractor] -> [Texto] -> Datos 
extraerFeatures extractores textos = [[ normalizarExtractor textos extractor texto | extractor <- extractores] | texto <- textos]

distEuclideana :: Medida
distEuclideana = undefined

distCoseno :: Medida
distCoseno = undefined

knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn = undefined

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy = undefined

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos = undefined

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation = undefined
