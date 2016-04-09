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

-- EJERCICIO 1
split :: Eq a => a -> [a] -> [[a]]
split comparator = foldr f [[]]
		where f =(\x xss -> if x == comparator 
							then [] : xss 
							else (x : head(xss)) : tail(xss))
				
-- EJERCICIO 2
longitudPromedioPalabras :: Extractor
longitudPromedioPalabras xs =  mean (map (\x -> genericLength x) (split ' ' xs))

-- EJERCICIO 3
cuentas :: Eq a => [a] -> [(Int, a)]
cuentas xs = zip ([length (filter ((\z -> \x -> x==z) y) xs)| y <- sinRepetidos xs]) (sinRepetidos xs)

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos = nub

-- EJERCICIO 4
repeticionesPromedio :: Extractor
repeticionesPromedio xs = mean ( map (\tupla -> fromIntegral(fst tupla)) (cuentas (split ' ' xs)))

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

-- EJERCICIO 5
-- PREGUNTAR SI HAY QUE USAR REPETICIONES PROMEDIO!!!!
frecuenciaTokens :: [Extractor]
frecuenciaTokens = [ extractorAPartirDe token | token <- tokens ]

extractorAPartirDe :: Char -> [Char] -> Float
extractorAPartirDe token xs = fromIntegral (buscarValorEnTupla (filtrar token)) / fromIntegral( (sum (map (\tupla -> fst tupla) (cuentas xs))))
							where filtrar token = (filter ((\tok tupla -> (snd tupla)==tok) token) (cuentas xs))
						
buscarValorEnTupla:: [(Int, a)] -> Int
buscarValorEnTupla [] = 0
buscarValorEnTupla [x] = fst x 

-- EJERCICIO 6
normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor [] extractor = const 0
normalizarExtractor textos extractor = (\text ->  (extractor text) / (maximum (map (abs) [(extractor texto) | texto <- textos])))

-- EJERCICIO 7
extraerFeatures :: [Extractor] -> [Texto] -> Datos 
extraerFeatures extractores textos = [[ normalizarExtractor textos extractor texto | extractor <- extractores] | texto <- textos]

-- EJERCICIO 8
distEuclideana :: Medida
distEuclideana p q = sqrt (sum (binomiosCuadrado p q) )

binomiosCuadrado:: Instancia -> Instancia -> [Float]
binomiosCuadrado p q = map (**2) (zipWith (-) p q)

distCoseno :: Medida
distCoseno p q = (sumatoriaProductos p q) / (productoVectorial p q)

sumatoriaProductos :: Medida
sumatoriaProductos p q = sum (productos p q)

productos :: Instancia -> Instancia -> [Float]
productos = zipWith (*)

productoVectorial :: Medida
productoVectorial p q = sqrt ((sumatoriaProductos p p) * (sumatoriaProductos q q))

-- EJERCICIO 9
knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn = undefined

-- EJERCICIO 10
separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos datos etiquetas n p = (getTrainDatos (partirDatos n datos) p, getValDatos (partirDatos n datos) p, getTrainEtiquetas (partirEtiquetas n etiquetas) p, getValEtiquetas (partirEtiquetas n etiquetas) p)

partirDatos :: Int -> Datos -> [Datos]
partirDatos n = foldl (\z elem -> if (length (head z)) < n then [((head z) ++ [elem])] ++ (tail z) else (z ++ [[elem]])) [[]]

partirEtiquetas :: Int -> [Etiqueta] -> [[Etiqueta]]
partirEtiquetas n = foldl (\z elem -> if (length (head z)) < n then [((head z) ++ [elem])] ++ (tail z) else (z ++ [[elem]])) [[]]

getTrainDatos :: [Datos] -> Int -> Datos
getTrainDatos datos n = concat ((take (n-1) datos) ++ (drop n datos))

getTrainEtiquetas :: [[Etiqueta]] -> Int -> [Etiqueta]
getTrainEtiquetas etiquetas n = concat ((take (n-1) etiquetas) ++ (drop n etiquetas))

getValDatos :: [Datos] -> Int -> Datos
getValDatos datos n = concat (tail (take n datos))

getValEtiquetas :: [[Etiqueta]] -> Int -> [Etiqueta]
getValEtiquetas etiquetas n = concat (tail (take n etiquetas))

-- EJERCICIO 11
accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy e1 e2 = sumaIguales (zip e1 e2) / fromIntegral (length (zip e1 e2))

sumaIguales:: [(String, String)] -> Float
sumaIguales = foldr (\t rec -> if fst t == snd t then 1+rec else rec) 0

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation = undefined
