module Tp where

import Data.List
import System.IO
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
split elementoSeparador xs = filter (\word -> length word > 0) (foldr f [[]] xs) where f = (\x xss -> if x == elementoSeparador then [] : xss else (x : head(xss)) : tail(xss))
-- EJERCICIO 2
longitudPromedioPalabras :: Extractor
longitudPromedioPalabras xs =  mean (map genericLength (listaDePalabras xs)) where listaDePalabras xs = split ' ' xs
-- EJERCICIO 3
cuentas :: Eq a => [a] -> [(Int, a)]
cuentas xs = zip cantidadDeRepeticiones (nub xs) where cantidadDeRepeticiones = [length (filter (== y) xs)| y <- nub xs]
-- EJERCICIO 4
repeticionesPromedio :: Extractor
repeticionesPromedio xs = mean ( map (\tupla -> fromIntegral (fst tupla)) (cuentas (listaDePalabras xs))) where listaDePalabras xs = split ' ' xs 

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

-- EJERCICIO 5
fstDeLaUnicaTuplaEnLista :: [(Int, a)] -> Int
fstDeLaUnicaTuplaEnLista [] = 0
fstDeLaUnicaTuplaEnLista [x] = fst x 

frecuenciaTokens :: [Extractor]
frecuenciaTokens =  map (\token -> (\texto -> let elemNQuantities = cuentas texto in (getTokenQuantityIn elemNQuantities token) / (sumAllQuantitiesIn elemNQuantities))) tokens 

sumAllQuantitiesIn :: [(Int, a)] -> Float
sumAllQuantitiesIn = (\elemNQuantities -> fromIntegral $ sum $ map (\elemAndQuantity -> fst elemAndQuantity) $ elemNQuantities)

getTokenQuantityIn :: Eq a => [(Int, a)] -> a -> Float
getTokenQuantityIn = (\elemNQuantities token -> fromIntegral $ fstDeLaUnicaTuplaEnLista $ filter (\elemAndQuantity -> (snd elemAndQuantity)==token) $ elemNQuantities)

-- EJERCICIO 6
normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor [] extractor = const 0
normalizarExtractor textos extractor = let maximoFeature = maximum (map abs [(extractor texto) | texto <- textos]) in (\text -> (extractor text) / maximoFeature)

-- EJERCICIO 7
extraerFeatures :: [Extractor] -> [Texto] -> Datos 
extraerFeatures extractores textos = let extractoresNorm = map (\extr -> normalizarExtractor textos extr) extractores in map (\text-> (map (\normExtr -> normExtr text) extractoresNorm)) textos
-- EJERCICIO 8
distEuclideana :: Medida
distEuclideana p q = sqrt (sum (binomiosCuadrado p q) ) where binomiosCuadrado p q = map (\x -> x*x) (zipWith (-) p q)

distCoseno :: Medida
distCoseno p q = (sumatoriaProductos p q) / (productoVectorial p q)

sumatoriaProductos :: Medida
sumatoriaProductos p q = sum (productos p q) where productos = zipWith (*)

productoVectorial :: Medida
productoVectorial p q = sqrt ((sumatoriaProductos p p) * (sumatoriaProductos q q))

-- EJERCICIO 9 ----------------
knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn n matrizDatos etiquetas fDistancia = (\instancia -> moda n etiquetas $ zip (getDistanciasAInstancia matrizDatos instancia fDistancia) etiquetas)

getDistanciasAInstancia :: Datos -> Instancia -> Medida -> [Float]
getDistanciasAInstancia = (\matrizDatos instancia fDistancia -> map (\dato -> fDistancia dato instancia) matrizDatos) 

moda :: Int  -> [Etiqueta] -> [(Float, Etiqueta)] -> Etiqueta
moda = (\n etiquetas distsConEtiqs  -> snd $ maximumBy compare $ getNMejoresEtiqs n distsConEtiqs etiquetas)

getNMejoresEtiqs :: Int -> [(Float, Etiqueta)] -> [Etiqueta]  -> [(Int, Etiqueta)]
getNMejoresEtiqs = (\n distanciasConEtiqs etiquetas -> contarAparicionesEtiq (nub etiquetas) $ take n $ sortBy compare distanciasConEtiqs)

contarAparicionesEtiq :: [Etiqueta] -> [(Float, Etiqueta)] -> [(Int, Etiqueta)]
contarAparicionesEtiq = (\etiquetasSinRepe nMasCercanos -> [(aparicionesEtiqueta, etiqueta) | etiqueta <- etiquetasSinRepe, let aparicionesEtiqueta = length(filter (matcheaEtiqueta etiqueta) nMasCercanos) ])

matcheaEtiqueta:: Etiqueta -> (Float, Etiqueta) -> Bool
matcheaEtiqueta etiqueta = (\label tupla -> label==(snd tupla)) etiqueta

-- EJERCICIO 10
separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos datos etiquetas n p = let datosParticionado = (sacarInvalidos (foldl (\z elem -> if (length (last z)) < (div (length datos) n) then (init z) ++ [(last z) ++ [elem]] else (z ++ [[elem]])) [[]] datos) n) in let etiquetasParticionado = (sacarInvalidos (foldl (\z elem -> if (length (last z)) < (div (length etiquetas) n) then (init z) ++ [(last z) ++ [elem]] else (z ++ [[elem]])) [[]] etiquetas) n) in (getTrain datosParticionado p, getVal datosParticionado p, getTrain etiquetasParticionado p, getVal etiquetasParticionado p)

sacarInvalidos :: [[a]] -> Int -> [[a]]
sacarInvalidos datos n = if (length (last datos)) < n then init datos else datos

getTrain:: [[a]] -> Int -> [a]
getTrain datos p = concat ((take (p-1) datos) ++ (drop p datos))

getVal::[a] -> Int -> a
getVal datos n = last (take n datos)

-- EJERCICIO 11
accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy e1 e2 = sumaIguales (zip e1 e2) / fromIntegral (length (zip e1 e2)) where sumaIguales = foldr (\t rec -> if fst t == snd t then 1+rec else rec) 0

-- EJERCICIO 12
nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n datos etiquetas = mean $ accuracyN $ applyKnnToPartitions [separarDatos datos etiquetas n p | p <- [1..n]]

applyKnnToPartitions :: [(Datos, Datos, [Etiqueta], [Etiqueta])] -> [([Etiqueta], [Etiqueta])]
applyKnnToPartitions = map (\(xTrain, xValid, yTrain, yValid) -> (applyKnnToAllValid xTrain yTrain xValid, yValid))  

applyKnnToAllValid :: Datos -> [Etiqueta] -> Datos -> [Etiqueta]
applyKnnToAllValid = (\xTrain yTrain xValid -> let trainedKnn = knn 15 xTrain yTrain distEuclideana in map (\validInstancia -> trainedKnn validInstancia) xValid )

accuracyN :: [([Etiqueta], [Etiqueta])] -> [Float]
accuracyN = map (\(etiquetasSupuestas, etiquetasReales) -> accuracy etiquetasSupuestas etiquetasReales) 