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

-- Función agregada por código repetido y por declaratividad. Se sacó del código y se reemplazo por where debido a que ralentiza el tiempo de ejecución.
listaDePalabras:: [Char] -> [[Char]]
listaDePalabras xs = split ' ' xs

-- EJERCICIO 1
split :: Eq a => a -> [a] -> [[a]]
split elementoSeparador xs= filter (\word -> length word > 0) (foldr f [[]] xs)
		where f =(\x xss -> if x == elementoSeparador
							then [] : xss 
							else (x : head(xss)) : tail(xss))

-- EJERCICIO 2
longitudPromedioPalabras :: Extractor
longitudPromedioPalabras xs =  mean (map genericLength (listaDePalabras xs))
	where listaDePalabras xs = split ' ' xs

-- EJERCICIO 3
cuentas :: Eq a => [a] -> [(Int, a)]
cuentas xs = zip cantidadDeRepeticiones (nub xs)
		where cantidadDeRepeticiones = [length (filter (== y) xs)| y <- nub xs]

-- EJERCICIO 4
repeticionesPromedio :: Extractor
repeticionesPromedio xs = mean ( map (\tupla -> fromIntegral (fst tupla)) (cuentas (listaDePalabras xs)))
		where listaDePalabras xs = split ' ' xs 

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

-- EJERCICIO 5
-- Los fromIntegral transforman a Float los Int. Se debe tener en cuenta el mismo comentario que en el ejercicio 4.
frecuenciaTokens :: [Extractor]
frecuenciaTokens = [ extractorAPartirDe token | token <- tokens ]
	where extractorAPartirDe token xs = fromIntegral (fstDeLaUnicaTuplaEnLista (filtrar token)) / fromIntegral( (sum (map (\tupla -> fst tupla) (cuentas xs))))
		where filtrar token = (filter ((\item tupla -> (snd tupla)==item) token) (cuentas xs))
						
fstDeLaUnicaTuplaEnLista :: [(Int, a)] -> Int
fstDeLaUnicaTuplaEnLista [] = 0
fstDeLaUnicaTuplaEnLista [x] = fst x 

-- EJERCICIO 6
normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor [] extractor = const 0
normalizarExtractor textos extractor = let maximoFeature = maximum (map abs [(extractor texto) | texto <- textos]) in (\text -> (extractor text) / maximoFeature)

-- EJERCICIO 7
extraerFeatures :: [Extractor] -> [Texto] -> Datos 
extraerFeatures extractores textos = let extractoresNorm = map (\extr -> normalizarExtractor textos extr) extractores in
									map (\text-> (map (\normExtr -> normExtr text) extractoresNorm)) textos
-- EJERCICIO 8
distEuclideana :: Medida
distEuclideana p q = sqrt (sum (binomiosCuadrado p q) )
	where binomiosCuadrado p q = map (\x -> x*x) (zipWith (-) p q)

distCoseno :: Medida
distCoseno p q = (sumatoriaProductos p q) / (productoVectorial p q)

sumatoriaProductos :: Medida
sumatoriaProductos p q = sum (productos p q)
	where productos = zipWith (*)

productoVectorial :: Medida
productoVectorial p q = sqrt ((sumatoriaProductos p p) * (sumatoriaProductos q q))


-- EJERCICIO 9
-- knn delega la obtención de la Etiqueta que devuelve el Modelo. Se encarga de devolver la función (Instancia -> Etiqueta)
knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn n matrizDatos etiquetas fDistancia = (\instancia -> let distEntreDatosEInstancia = zip (map (\dato -> fDistancia dato instancia) matrizDatos) etiquetas
										 in moda n distEntreDatosEInstancia etiquetas)
moda :: Int -> [(Float, Etiqueta)] -> [Etiqueta] -> Etiqueta
moda n relacionDistanciaEtiqueta etiquetas = snd (maximumBy compare (contadorDeNMejoresEtiquetas n relacionDistanciaEtiqueta etiquetas))

contadorDeNMejoresEtiquetas:: Int -> [(Float, Etiqueta)] -> [Etiqueta]  -> [(Int, Etiqueta)]
contadorDeNMejoresEtiquetas n relacionDistanciaEtiqueta etiquetas = let ordenadosXDistancia = (sortBy compare relacionDistanciaEtiqueta) in
																	let	nMasCercanos = (take n ordenadosXDistancia) 
															   		 	etiquetasSinRepetidos = (nub etiquetas)
		in [(aparicionesEtiqueta, etiqueta) | etiqueta <- etiquetasSinRepetidos, let aparicionesEtiqueta = length(filter (matcheaEtiqueta etiqueta) nMasCercanos) ]					
-- cantidadAparicionesEnMejoresN toma la cantidad de apariciones de la etiqueta en las N mejores tuplas (asegurado por relacionDistanciaEtiquetaOrdenada).

matcheaEtiqueta:: Etiqueta -> (Float, Etiqueta) -> Bool
matcheaEtiqueta etiqueta = (\label tupla -> label==(snd tupla)) etiqueta

-- EJERCICIO 10
separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])

separarDatos datos etiquetas n p = let datosParticionado = (sacarInvalidos (foldl (\z elem -> if (length (last z)) < (div (length datos) n) then (init z) ++ [(last z) ++ [elem]] else (z ++ [[elem]])) [[]] datos) n)
								    in let etiquetasParticionado = (sacarInvalidos (foldl (\z elem -> if (length (last z)) < (div (length etiquetas) n) then (init z) ++ [(last z) ++ [elem]] else (z ++ [[elem]])) [[]] etiquetas) n)
										in (getTrain datosParticionado p, getVal datosParticionado p, getTrain etiquetasParticionado p, getVal etiquetasParticionado p)

sacarInvalidos :: [[a]] -> Int -> [[a]]
sacarInvalidos datos n = if (length (last datos)) < n then init datos else datos

getTrain:: [[a]] -> Int -> [a]
getTrain datos p = concat ((take (p-1) datos) ++ (drop p datos))

getVal::[a] -> Int -> a
getVal datos n = last (take n datos)

-- EJERCICIO 11
accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy e1 e2 = sumaIguales (zip e1 e2) / fromIntegral (length (zip e1 e2))
	where sumaIguales = foldr (\t rec -> if fst t == snd t then 1+rec else rec) 0

-- EJERCICIO 12
nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n datos etiquetas = mean (accuracyN (separarDatosEnN datos etiquetas n n))
	where separarDatosEnN datos etiquetas n p = if p == 0 then [] else (separarDatos datos etiquetas n p) : (separarDatosEnN datos etiquetas n (p-1))

accuracyN :: [(Datos, Datos, [Etiqueta], [Etiqueta])] -> [Float]
accuracyN muestra = map (\tupla -> accuracy (fst tupla) (snd tupla)) (zip (knnN muestra) (map (\(a,b,c,d) -> d) muestra))
	where knnN = map (\x -> calculateKnn x) 
		where calculateKnn (w,x,y,z) = map (\a -> ((knn 15 w y distEuclideana) a)) x
