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

-- Función agregada por código repetido y por declaratividad. Se sacó del código y se reemplazo por where debido a que ralentiza el tiempo de ejecución.
listaDePalabras:: [Char] -> [[Char]]
listaDePalabras xs = split ' ' xs

-- Esta función se realizó en inglés por consistencia con el uso de lo provisto por las líbrerias.
tupleComparator:: (Ord a, Ord b) => (a,b) -> (a,b) -> Ordering
tupleComparator (a1,b1) (a2,b2) |a1 < a2 = LT
								|a1 > a2 = GT
								|a1 == a2 = compare b1 b2

-- EJERCICIO 1
split :: Eq a => a -> [a] -> [[a]]
split elementoSeparador = foldr f [[]]
		where f =(\x xss -> if x == elementoSeparador
							then [] : xss 
							else (x : head(xss)) : tail(xss))
				
-- EJERCICIO 2
longitudPromedioPalabras :: Extractor
longitudPromedioPalabras xs =  mean (map (\x -> genericLength x) (listaDePalabras xs))
	where listaDePalabras xs = split ' ' xs

-- EJERCICIO 3
cuentas :: Eq a => [a] -> [(Int, a)]
cuentas xs = zip cantidadDeRepeticiones (nub xs)
		where cantidadDeRepeticiones = [length (filter ((\z -> \x -> x==z) y) xs)| y <- nub xs]

-- EJERCICIO 4
repeticionesPromedio :: Extractor
repeticionesPromedio xs = mean ( map (\tupla -> fromIntegral(fst tupla)) (cuentas (listaDePalabras xs)))
		where listaDePalabras xs = split ' ' xs 

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

-- EJERCICIO 5
-- Los fromIntegral transforman a Float los Int. Se debe tener en cuenta el mismo comentario que en el ejercicio 4.
frecuenciaTokens :: [Extractor]
frecuenciaTokens = [ extractorAPartirDe token | token <- tokens ]
	where extractorAPartirDe token xs = fromIntegral (obtenerValorEnTupla (filtrar token)) / fromIntegral( (sum (map (\tupla -> fst tupla) (cuentas xs))))
		where filtrar token = (filter ((\item tupla -> (snd tupla)==item) token) (cuentas xs))
						
obtenerValorEnTupla:: [(Int, a)] -> Int
obtenerValorEnTupla [] = 0
obtenerValorEnTupla [x] = fst x 

-- EJERCICIO 6
normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor [] extractor = const 0
normalizarExtractor textos extractor = (\text ->  (extractor text) / maximoFeature)
		where maximoFeature = maximum (map (abs) [(extractor texto) | texto <- textos])

-- EJERCICIO 7
extraerFeatures :: [Extractor] -> [Texto] -> Datos 
extraerFeatures extractores textos = [[ normalizarExtractor textos extractor texto | extractor <- extractores] | texto <- textos]

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
knn n matrizDatos etiquetas fDistancia =  (\instancia -> calcularModaAPartirDeNMejores n (crearRelacionDistanciaEtiqueta instancia etiquetas matrizDatos) 											etiquetas)
		where crearRelacionDistanciaEtiqueta instancia etiquetas matrizDatos = 
				zip (map ((\inst dato -> fDistancia dato inst) instancia) matrizDatos) etiquetas
-- crearRelacionDistanciaEtiqueta une en una tupla las etiquetas con la aplicación de la función fDistancia.

-- maximumBy es una función de Data.List. Toma un comparador (tupleComparator) y una lista y devuelve la lista ordenada en base al comparador.
-- En nuestro caso, fue utilizada para poder comparar por el valor númerico de la tupla. 										
calcularModaAPartirDeNMejores:: Int -> [(Float, Etiqueta)] -> [Etiqueta] -> Etiqueta
calcularModaAPartirDeNMejores n relacionDistanciaEtiqueta etiquetas = snd (maximumBy tupleComparator (contadorDeNMejoresEtiquetas n relacionDistanciaEtiqueta 																			etiquetas))
		where contadorDeNMejoresEtiquetas n relacionDistanciaEtiqueta etiquetas = 
			[((cantidadAparicionesEnMejoresN n relacionDistanciaEtiqueta etiqueta), etiqueta) | etiqueta <- (nub etiquetas)]
			where cantidadAparicionesEnMejoresN n relacionDistanciaEtiqueta etiqueta = 
				length(filter ((\label tupla -> label==(snd tupla)) etiqueta) (tomarNMaximos n relacionDistanciaEtiqueta))
				where tomarNMaximos n relacionDistanciaEtiqueta = if n == 0 then [] else [(minimumBy tupleComparator relacionDistanciaEtiqueta)]++(tomarNMaximos (n-1) ([item | item <- relacionDistanciaEtiqueta, not(item == (maximumBy tupleComparator relacionDistanciaEtiqueta))]))	
-- cantidadAparicionesEnMejoresN toma la cantidad de apariciones de la etiqueta en las N mejores tuplas (asegurado por relacionDistanciaEtiquetaOrdenada).

-- EJERCICIO 10
-- PartirInfoEnN divide la información en n conjuntos
-- SacarInvalidos toma la infomación dividida en n conjuntos y verifica que todos sean del mismo tamaño
separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos datos etiquetas n p = (getTrain (partirInfoEnN n datos) p, getVal (partirInfoEnN n datos) p, 
									getTrain (partirInfoEnN n etiquetas) p, getVal (partirInfoEnN n etiquetas) p)
	where partirInfoEnN n info = sacarInvalidos (foldl (\z elem -> if (length (last z)) < (div (length info) n) then (init z) ++ [(last z) ++ [elem]] else (z ++ [[elem]])) [[]] info) n
		where sacarInvalidos datos n = if (length (last datos)) < n then init datos else datos

-- Devuelve los datos o etiquetas que se utilizaran para entrenamiento
getTrain:: [[a]] -> Int -> [a]
getTrain datos p = concat ((take (p-1) datos) ++ (drop p datos))

-- Devuelve los datos o etiquetas que se utilizan para validacion 
getVal::[a] -> Int -> a
getVal datos n = last (take n datos)

-- EJERCICIO 11
accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy e1 e2 = sumaIguales (zip e1 e2) / fromIntegral (length (zip e1 e2))
	where sumaIguales = foldr (\t rec -> if fst t == snd t then 1+rec else rec) 0

-- EJERCICIO 12
-- separarDatosEnN separa los datos en conjuntos de tamaño n y varía el conjunto que toma para validación (este se elige de 1 a n)
nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n datos etiquetas = mean (accuracyN (separarDatosEnN datos etiquetas n n))
	where separarDatosEnN datos etiquetas n p = if p == 0 then [] else (separarDatos datos etiquetas n p) : (separarDatosEnN datos etiquetas n (p-1))

-- Esta función calcula el accuracy entre las etiquetas aprendidas y las de validación. Las aprendidas son calculadas con la función knn.
accuracyN :: [(Datos, Datos, [Etiqueta], [Etiqueta])] -> [Float]
accuracyN muestra = map (\tupla -> accuracy (fst tupla) (snd tupla)) (zip (knnN muestra) (map (\cuadrupla -> fourth4 cuadrupla) muestra))
	where knnN = foldr (\x rec -> (calculateKnn x) : rec) []
		where calculateKnn cuadrupla = foldr (\x rec -> ((knn 15 (first4 cuadrupla) (third4 cuadrupla) distEuclideana) x) : rec) [] (second4 cuadrupla)

first4 :: (Datos, Datos, [Etiqueta], [Etiqueta]) -> Datos
first4 (elem,_,_,_) = elem

second4 :: (Datos, Datos, [Etiqueta], [Etiqueta]) -> Datos
second4 (_,elem,_,_) = elem

third4 :: (Datos, Datos, [Etiqueta], [Etiqueta]) -> [Etiqueta]
third4 (_,_,elem,_) = elem

fourth4 :: (Datos, Datos, [Etiqueta], [Etiqueta]) -> [Etiqueta]
fourth4 (_,_,_,elem) = elem

