import Test.HUnit 

--Test split, Ejercicio 1--
splitPorEspacioPresente = split ' ' "Habia una vez."
splitPorComaPresente = split ',' "Habia, una, vez."
splitPorEspacioNoPresente = split ' ' "Habiaunavez."
splitPorComaNoPresente = split ',' "Habia una vez."
splitSoloElSeparador = split ',' ","
splitTresSeparadores = split ',' ",,,"

splitTest1 = TestCase (assertEqual "Por espacio, presente" ["Habia", "una", "vez."] splitPorEspacioPresente)
splitTest2 = TestCase (assertEqual "Por coma, presente" ["Habia", " una", " vez."] splitPorComaPresente)
splitTest3 = TestCase (assertEqual "Por espacio, no presente " ["Habiaunavez."] splitPorEspacioNoPresente)
splitTest4 = TestCase (assertEqual "Por coma, no presente" ["Habia una vez."] splitPorComaNoPresente)
splitTest5 = TestCase (assertEqual "Un separador" ["", ""] splitSoloElSeparador)
splitTest6 = TestCase (assertEqual "Tres separadores juntos" ["", "", "", ""] splitTresSeparadores)


--Test longitudPromedioPalabras, Ejercicio 2--
longitudPromedioSinTexto = longitudPromedioPalabras ""
longitudPromedioUnEspacio = longitudPromedioPalabras " "
longitudPromedioLetrasSueltas = longitudPromedioPalabras "a b c d e f g h i"
longitudPromedioDosLetrasXPalabra = longitudPromedioPalabras "aa bb cc dd ee ff gg hh ii"
longitudPromedioUnaPalabraLarga = longitudPromedioPalabras "aabbccddeeffgghhii"
longitudPromedioDiferentesTamaños2Palabras = longitudPromedioPalabras "aabbcc aabb"
longitudPromedioDiferentesTamaños3Palabras = longitudPromedioPalabras "aabbcc aabb ad"

longitudPromedioTest1 = TestCase (assertEqual "Sin texto" 0 longitudPromedioSinTexto)
longitudPromedioTest2 = TestCase (assertEqual "Un espacio" 0 longitudPromedioUnEspacio)
longitudPromedioTest3 = TestCase (assertEqual "Letras sueltas" 1 longitudPromedioLetrasSueltas)
longitudPromedioTest4 = TestCase (assertEqual "Dos letras por palabra" 2 longitudPromedioDosLetrasXPalabra)
longitudPromedioTest5 = TestCase (assertEqual "Una palabra larga" 18 longitudPromedioUnaPalabraLarga)
longitudPromedioTest6 = TestCase (assertEqual "Diferentes tamaños, 2 palabras" 5 longitudPromedioDiferentesTamaños2Palabras)
longitudPromedioTest7 = TestCase (assertEqual "Diferentes tamaños, 3 palabras" 4 longitudPromedioDiferentesTamaños3Palabras)

--Test cuentas, Ejercicio 3--
cuentasDelVacio = cuentas [""]
cuentasDelEspacio = cuentas [" "]
cuentasUnaPalabraUnaRepeticion = cuentas ["Una"]
cuentasUnaPalabraVariasRepeticiones = cuentas ["Una", "Una", "Una"]
cuentasMuchasPalabrasUnaRepeticion = cuentas  ["Una", "Dos", "Tres", "Cuatro"]
cuentasMuchasPalabrasMuchasRepeticiones = cuentas  ["Una", "Dos", "Tres", "Cuatro", "Una", "Dos", "Tres", "Cuatro", "Una", "Dos", "Tres", "Cuatro"]

cuentasTest1 = TestCase (assertEqual "Vacio" [(1,"")] cuentasDelVacio)
cuentasTest2 = TestCase (assertEqual "Espacio" [(1," ")] cuentasDelEspacio)
cuentasTest3 = TestCase (assertEqual "Una palabra una repeticion" [(1,"Una")] cuentasUnaPalabraUnaRepeticion)
cuentasTest4 = TestCase (assertEqual "Una palabra varias repeticiones" [(3,"Una")] cuentasUnaPalabraVariasRepeticiones)
cuentasTest5 = TestCase (assertEqual "Muchas palabras, una repeticion" [(1,"Una"),(1,"Dos"),(1,"Tres"),(1,"Cuatro")] cuentasMuchasPalabrasUnaRepeticion)
cuentasTest6 = TestCase (assertEqual "Muchas palabras, muchas repeticiones" [(3,"Una"),(3,"Dos"),(3,"Tres"),(3,"Cuatro")] cuentasMuchasPalabrasMuchasRepeticiones)

--Test repeticionesPromedio, Ejercicio 4--
repeticionesPromedioVacio = repeticionesPromedio ""
repeticionesPromedioEspacio = repeticionesPromedio " "
repeticionesPromedioUnaPalabra = repeticionesPromedio "Una"
repeticionesPromedioUnaPalabra3Veces = repeticionesPromedio "Una Una Una"
repeticionesPromedioMuchasPalabras1Vez = repeticionesPromedio  "Una Dos Tres Cuatro"
repeticionesPromedioMuchasPalabras3Veces = repeticionesPromedio  "Una Dos Tres Cuatro Una Dos Tres Cuatro Una Dos Tres Cuatro"

repeticionesPromedioTest1 = TestCase (assertEqual "Vacio" 1 repeticionesPromedioVacio)
repeticionesPromedioTest2 = TestCase (assertEqual "Espacio" 1 repeticionesPromedioEspacio)
repeticionesPromedioTest3 = TestCase (assertEqual "Una palabra una repeticion" 1 repeticionesPromedioUnaPalabra)
repeticionesPromedioTest4 = TestCase (assertEqual "Una palabra varias repeticiones" 3 repeticionesPromedioUnaPalabra3Veces)
repeticionesPromedioTest5 = TestCase (assertEqual "Muchas palabras, una repeticion" 1 repeticionesPromedioMuchasPalabras1Vez)
repeticionesPromedioTest6 = TestCase (assertEqual "Muchas palabras, muchas repeticiones" 3 repeticionesPromedioMuchasPalabras3Veces)

--Test frecuenciasTokens, Ejercicio 5--
frecuenciasTokensVacio = (head frecuenciaTokens) ""
frecuenciasTokensElToken = (head frecuenciaTokens) "_"
frecuenciasTokensUnaPalabraSinToken = (head frecuenciaTokens) "Una"
frecuenciasTokensUnaPalabraConToken = (head frecuenciaTokens) "Una_"
frecuenciasTokensMuchasPalabrasSinToken = (head frecuenciaTokens)  "Una Dos Tres Cuatro"
frecuenciasTokensPalabrasConToken = (head frecuenciaTokens) "Una_Dos_Tres"

frecuenciasTokensTest1 = TestCase (assertEqual "Vacio" 0 frecuenciasTokensVacio)
frecuenciasTokensTest2 = TestCase (assertEqual "Espacio" 0 frecuenciasTokensElToken)
frecuenciasTokensTest3 = TestCase (assertEqual "Una palabra sin Token" 0 frecuenciasTokensUnaPalabraSinToken)
frecuenciasTokensTest4 = TestCase (assertEqual "Una palabra con Token" 0.25 frecuenciasTokensUnaPalabraConToken)
frecuenciasTokensTest5 = TestCase (assertEqual "Muchas palabras, sin Token" 0 frecuenciasTokensMuchasPalabrasSinToken)
frecuenciasTokensTest6 = TestCase (assertEqual "Muchas palabras, con Token" 1/12 frecuenciasTokensPalabrasConToken)

--Test normalizadoExtractores y extraerFeatures, Ejercicio 6 y Ejercicio 7 respectivamente--

checkNormalizado :: [Float] -> Bool
checkNormalizado xs = ((head xs)) => 0 && ((head xs) <= 1) && ((head (head xs)) <= 1) &&  ((head (head xs)) >= 0)

estaTodoNormalizado :: [[Float]] -> Bool
estaTodoNormalizado xss = foldr (\parDeFeatures -> \rec -> (checkNormalizado parDeFeatures) && rec) True

aCheckearNormalizado = extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] ["b=a", "a = 2; a = 4", "asd", "1233243453", "assadasdasasd", "123 as"]
normalizarExtractorTest1 = TestCase (assertEqual "Esta normalizado" True (estaTodoNormalizado aCheckearNormalizado))

--Test frecuenciasTokens, Ejercicio 8--
distanciaEuclideanaCero = distEuclideana [0,0] [0,0]
distanciaEuclideana2 = distEuclideana [1,0] [0,1]
distanciaEuclideana4 = distEuclideana [1,1] [1,1]

distanciaCosenoCero = distEuclideana [0,0] [0,0]
distanciaCoseno2 = distEuclideana [1,0] [0,1]
distanciaCoseno4 = distEuclideana [1,1] [1,1]

distanciaEuclideanaTest1 = TestCase (assertEqual "Ceros" 0 distanciaEuclideanaCero)
distanciaEuclideanaTest2 = TestCase (assertEqual "1 0, 0 1" 1.414214 distanciaEuclideana2)
distanciaEuclideanaTest3 = TestCase (assertEqual "1 1, 1 1" 0 distanciaEuclideana4)

distanciaCosenoTest1 = TestCase (assertEqual "Ceros" 0 distanciaCosenoCero)
distanciaCosenoTest2 = TestCase (assertEqual "1 0, 0 1" 0 distanciaCoseno2)
distanciaCosenoTest3 = TestCase (assertEqual "1 1, 1 1" 1 distanciaCoseno4)

--Test knn, Ejercicio 9--
knnEnun = (knn 2 [[0,1],[0,2],[2,1],[1,1],[2,3]] ["i","i","f","f","i"] distEuclideana) [1,1]
knnTest = TestCase (assertEqual "Knn Enunciado" "f" knnEnun)


--Test separarDatos, Ejercicio 10--

xsTestEj10 = [[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7]] :: Datos
yTestEj10 = ["1","2","3","4","5","6","7"]
let (x_train, x_val, y_train, y_val) = separarDatos xs y 3 2

separarDatosTest = TestCase (assertEqual "SepararDatos" (x_train, y_train) ([[1.0,1.0],[2.0,2.0],[5.0,5.0],[6.0,6.0]],["1","2","5","6"]) )



--Test accuracy, Ejercicio 11--

accuracy0 = accuracy ["f", "f", "f", "f", "f"] ["f", "f", "f", "f", "f"]
accuracy60 = accuracy ["f", "f", "i", "i", "f"] ["i", "f", "i", "f", "f"]
accuracy100 = accuracy ["f", "f", "f", "f", "f"] ["f", "f", "f", "f", "f"]

accuracyTest1 = TestCase (assertEqual "0% accuracy" 0 accuracy0)
accuracyTest2 = TestCase (assertEqual "60% accuracy" 0.6 accuracy60)
accuracyTest3 = TestCase (assertEqual "100% accuracy" 1 accuracy100)

--Test separarDatos, Ejercicio 10--

let xs = [[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7]] :: Datos
let y = ["1","2","3","4","5","6","7"]
let (x_train, x_val, y_train, y_val) = separarDatos xs y 3 2

separarDatos = TestCase (assertEqual "separarDatos" 1 (x_train, x_val, y_train, y_val) ([[1.0,1.0],[2.0,2.0],[5.0,5.0],[6.0,6.0]],["1","2","5","6"]))


-------------------------------------------------------------------------------------------------------------------------------
testsSplit = TestList [TestLabel "splitTest1" splitTest1, TestLabel "splitTest2" splitTest2, TestLabel "splitTest3" splitTest3,
				  TestLabel "splitTest4" splitTest4, TestLabel "splitTest5" splitTest5, TestLabel "splitTest6" splitTest6]

testsLongitudPromedio = TestList [TestLabel "longitudPromedio1" longitudPromedioTest1, TestLabel "longitudPromedio2" longitudPromedioTest2, TestLabel "longitudPromedio3" longitudPromedioTest3,
				  TestLabel "longitudPromedio4" longitudPromedioTest4, TestLabel "longitudPromedio5" longitudPromedioTest5, TestLabel "longitudPromedio6" longitudPromedioTest6,
				   TestLabel "longitudPromedio6" longitudPromedioTest7]

testCuentas = TestList [TestLabel "cuentas1" cuentasTest1, TestLabel "cuentas2" cuentasTest2, TestLabel "cuentas3" cuentasTest3,
				  TestLabel "cuentas4" cuentasTest4, TestLabel "cuentas5" cuentasTest5, TestLabel "cuentas56" cuentasTest6]

testRepeticionesPromedio = TestList [TestLabel "repeticionesPromedioTest1" repeticionesPromedioTest1, TestLabel "repeticionesPromedioTest2" repeticionesPromedioTest2, TestLabel "repeticionesPromedioTest3" repeticionesPromedioTest3,
				  TestLabel "repeticionesPromedioTest4" repeticionesPromedioTest4, TestLabel "repeticionesPromedioTest5" repeticionesPromedioTest5, TestLabel "repeticionesPromedioTest6" repeticionesPromedioTest6]

testfrecuenciasTokensPromedio = TestList [TestLabel "frecuenciasTokensTest1" frecuenciasTokensTest1, TestLabel "frecuenciasTokensTest2" frecuenciasTokensTest2, TestLabel "frecuenciasTokensTest3" frecuenciasTokensTest3,
				  TestLabel "frecuenciasTokensTest4" frecuenciasTokensTest4, TestLabel "frecuenciasTokensTest5" frecuenciasTokensTest5, TestLabel "frecuenciasTokensTest6" frecuenciasTokensTest6]

testNormalizadoDeExtractores = TestList [TestLabel "normalizarExtractorTest1" normalizarExtractorTest1]

distancias = TestList [TestLabel "distanciaEuclideanaTest1" distanciaEuclideanaTest1, TestLabel "distanciaEuclideanaTest2" distanciaEuclideanaTest2, TestLabel "distanciaEuclideanaTest3" distanciaEuclideanaTest3,
				  TestLabel "distanciaCosenoTest1" distanciaCosenoTest1, TestLabel "distanciaCosenoTest2" distanciaCosenoTest2, TestLabel "distanciaCosenoTest3" distanciaCosenoTest3]
testKNN = TestList [TestLabel "knnTest" knnTest]

testAccuracy = TestList [TestLabel "accuracyTest1" accuracyTest1, TestLabel "accuracyTest1" accuracyTest2, TestLabel "accuracyTest3" accuracyTest3]

testSepararDatos = TestList [TestLabel "separarDatosTest" separarDatosTest]
