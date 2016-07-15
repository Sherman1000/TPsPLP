:- dynamic(diccionario/1).

% Dado un nombre de archivo que contiene todas las palabras que se quieren
% agregar al diccionario (una por linea), vacia diccionario/1 y agrega
% las definiciones nuevas

cargar(NombreDeArchivo) :-
  retractall(diccionario(_)),
  atom_codes(NombreDeArchivo, Arch),
  open(Arch, read, Str),
  read_file(Str,_),
  close(Str).

read_file(Stream,[]) :- at_end_of_stream(Stream).
read_file(Stream,[X|L]) :-
    not(at_end_of_stream(Stream)),
    read_line_to_codes(Stream,Codes),
    string_codes(X, Codes),
    assertz(diccionario(X)),
    read_file(Stream,L), !.


%Ejercicio 1
%diccionario_lista(?Lcode): la reversibilidad depende de la reversibilidad de string_codes, la cual es string_codes(?String, ?Codes).

diccionario_lista(Lcode) :- diccionario(PalabraDelDicc), 
							string_codes(PalabraDelDicc, Lcode). 


%Ejercicio 2
%juntar_con(+S, ?J , ?R): Si no se instancia S, el predicado no funciona correctamente (aunque no se cuelga) por la forma en que se realizan los Appends. 
% J puede ser una variable que, si R está instanciado, terminará unificándose. Si no, appendeará a la variable J.

juntar_con([L], _ , L).
juntar_con([Ls | Lss], J, R) :- juntar_con(Lss, J, Rrec), 
								append(RPref, Rrec, R), 
								append(Ls, [J], RPref),!.  


%Ejercicio 3
%palabras(+S, ?P): Los motivos por los que S debe estar instanciado se muestran en el análisis de split_por_caracter.

palabras(S, P) :- split_por_caracter(S, espacio, P).


%Ejercicio 4

%asignar_var(?A, +MI, ?MF): Si MI no está instanciado, se cuelga cuando trata de encontrar otra solución más que MI = [] y MF = A, debido a que genera
% combinaciones con listas infinitas. A o MF deben estar instanciados. No pueden no estar instanciados al mismo tiempo.

%Este ejercicio funciona gracias a la capacidad de ProLog de generar variables frescas bajo demanda utilizando 
%variables anonimas generadas con la keyword "_", ademas, la manera de representar dichas variables frescas ayuda para que estas puedan ser manipulables dentro de los predicados.
%Si se nos diera como variable fresca una que ya hemos utilizado antes nuestro predicado asignar_var se volveria inconsistente.


asignar_var(A, MI, MF) :- not(member((A, _), MI)), append(MI, [(A, _)], MF).
asignar_var(A, MI, MI) :- member((A, _), MI).

%get_keys(?Tuples, ?TMapped): Si ambas no están instanciadas, genera infinitos resultados.

get_keys([], []).
get_keys([(Key, _) | Ts], TMapped) :- get_keys(Ts, Trec), 
									  append([Key], Trec, TMapped).

%Ejercicio 5
%palabras_con_variables(+L, ?V): Si L no estuviera instanciado, no se rompe pero genera infinitos resultados inservibles con combinaciones de listas vacias. Esto sucede debido
%a la utilización de member().

palabras_con_variables(L, V) :- palabras_con_var_y_mapa(L, V, [], _).


%palabras_con_var_y_mapa(+L, ?V, +Mi, -Mf)

palabras_con_var_y_mapa([], [], Mf, Mf).
palabras_con_var_y_mapa([Ls | Lss], [Vs | Vss], M, Mf1) :- pares_definidos_en_mapa(Ls, Vs, M, Mf0), 
														   palabras_con_var_y_mapa(Lss, Vss, Mf0, Mf1).


%pares_definidos_en_mapa(+L, ?V, +Mi, -Mf)

pares_definidos_en_mapa([], [], Mf, Mf).
pares_definidos_en_mapa([X | Ls], [V | Vs], Mi0, Mf) :- asignar_var(X, Mi0 , Mi1), 
														member((X,V), Mi1), 
														pares_definidos_en_mapa(Ls, Vs, Mi1, Mf).


%Ejercicio 6
%quitar(?E, +Ls, ?R): E puede no estar instanciado ya que el algoritmo también elimina variables en caso de estar. Ls deberá estar instanciada para evitar la generación de 
%listas infinitas. Si no esta instanciado, devuelve una solución trivial y luego se cuelga cuando se pide otra (debido al uso de append).

quitar(_, [], R) :- R = [].
quitar(E, [L|Ls], R) :- E == L, quitar(E, Ls, R).
quitar(E, [L|Ls], R) :- E \== L, quitar(E, Ls, Rrec), 
						append([L], Rrec, R).


%Ejercicio 7
%cant_distintos(+Ls, ?S): Ls deberá estar instanciado para evitar la generación de listas infinitas. Si no esta instanciado, devuelve una solución trivial y luego se cuelga 
%cuando se pide otra (debido al uso de quitar(), quien termina llamando a append()).

cant_distintos([], S) :- S = 0.
cant_distintos([L | Ls], S) :- quitar(L, Ls, RQuitado), 
							   cant_distintos(RQuitado, CuentaRec), 
							   S is (1 + CuentaRec).


%Ejercicio 8
descifrar(S, M) :- palabras(S, P),
				   cant_distintos(P, Pn), 	
				   palabras_con_variables(P, V), 
				   descifrarPalabras(V),
				   cant_distintos(V, Pn),
				   juntar_con(V, 32, PalabrasSeparadas), 
				   string_codes(M, PalabrasSeparadas).

descifrarPalabras([]).
descifrarPalabras([Vs | Vss]) :- cant_distintos(Vs, Pn),
								 diccionario_lista(Vs), 
								 cant_distintos(Vs, Pn),
								 descifrarPalabras(Vss).
											 
%Ejercicio 9
%descifrar_sin_espacios(+S, ?M), necesariamente S debe estar instanciado para generar las posibles intercalaciones con espacio resultantes, que luego deberan ser descifradas.
%Si no estuviera instanciada la S podria instanciarse en secuencias, potencialmente infinitas, que nunca daran una oracion valida considerando el diccionario cargado actual.

descifrar_sin_espacios(S, M) :- con_espacios_intercalados(S, SWithSpaces),  
								descifrar(SWithSpaces, M).


%con_espacios_intercalados(+S, ?R)

con_espacios_intercalados([], []).
con_espacios_intercalados(S, SWithSpaces) :- append(SPref, SSuf, S), 
											SPref \== [], 
											intercalar_o_no(SPref, SSuf, SPrefIntercalado), 
											con_espacios_intercalados(SSuf, SWithSpacesSuf), 
											append(SPrefIntercalado, SWithSpacesSuf, SWithSpaces).


%intercalar_o_no(+P, +S, ?R)

intercalar_o_no(Pref, [], Pref). 
intercalar_o_no(Pref, _, PrefNuevo) :- append(Pref, [espacio], PrefNuevo).


%Ejercicio 10
%mensajes_mas_parejos(+S, ?M): S debe estar instanciada en particular, por el uso de descifrar_sin_espacios(). 

mensajes_mas_parejos(S, M) :- descifrar_sin_espacios(S, M), 
							  string_codes(M, L),
  							  calcular_desvio(L, DesvioM), 
							  not(hay_uno_menor(DesvioM, S)).

 
%hay_uno_menor(+DesvioM, +S).

hay_uno_menor(DesvioM, S) :- descifrar_sin_espacios(S, MComparador),
							 string_codes(MComparador, LComparador),
							 calcular_desvio(LComparador, DesvioComp),
							 DesvioM > DesvioComp.


%calcular_desvio(+Mensaje, ?Desvio).

calcular_desvio(Mensaje, Desvio) :- split_por_caracter(Mensaje, 32, MsjSeparadosPorEspacio), 
								    calcular_desvio_sobre_lista_de_palabras(MsjSeparadosPorEspacio, Desvio).


%split_por_caracter(?Sentencia, +Caracter, ?ListaDePalabras).
% En caso de que ListaDePalabras esté instanciado, Sentencia debe estar instanciado.

split_por_caracter([], _, []).
split_por_caracter(Sentencia, Caracter, Ll) :- quitar(Caracter, Sentencia, SentenciaSinC), generarListas(SentenciaSinC, Ll), juntar_con(Ll, Caracter, Sentencia).

generarListas([], []).
generarListas(Sentencia, R) :- prefixNoVacio(P, Sentencia), sufix(P, Sentencia, SentenciaSinP), generarListas(SentenciaSinP, Rec), append([P], Rec, R).

prefixNoVacio(P, L) :- append(P, _, L), length(P, Long), Long > 0.

sufix(Prefijo, Sentencia, Sufijo) :- append(Prefijo, Sufijo, Sentencia).

%calcular_desvio_sobre_lista_de_palabras(+Palabras, ?Desvio).

calcular_desvio_sobre_lista_de_palabras(Palabras, Desvio) :- calcular_longitud_media(Palabras, LongMedia), 
															 binomios_cuadrados(Palabras, LongMedia, BCuadrados), 
															 sum_list(BCuadrados, Sumatoria), 
															 length(Palabras, LPalabras), Division is Sumatoria / LPalabras, Desvio is sqrt(Division).


%calcular_longitud_media(+P, ?LongMedia).

calcular_longitud_media(P, LongMedia) :- length_list(P, LengthList), 
										 average(LengthList, LongMedia).


%length_list(+Ls, ?LList).

length_list([L | Ls], LList) :- append([LLength], LListRec, LList), 
								length_list(Ls, LListRec),
								length(L, LLength),!. 
length_list([L], LList) :- length(L, LLength), LList = [LLength].


%average(+List, ?Average).

average(List, Average):- sum_list(List, Sum), 
						 length(List, Length), 
						 Length > 0, 
						 Average is (Sum / Length).


%binomios_cuadrados(+Ps, +LongMedia, -BCuadrados).
% LongMedia debe estar instanciado ya que Resta en binomio_cuadrado() se forma a partir de un "is" que debe tener ambos parametros instanciados.

binomios_cuadrados([P], LongMedia, [BCuadrado]) :- binomio_cuadrado(P, LongMedia, BCuadrado). 
binomios_cuadrados([P | Ps], LongMedia, BCuadrados) :- append([BCuadrado], RecBCuadrados, BCuadrados), 
													   binomios_cuadrados(Ps, LongMedia, RecBCuadrados), 
													   binomio_cuadrado(P, LongMedia, BCuadrado),!.


%binomio_cuadrado(?P, +LongMedia, -BCuadrado).  
% En caso de que P no esté instanciado, devuelve infinitas soluciones. Esto NECESITA que BCuadrado tampoco esté instanciado.

binomio_cuadrado(P, LongMedia, BCuadrado) :- length(P, LongP), 
											 Resta is (LongP-LongMedia), 
											 BCuadrado is (Resta^2).


% listar mensajes secretos de ejemplo.
ej(1, [rombo, cuadrado, espacio, perro, cuadrado, sol, cuadrado]).
% solo debería ser "la cosa" porque cuadrado != triangulo
ej(2, [rombo, cuadrado, espacio, perro, triangulo, sol, cuadrado]).

ej(3, [rombo, cuadrado, perro, cuadrado, sol, luna, triangulo, estrella, arbol, gato]).
