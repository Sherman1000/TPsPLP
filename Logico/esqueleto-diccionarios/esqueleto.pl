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
%diccionario_lista(+Lcode)

diccionario_lista(Lcode) :- diccionario(PalabraDelDicc), 
							string_codes(PalabraDelDicc, Lcode). 

%Ejercicio 2
%juntar_con(+S, +a , ?R)

juntar_con([L], _ , L).
juntar_con([Ls | Lss], J, R) :- juntar_con(Lss, J, Rrec), 
								append(Rpref, Rrec, R), 
								append(Ls, [J], Rpref),!.  

%Ejercicio 3
%palabras(?S, +P)

palabras(S, P) :- split_por_espacio(S, P).

%Ejercicio 4
asignar_var(A, MI, MF) :- getKeys(MI, Keys), 
						  not(member(A, Keys)), 
						  append(MI, [(A, X)], MF).
asignar_var(A, MI, MI) :- getKeys(MI, Keys), 
						  member(A, Keys).

getKeys([], []).
getKeys([(Key, Value) | Ts], Tmapped) :- getKeys(Ts, Trec), 
										 append([Key], Trec, Tmapped).

%Ejercicio 5
%palabras_con_variables(+L, ?V) 

palabras_con_variables(L, V) :- palabras_con_var_y_mapa(L, V, [], Mf).

palabras_con_var_y_mapa([], [], Mf, Mf).
palabras_con_var_y_mapa([Ls | Lss], [Vs | Vss], M, Mf1) :- pares_definidos_en_mapa(Ls, Vs, M, Mf0), 
														   palabras_con_var_y_mapa(Lss, Vss, Mf0, Mf1).

pares_definidos_en_mapa([], [], Mf, Mf).
pares_definidos_en_mapa([X | Ls], [V | Vs], Mi0, Mf) :- asignar_var(X, Mi0 , Mi1), 
														member((X,V), Mi1), 
														pares_definidos_en_mapa(Ls, Vs, Mi1, Mf).

%Ejercicio 6
quitar(_, [], R) :- R = [].
quitar(E, [L|Ls], R) :- E == L, quitar(E, Ls, R).
quitar(E, [L|Ls], R) :- E \== L, quitar(E, Ls, Rrec), 
						append([L], Rrec, R).

%Ejercicio 7
cant_distintos([], S) :- S = 0.
cant_distintos([L | Ls], S) :- quitar(L, Ls, Rquitado), 
							   cant_distintos(Rquitado, CuentaRec), 
							   S is (1 + CuentaRec).

%Ejercicio 8
descifrar(S, M) :- palabras(S, P), 	
				   palabras_con_variables(P, V), 
				   descifrarPalabras(V, Mvar), 
				   juntar_con(Mvar, 32, Palabrasseparadas), 
				   string_codes(M, Palabrasseparadas).

descifrarPalabras([], []).
descifrarPalabras([Ps | Pss], Mvar) :- diccionario_lista(PalabraDelDicc), 
									   descifrarPalabras(Pss, Mrec), 
									   palabra_valida(Ps, PalabraDelDicc, Mp), 
									   append([Mp], Mrec, Mvar).

palabra_valida([], [], []).
palabra_valida([Var | Vars], [P | Ps], M) :- length(Vars, Lv), 
											 length(Ps, Lp), 
											 Lv == Lp, 
											 palabra_valida(Vars, Ps, Mrec), 
											 P = Var, 
											 append([P], Mrec, M).

%Ejercicio 9
%descifrar_sin_espacios(+S, ?M)

descifrar_sin_espacios(S, M) :- con_espacios_intercalados(S, SwithSpaces), sacar_espacio_del_final(SwithSpaces, Sfinal),  descifrar(Sfinal, M).

sacar_espacio_del_final([], []).
sacar_espacio_del_final([espacio], []).
sacar_espacio_del_final([ X | Ls1 ], [Y | Ls2]) :- X = Y, sacar_espacio_del_final( Ls1, Ls2).

con_espacios_intercalados([], []).
con_espacios_intercalados([A], [A]).
con_espacios_intercalados(S, SwithSpaces) :- append(Spref, Ssuf, S), Spref \== [], append(Spref, [espacio], SwithSpacesPref), con_espacios_intercalados(Ssuf, SwithSpacesSuf), append(SwithSpacesPref, SwithSpacesSuf, SwithSpaces).


%Ejercicio 10
%mensajes_mas_parejos(S, M). FALTA ANALISIS A PARTIR DE descifrar_sin_espacios(S,M).
mensajes_mas_parejos(S, M) :- descifrar_sin_espacios(S, M), 
							  calcular_desvio(M, DesvioM), 
							  descifrar_sin_espacios(S, MComparador), 
							  calcular_desvio(MComparador, DesvioComp), 
							  DesvioM =< DesvioComp.
 
%calcular_desvio(+Mensaje, ?Desvio).
calcular_desvio(Mensaje, Desvio) :- split_por_espacio(Mensaje, MsjSeparadosPorEspacio), 
								    calcular_desvio_sobre_lista_de_palabras(MsjSeparadosPorEspacio, Desvio).

%split_por_espacio(?Sentencia, ?ListaDePalabras).
% En caso de que ListaDePalabras esté instanciado, Sentencia debe estar instanciado.

split_por_espacio([],[]).
split_por_espacio(Sentencia, ListaDePalabras) :- leer_hasta_espacio(Sentencia, Palabra),
										   borrar_primera_palabra(Palabra, Sentencia, SSinPalabra),
										   split_por_espacio(SSinPalabra, RecListaDePalabras),
										   append([Palabra], RecListaDePalabras, ListaDePalabras), !.

%leer_hasta_espacio(+Caracteres, ?PrimeraPalabra)									
leer_hasta_espacio(Caracteres, PrimeraPalabra) :- primera_palabra(Caracteres, [], PrimeraPalabra).

%primera_palabra(+Cs, ?Accum, ?PrimeraPalabra).
% En caso de instanciarse Accum, debe tener relacion con lo puesto en Cs para que el algoritmo tenga sentido. Hay casos para los que funciona tener ?Cs, pero en otros se cuelga. 
% Al igual que para Accum, esos casos deben tener sentido en el algoritmo. No se recomienda.

primera_palabra([],PrimeraPalabra, PrimeraPalabra).
primera_palabra([C|Cs],Accum, PrimeraPalabra) :- C\=32,
									  append(Accum, [C], AccumConCaracter),
									  primera_palabra(Cs,AccumConCaracter, PrimeraPalabra).
primera_palabra([C|_],FirstWord,FirstWord) :- C==32.

%borrar_primera_palabra(?Palabra, ?Sentencia, ?SentenciaSinPalabra).
%Siempre brinda una sola solución. 

borrar_primera_palabra(Palabra, Sentencia, SentenciaSinPalabra) :- append(Palabra, [32|SentenciaSinPalabra], Sentencia), !.
borrar_primera_palabra(Palabra, Sentencia, SentenciaSinPalabra) :- append(Palabra, SentenciaSinPalabra, Sentencia).

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
