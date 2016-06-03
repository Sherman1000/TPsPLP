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
diccionario_lista(Lcode) :- diccionario(PalabraDelDicc), string_codes(PalabraDelDicc, Lcode). 

%Ejercicio 2
juntar_con([L], _ , L).
juntar_con([Ls | Lss], J, R) :- juntar_con(Lss, J, Rrec), append(Rpref, Rrec, R), append(Ls, [J], Rpref),!.  

separar_con([Ls | Lss], J, R) :- append(Rpref, Rrec, R), juntar_con(Lss, J, Rrec), append(Ls, [J], Rpref),!. 
separar_con([L], _, L).

%Ejercicio 3
palabras(S, P) :- separar_con(P, espacio, S).

%Ejercicio 4
%Preguntar sobre la respuesta que hay que dar en el codigo? Que nos estamos perdiendo? el tercer caso del TP termina sin dar false.
asignar_var(A, MI, MF) :- getKeys(MI, Keys), not(member(A, Keys)), append(MI, [(A, X)], MF).
asignar_var(A, MI, MI) :- getKeys(MI, Keys), member(A, Keys).

getKeys([], []).
getKeys([(Key, Value) | Ts], Tmapped) :- getKeys(Ts, Trec), append([Key], Trec, Tmapped).

%Ejercicio 5
palabras_con_variables(L, V) :- palabras_con_var_y_mapa(L, V, [], Mf).

palabras_con_var_y_mapa([], [], Mf, Mf).
palabras_con_var_y_mapa([Ls | Lss], [Vs | Vss], M, Mf1) :- pares_definidos_en_mapa(Ls, Vs, M, Mf0), palabras_con_var_y_mapa(Lss, Vss, Mf0, Mf1).

pares_definidos_en_mapa([], [], Mf, Mf).
pares_definidos_en_mapa([X | Ls], [V | Vs], Mi0, Mf) :- asignar_var(X, Mi0 , Mi1), member((X,V), Mi1), pares_definidos_en_mapa(Ls, Vs, Mi1, Mf).

%Ejercicio 6
quitar(_, [], R) :- R = [].
quitar(E, [L|Ls], R) :- E == L, quitar(E, Ls, R).
quitar(E, [L|Ls], R) :- E \== L, quitar(E, Ls, Rrec), append([L], Rrec, R).

%Ejercicio 7
cant_distintos([], S) :- S = 0.
cant_distintos([L | Ls], S) :- quitar(L, Ls, Rquitado), cant_distintos(Rquitado, CuentaRec), S is (1 + CuentaRec).

%Ejercicio 8
descifrar(S, M) :- palabras(S, P), palabras_con_variables(P, V), descifrarPalabras(V, Mvar), juntar_con(Mvar, 32, Palabrasseparadas), string_codes(M, Palabrasseparadas).

descifrarPalabras([], []).
descifrarPalabras([Ps | Pss], Mvar) :- diccionario_lista(PalabraDelDicc), descifrarPalabras(Pss, Mrec), palabra_valida(Ps, PalabraDelDicc, Mp), append([Mp], Mrec, Mvar).

palabra_valida([], [], []).
palabra_valida([Var | Vars], [P | Ps], M) :- length(Vars, Lv), length(Ps, Lp), Lv == Lp, palabra_valida(Vars, Ps, Mrec), P = Var, append([P], Mrec, M).

%Ejercicio 10
%mensajes_mas_parejos(S, M) :- descifrar_sin_espacios(S, Msjs), menor_desvio_standard_por_palabra(Msjs, M).

%menor_desvio_standard_por_palabra(Msjs, M) :- calcular_desvios(Msjs, Desvios), menor_desvio(Desvios, MinDesvio), tiene_desvio(Msjs, MinDesvio, M).

%calcular_desvios([], Desvios) :- Desvios = [].
%calcular_desvios([Msj | Msjs], Desvios) :- calcular_desvio(Msj, Desvio), calcular_desvios(Msjs, Drec), append(Desvio, Drec, Desvios).  

%FALTA CIFRAR DE STRING A CUADRADO ROMBO Y QUE SE YO. SI NO NO PUEDO USAR PALABRAS! O SE PUEDE USAR EL DESCIFRAR INVERSO???
%calcular_desvio(Msj | Desvio) :- palabras(Msj, P), calcular_longitud_media(P, LongMedia), binomios_cuadrados(P, LongMedia, BCuadrado), sum_list(BCuadrado, Sumatoria), Desvio = sqr(Sumatoria)/length(P).

%calcular_longitud_media(P, LongMedia) :- 

%binomios_cuadrados([], BCuadrados) :- BCuadrados = []. 
%binomios_cuadrados([P | Ps], LongMedia, BCuadrados) :- binomio_cuadrado(P, LongMedia, BCuadrado), binomios_cuadrados(Ps, LongMedia, RecBCuadrados), append(BCuadrado, RecBCuadrados, BCuadrados).

%binomio_cuadrado(P, LongMedia, BCuadrado) :- Resta = length(P) - LongMedia, BCuadrado = Resta^2.

% listar mensajes secretos de ejemplo.
ej(1, [rombo, cuadrado, espacio, perro, cuadrado, sol, cuadrado]).
% solo debería ser "la cosa" porque cuadrado != triangulo
ej(2, [rombo, cuadrado, espacio, perro, triangulo, sol, cuadrado]).

ej(3, [rombo, cuadrado, perro, cuadrado, sol, luna, triangulo, estrella, arbol, gato]).
