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
%juntar_con([L], _, L).
juntar_con([Ls | Lss], J, R) :- append(Rpref, Rrec, R), append(Ls, [J], Rpref), juntar_con(Lss, J, Rrec).  
juntar_con([L], _, R) :- R = L.

%Ejercicio 3
palabras(S, P) :- juntar_con(P, espacio, S).

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
%descifrar(S, M) :-

% listar mensajes secretos de ejemplo.
ej(1, [rombo, cuadrado, espacio, perro, cuadrado, sol, cuadrado]).
% solo debería ser "la cosa" porque cuadrado != triangulo
ej(2, [rombo, cuadrado, espacio, perro, triangulo, sol, cuadrado]).

ej(3, [rombo, cuadrado, perro, cuadrado, sol, luna, triangulo, estrella, arbol, gato]).
