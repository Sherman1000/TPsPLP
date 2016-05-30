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
%L lista de listas, J elemento a intercalar, R result.
%juntar_con([L], _, L).

juntar_con([L], _, R) :- R = L.
juntar_con([Ls | Lss], J, R) :- append(Ls, [J], Rpref), juntar_con(Lss, J, Rrec), append(Rpref, Rrec, R).  

%Ejercicio 3
%palabras([], _).
%palabras([X | Ls], P) :- X == espacio, prefijo(P1, P), append(P1, [[]], P), palabras(Ls, P).
%palabras([X | Ls], P) :- X \== espacio, last(P, Plast), last(Plast, X), palabras(Ls, P).
%palabras([X | Ls], P) :- X \== espacio, last(P, Plast), append(Plast, [X], P1), prefijo(Plast, P1),  palabras(Ls, P).
%prefijo(Ls1, Ls2) :- append(Ls1, Lprima, Ls2).

palabras(S, P) :- juntar_con(P, espacio, S).

%Ejercicio 4
%Preguntar sobre la respuesta que hay que dar en el codigo? Que nos estamos perdiendo? el tercer caso del TP termina sin dar false.
asignar_var(A, MI, MF) :- getKeys(MI, Keys), not(member(A, Keys)), append(MI, [(A, X)], MF).
asignar_var(A, MI, MF) :- getKeys(MI, Keys), member(A, Keys), MF = MI.

getKeys([], []).
getKeys([(Key, Value) | Ts], Tmapped) :- getKeys(Ts, Trec), append([Key], Trec, Tmapped).

%Ejercicio 5
% No anda, preguntar xq carajo.
palabras_con_variables([[]], V) :- V = [[]].
palabras_con_variables([Ps:Pss], V) :- asignar_var_a_lista(Ps, [], PsVars), getValues(PsVars, PsValues), palabras_con_variables(Pss, Vrec), append(PsValues, Vrec, V).  

asignar_var_a_lista([], MI, MF) :- MF = MI.
asignar_var_a_lista([P | Ps], MI, MF) :- asignar_var(P, MI, MF1), asignar_var_a_lista(Ps, MF1, MFrec), append(MF1, MFrec, MF).

getValues([], []).
getValues([(Key, Value) | Ts], Tmapped) :- getValues(Ts, Trec), append([Value], Trec, Tmapped).

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
