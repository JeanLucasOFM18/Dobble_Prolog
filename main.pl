% LAB 2 PROLOG
% Nombre: Jean Lucas Rivera
% Sección: A1
% Profesor de Sección: Gonzalo Martinez

% IMPORTANTE
% Leer los ejemplos en la última sección, ya que, ahí se explica como funciona cada predicado

% --------------------------------------TDA CARDSSET--------------------------------------------------

% Dominio
% Elements : Lista
% NumC, maxC, Seed : Enteros

% Predicados
% cola([_|C],C) (aridad = 2)
% primerElemento([P],P) (aridad = 2)
% aleatorizar([P|C],R) (aridad = 2)
% seleccionaCartas(Lista, D3, CS) (aridad = 3)
% maxCartas(Lista, N, X, Aux, CS) (aridad = 5)
% firstCard(Lista, Num, ListaCa, CartaInicial) (aridad = 4)
% nextCards(Lista, Num, Aux, ListaCa, J, K, X, Carta, Mazo) (aridad = 9)
% lastCards(Lista, Num, Aux, ListaCa, J, K, I, X, Carta, Mazo) (aridad = 10)
% agregarMazo(Lista, Carta, Mazo) (aridad = 3)
% agregarInicio(X, L1, [X|L1]) (aridad = 3)
% agregarFinal([H|T], X, [H|L]) (aridad = 3)
% getElements(Lista, Entero, TC) (aridad = 3)
% getNumC(Lista, Entero, TC) (aridad = 3)
% getMaxC(Lista, Entero, TC) (aridad = 3)
% acortarListaElementos(D1, D2, L) (aridad = 3)
% obtenerElemento([_|Xs], Entero, TC) (aridad = 3)
% obtenerElementosNecesarios([_|Xs], Entero, TC) (aridad = 3)
% reverse([H|T],Z,Acc) (aridad = 3)
% seleccionCartas(SetCartas, N, N1, X, ContadorAux, Contador) (aridad = 6)
% comparaCartas(SetCartas, N, N1, X, C1, C2, ContadorAux, Contador) (aridad = 8)
% esValida(SetCartas, N, N1, X, 1, Contador) (aridad = 6)
% miembro(H,[_|T]) (aridad = 2)
% intersecta([H|T], L2, Count) (aridad = 3)
% comprueba(SetCartas, CantCartas, LargoMazo, CS) (aridad = 4)
% obtenerListaElementos(SetCartas, N, LargoMazo, Lista, CS) (aridad = 5)
% eliminaDuplicados([T|C], S, D) (aridad = 3)
% formarMazoMissingCards(Lista, Tamanio, CS) (aridad = 3)
% missingCards(MazoOriginal, MazoCreado, CantCartas, LargoMazo, Num, Num2, Num3, Aux, Lista, CS) (aridad = 10)
% comparaCartasMissing(MazoOriginal, MazoCreado, E1, E2, CantCartas, LargoMazo, Num, Num2, Num3, Aux, Lista, CS) (aridad = 12)
% compruebaMazo(MazoOriginal, MazoCreado, X, CantCartas, LargoMazo, Num, Num2, Num3, Aux, Lista, CS) (aridad = 11)
% stringCartas(CardsSet, N, X, StringAux, String) (aridad = 5)

% Metas primarias
% getElements, eliminaDuplicados, getNumC, getMaxC, aleatorizar, acortarListaElemento,
% firstCard, agregarMazo, nextCards, lastCards, seleccionaCartas

% Metas secundarias
% Todos los demás predicados

% -------------------------------------REPRESENTACION----------------------------------------------

% El TDA CardsSet representa el set de cartas del juego dobble, esto a través de una lista que
% contiene varias sublistas dentro, donde cada sublista es una carta del set

% --------------------------------------CONSTRUCTOR Y PERTENENCIA--------------------------------------------------

% Clausulas
% Hechos

% Dominio: una lista de elementos (list), cantidad de elementos por carta (int), cantidad máxima de cartas (int), un entero y el set de cartas (list)
% Descripción: Predicado encargado de crear el set de cartas con los datos dados por el usuario
cardsSet(Elements, NumC, MaxC, Seed, CS) :-
    getElements([Elements, NumC, MaxC, Seed], 0, D1),
    eliminaDuplicados(D1, R),
    getNumC([Elements, NumC, MaxC, Seed], 1, D2),
    getMaxC([Elements, NumC, MaxC, Seed], 2, D3),
    aleatorizar(R, R1),
    acortarListaElementos(R1, D2, LE),
    D4 is D2 - 1,
    firstCard(LE, D4, [], C1),
    agregarMazo([], C1, C2),
    D5 is D2 - 1,
    nextCards(LE, D2, 0, [], 1, 0, D5, [], C3),
    append(C2, C3, C4),
    lastCards(LE, D4, 0, [], 0, 0, 0, D4, [], C5),
    append(C4, C5, C6),
    seleccionaCartas(C6, D3, CS), !.

% --------------------------------------SELECTORES--------------------------------------------------

% Dominio: Un cardsSet (list), la posición del elemento escogido (entero), y una lista con los elementos (list)
% Descripción: Predicado que obtiene los elementos para formar las cartas
getElements(Lista, Entero, TC) :-
    obtenerElemento(Lista, Entero, TC).

% Dominio: Un cardsSet (list), la posición del elemento escogido (entero), y la cantidad de elementos por carta (entero)
% Descripción: Predicado que obtiene la cantidad de elementos por carta
getNumC(Lista, Entero, TC) :-
    obtenerElemento(Lista, Entero, TC).

% Dominio: Un cardsSet (list), la posición del elemento escogido (entero), y la cantidad máxima de cartas (entero)
% Descripción: Predicado que obtiene la cantidad máxima de cartas
getMaxC(Lista, Entero, TC) :-
    obtenerElemento(Lista, Entero, TC).

% --------------------------------------OTROS PREDICADOS--------------------------------------------------

% Dominio: Dos listas
% Descripción: Predicado que obtiene la cola de una lista
cola([_],[]).
cola([_|C],C).

% Dominio: Una lista de elementos y un elemento
% Descripción: Predicado que obtiene la cabeza de una lista
primerElemento([P],P).
primerElemento([P|_],P).

% Dominio: Dos listas
% Descripción: Predicado que intercala los elementos de una lista con el fin de aleatorizarla
aleatorizar([],[]).
aleatorizar([P],[P]).
aleatorizar([P|C],R):-
    primerElemento(C,H),
    cola(C,C2),
    aleatorizar(C2,R2),
    append([H],[P],R1),
    append(R1,R2,R).

% Dominio: Una lista de cartas, cantidad máxima de cartas (entero) y el set de cartas (lista)
% Descripción: Predicado que hace uso del predicado maxCartas para obtener la cantidad de cartas
% solicitadas por el usuario
seleccionaCartas(Lista, D3, CS) :-
    maxCartas(Lista, D3, 0, [], CS), !.

% Dominio: Una lista de cartas, cantidad máxima de cartas (entero), un contador (entero) y dos listas
% Descripción: Predicado que se encarga de agregar la cantidad solicitada de cartas dadas por el usuario
maxCartas(_, N, N, Aux, Aux).
maxCartas(Lista, N, X, Aux, CS) :-
    obtenerElemento(Lista, X, Carta),
    agregarFinal(Aux, Carta, Mazo),
    X1 is X + 1,
    maxCartas(Lista, N, X1, Mazo, CS), !.

% Dominio: Una lista de elementos, cantidad de elementos por carta (entero) y dos listas
% Descripción: Predicado encargado de formar la primera carta del set
firstCard(0, 0, ListaCa, ListaCa).
firstCard(Lista, 0, ListaCa, CartaInicial) :-
    obtenerElemento(Lista, 0, Elemento),
    agregarInicio(Elemento, ListaCa, XD),
    firstCard(0, 0, XD, CartaInicial).
firstCard(Lista, Num, ListaCa, CartaInicial) :-
    obtenerElemento(Lista, Num, Elemento),
    agregarInicio(Elemento, ListaCa, XD),
    Aux is Num - 1,
    firstCard(Lista, Aux, XD, CartaInicial), !.

% Dominio: Una lista de elementos, cantidad de elementos por carta (entero), 4 enteros y 3 listas
% Descripción: Predicado encargado de formar las siguiente "n" cartas del set
nextCards(_, Num, 0, _, Num, 0, _, Mazo, Mazo).
nextCards(Lista, Num, _, ListaCa, J, X, X, Carta, Mazo2) :-
    agregarFinal(Carta, ListaCa, Mazo),
    Jnueva is J + 1,
    nextCards(Lista, Num, 0, [], Jnueva, 0, X, Mazo, Mazo2), !.
nextCards(Lista, Num, 0, ListaCa, J, K, X, Carta, Mazo) :-
    obtenerElemento(Lista, 0, Elemento),
    agregarFinal(ListaCa, Elemento, XD),
    Aux is ((Num - 1) * J) + (K + 1),
    nextCards(Lista, Num, Aux, XD, J, K, X, Carta, Mazo), !.
nextCards(Lista, Num, Aux, ListaCa, J, K, X, Carta, Mazo) :-
    obtenerElemento(Lista, Aux, Elemento),
    agregarFinal(ListaCa, Elemento, XD),
    Aux2 is Aux + 1,
    Knueva is K + 1,
    nextCards(Lista, Num, Aux2, XD, J, Knueva, X, Carta, Mazo), !.

% Dominio: Una lista de elementos, cantidad de elementos por carta (entero), 5 enteros y 3 listas
% Descripción: Predicado encargado de formar las siguiente "n2" cartas del set
lastCards(_, Num, _, _, _, _, Num, _, Mazo, Mazo) :- !.
lastCards(Lista, Num, 0, [], Num, _, I, X, Carta, Mazo) :-
    Inueva is I + 1,
    lastCards(Lista, Num, 0, [], 0, 0, Inueva, X, Carta, Mazo), !.
lastCards(Lista, Num, _, ListaCa, J, X, I, X, Carta, Mazo2) :-
    agregarFinal(Carta, ListaCa, Mazo),
    Jnueva is J + 1,
    lastCards(Lista, Num, 0, [], Jnueva, 0, I, X, Mazo, Mazo2), !.
lastCards(Lista, Num, 0, ListaCa, J, K, I, X, Carta, Mazo) :-
    Inueva is I + 1,
    obtenerElemento(Lista, Inueva, Elemento),
    agregarFinal(ListaCa, Elemento, XD),
    Aux is (Num + 1 + Num * K + (I * K + J) mod Num),
    lastCards(Lista, Num, Aux, XD, J, K, I, X, Carta, Mazo), !.
lastCards(Lista, Num, Aux, ListaCa, J, K, I, X, Carta, Mazo) :-
    obtenerElemento(Lista, Aux, Elemento),
    agregarFinal(ListaCa, Elemento, XD),
    Knueva is K + 1,
    Aux2 is (Num + 1 + Num * Knueva + (I * Knueva + J) mod Num),
    lastCards(Lista, Num, Aux2, XD, J, Knueva, I, X, Carta, Mazo), !.

% Dominio: Una lista de cartas (list), una carta (list) y un set de cartas (list)
% Descripción: Predicado que hace uso del predicado agregarFinal para agregar la carta al set
agregarMazo(Lista, Carta, Mazo) :-
    agregarFinal(Lista, Carta, Mazo).

% Dominio: Una lista (list), elemento a agregar y la lista con el elemento agregado (list)
% Descripción: Predicado encargado de agregar al inicio de una lista un elemento determinado
agregarInicio(X, L1, [X|L1]).

% Dominio: Una lista (list), elemento a agregar y la lista con el elemento agregado (list)
% Descripción: Predicado encargado de agregar al final de una lista un elemento determinado
agregarFinal([], X, [X]).
agregarFinal([H|T], X, [H|L]) :- agregarFinal(T, X, L).

% Dominio: Una lista de elementos (list), elementos por carta (entero) y una lista con los elementos necesarios (list)
% Descripción: Predicado que se encarga de obtener los elementos necesarios para la creación de un set válido
acortarListaElementos(D1, D2, L) :-
    reverse(D1, R1, []),
    largo(R1, C1),
    calculo(D2, C2),
    C3 is (C1 - 1) - C2,
    obtenerElementoNecesarios(R1, C3, E1),
    reverse(E1, L, []).

% Dominio: Una lista (list), una posición (entero) y el elemento obtenido
% Descripción: Predicado que se encarga de obtener un elemento de una lista
obtenerElemento([Y|_], 0, Y).
obtenerElemento([_|Xs], Entero, TC):-
          N is Entero - 1,
          obtenerElemento(Xs, N, TC).

% Dominio: Una lista (list), una posición (entero) y el elemento obtenido
% Descripción: Misma utilidad que obtenerElemento, con la diferencia de que este predicado está modificado
obtenerElementoNecesarios([_|Y], 0, Y).
obtenerElementoNecesarios([_|Xs], Entero, TC):-
          N is Entero - 1,
          obtenerElementoNecesarios(Xs, N, TC).

% Dominio: Una lista (list), una lista invertida (list) y una lista vacía (list)
% Descripción: Predicado que se encarga de invertir una lista
reverse([],Z,Z).
reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).

% Dominio: Una lista con las cartas (list) y 5 enteros
% Descripción: Predicado que determina si el set de cartas es válido
seleccionCartas(_, _, _, 1, ContadorAux, ContadorAux).
seleccionCartas(SetCartas, N, X, X, ContadorAux, Contador) :-
    Nnueva is N + 1,
    N1nueva is Nnueva + 1,
    Xnueva is X - 1,
    seleccionCartas(SetCartas, Nnueva, N1nueva, Xnueva, ContadorAux, Contador), !.
seleccionCartas(SetCartas, N, N1, X, ContadorAux, Contador) :-
    obtenerElemento(SetCartas, N, C1),
    obtenerElemento(SetCartas, N1, C2),
    comparaCartas(SetCartas, N, N1, X, C1, C2, ContadorAux, Contador), !.

% Dominio: Una lista con las cartas (list), 5 enteros y las 2 cartas a comparar
% Descripción: Predicado encargado de comparar las 2 cartas seleccionadas
comparaCartas(SetCartas, N, N1, X, C1, C2, _, Contador) :-
    intersecta(C1, C2, ContadorAux2),
    esValida(SetCartas, N, N1, X, ContadorAux2, Contador), !.

% Dominio: Una lista con las cartas (list) y 5 enteros
% Descripción: Predicado que determina si entre las dos cartas existe sólo un elemento igual
esValida(SetCartas, N, N1, X, 1, Contador) :-
    N1nueva is N1 + 1,
    seleccionCartas(SetCartas, N, N1nueva, X, 1, Contador), !.
esValida(_, _, _, _, ContadorAux, Contador) :-
    seleccionCartas(_, _, _, 1, ContadorAux, Contador).

% Dominio: Un elemento y una lista (list)
% Descripción: Predicado que verifica si un elemento pertenece a una lista
miembro(H,[H|_]).                   
miembro(H,[_|T]):-
    miembro(H,T).

% Dominio: Dos elementos (list) y un contador (entero)
% Descripción: Predicado que se encarga de contabilizar las veces que se encuentra un elemento en una lista
intersecta([],_,0):- !.
intersecta([H|T], L2, Count):-
    miembro(H, L2),
    intersecta(T, L2, CountAux),
    Count is CountAux + 1, !.
intersecta([_|T], L2, Count):-
    intersecta(T, L2, Count).

% Dominio: Una lista de cartas (list), 2 enteros y una lista (list)
% Desripción: Predicado que obtiene los elementos que se usaron para formar el set dado
comprueba(_, CantCartas, CantCartas, []).
comprueba(SetCartas, _, LargoMazo, CS) :-
    obtenerListaElementos(SetCartas, 0, LargoMazo, [], CS).

% Dominio: Una lista de cartas (list), 2 enteros y 2 listas
% Descripción: Predicado que obtiene la lista de elementos que se usó para confeccionar el set dado
obtenerListaElementos(_, LargoMazo, LargoMazo, Lista, Lista).
obtenerListaElementos(SetCartas, N, LargoMazo, Lista, CS) :-
    obtenerElemento(SetCartas,  N, Elementos),
    append(Lista, Elementos, L1),
    Nnueva is N + 1,
    obtenerListaElementos(SetCartas, Nnueva, LargoMazo, L1, E1),
    eliminaDuplicados(E1, CS).

% Dominio: Dos listas
% Descripción: Predicado que elimina los duplicados presentes en una lista
eliminaDuplicados([], [], _).
eliminaDuplicados([T|C], S, D) :- 
    eliminaDuplicados(C,Z,[T|D]), ((miembro(T,D), S=Z,!) ; S=[T|Z]).
eliminaDuplicados(L, S) :- 
    eliminaDuplicados(L,S,[]).

% Dominio: Una lista de elementos (list), tamaño (entero) y un set de cartas (list)
% Descripción: Predicado que crea un set de cartas a partir de los elementos obtenidos
formarMazoMissingCards(Lista, Tamanio, CS) :-
    Tamanio2 is Tamanio - 1,
    firstCard(Lista, Tamanio2, [], C1),
    agregarMazo([], C1, C2),
    nextCards(Lista, Tamanio, 0, [], 1, 0, Tamanio2, [], C3),
    append(C2, C3, C4),
    lastCards(Lista, Tamanio2, 0, [], 0, 0, 0, Tamanio2, [], C5),
    append(C4, C5, CS).

% Dominio: 4 listas y 6 enteros
% Descripción: Predicado encargado de encontrar las cartas que no estén presentes en el set dado originalmente
missingCards(_, _, CantCartas, _, _, _, CantCartas, _, Lista, Lista).
missingCards(MazoOriginal, MazoCreado, CantCartas, LargoMazo, Num, LargoMazo, Num3, 0, Lista, CS) :-
    obtenerElemento(MazoCreado, Num, E1),
    agregarFinal(Lista, E1, Cartas),
    LargoMazoNuevo is Num3 + 1,
    Numnuevo is Num + 1,
    missingCards(MazoOriginal, MazoCreado, CantCartas, LargoMazo, Numnuevo, 0, LargoMazoNuevo, 0, Cartas, CS), !.
missingCards(MazoOriginal, MazoCreado, CantCartas, LargoMazo, Num, LargoMazo, Num3, 3, Lista, CS) :-
    Numnuevo is Num + 1,
    missingCards(MazoOriginal, MazoCreado, CantCartas, LargoMazo, Numnuevo, 0, Num3, 0, Lista, CS), !.
missingCards(MazoOriginal, MazoCreado, CantCartas, LargoMazo, Num, Num2, Num3, Aux, Lista, CS) :-
    obtenerElemento(MazoCreado, Num, E1),
    obtenerElemento(MazoOriginal, Num2, E2),
    comparaCartasMissing(MazoOriginal, MazoCreado, E1, E2, CantCartas, LargoMazo, Num, Num2, Num3, Aux, Lista, CS), !.

% Dominio: 4 listas, 2 cartas y 6 enteros
% Descripción: Predicado encargado de comparar dos cartas dadas, verifica si poseen más de 1 elemento igual
comparaCartasMissing(MazoOriginal, MazoCreado, E1, E2, CantCartas, LargoMazo, Num, Num2, Num3, Aux, Lista, CS) :-
    intersecta(E1, E2, Contador),
    obtenerElemento(MazoOriginal, 0, Carta),
    largo(Carta, N),
    ((N == Contador, X = 1) ; (N \== Contador, X = 0)),
    compruebaMazo(MazoOriginal, MazoCreado, X, CantCartas, LargoMazo, Num, Num2, Num3, Aux, Lista, CS), !.

% Dominio: 4 listas y 7 enteros
% Descripción: Predicado que pasa a comparar la siguiente carta
compruebaMazo(MazoOriginal, MazoCreado, 1, CantCartas, LargoMazo, Num, Num2, Num3, _, Lista, CS) :-
    Num2nuevo is Num2 + 1,
    missingCards(MazoOriginal, MazoCreado, CantCartas, LargoMazo, Num, Num2nuevo, Num3, 3, Lista, CS), !.
compruebaMazo(MazoOriginal, MazoCreado, 0, CantCartas, LargoMazo, Num, Num2, Num3, Aux, Lista, CS) :-
    Num2nuevo is Num2 + 1,
    missingCards(MazoOriginal, MazoCreado, CantCartas, LargoMazo, Num, Num2nuevo, Num3, Aux, Lista, CS), !.

% Dominio: Una lista de cartas (list), 2 enteros y 2 strings
% Descripción: Predicado que representa como string el set de cartas creado
stringCartas(_, N, N, StringAux, StringAux).
stringCartas(CardsSet, N, 0, _, String) :-
    obtenerElemento(CardsSet, 0, Carta),
    atomics_to_string(Carta, ",", CartaString),
    stringAtom(Numero, 1),
    string_concat(Numero, ": ", Aux),
    string_concat("Carta ", Aux, Aux2),
    string_concat(Aux2, CartaString, StringResultado),
    stringCartas(CardsSet, N, 1, StringResultado, String), !.
stringCartas(CardsSet, N, X, StringAux, String) :-
    obtenerElemento(CardsSet, X, Carta),
    atomics_to_string(Carta, ",", CartaString),
    X1 is X + 1,
    stringAtom(Numero, X1),
    string_concat(Numero, ": ", Aux),
    string_concat("Carta ", Aux, Aux2),
    string_concat(Aux2, CartaString, StringAct),
    string_concat(StringAux, " | ", StringAct2),
    string_concat(StringAct2, StringAct, StringResultado),
    stringCartas(CardsSet, N, X1, StringResultado, String), !.
    
% --------------------------------------TDA GAME--------------------------------------------------

% Dominio
% Numplayers y Seed: Integer
% CS y Game: List
% Mode: String

% Predicados
% getNumPlayers(Lista, TC) (aridad = 2)
% getCS(Lista, TC) (aridad = 2)
% getMode(Lista, TC) (aridad = 2)
% cantidadPuntajes(L8, N, L9) (aridad = 3)
% determinarFuncion(User, GameIn, GameOut, Entero) (aridad = 4)
% register(Game, GameIn, User, N, X, GameOut) (aridad = 6)
% compararUsuarios(User, Game, N, X) (aridad = 4)
% stringAtom(X, Y) (aridad = 2)
% insertaIdx(X, Pos, [C|R], [C|R2]) (aridad = 4)
% eliminarLista(Posicion, L, L1) (aridad = 3)
% largo([_|Xs], N) (aridad = 2)
% calculo(N, TC) (aridad = 2)
% defineFuncionTurn(Game, Username, Entero) (aridad = 3)
% insertar(El, [G|R], P, [G|Res]) (aridad = 3)
% ejecutarJuego(Game, Action, Entero, GameOut) (aridad = 4)
% esEmpateoVictoria(Ganadores, Entero, Ganadores) (aridad = 3)
% obtenerGanadores(Puntajes, Jugadores, PtsGanador, N, X, ListaGanadores, GameOut) (aridad = 7)
% ganadores(Puntajes, Jugadores, PtsGanador, N, X, ListaGanadores, 1, GameOut) (aridad = 8)
% listaMax(M, [X|Xs]) (aridad = 2)
% verificarElemento(Game, Elemento, GameOut) (aridad = 3)
% modificaPuntaje(Game, Entero, GameOut) (aridad = 3)
% verificaTurno(Game, Cant, Cant, GameOut) (aridad = 4)
% obtenerRepetido(C1, ER, GameOut) (aridad = 3)
% compruebaElemento(C1, C2, N, N1, 0, Elemento, ER) (aridad = 7)
% turnoCompletado(Game, N2, N2, GameOut) (aridad = 4)
% devolverCartas(Mesa, N, Mazo, CS) (aridad = 4)
% quedanCartas(Game, N, CS, GameOut) (aridad = 4)
% ubicarUsuario(Jugadores, UserAtom, N, X, Registro, Posicion) (aridad = 6)
% verificarRegistro(Jugadores, UserAtom, N, X, Registro, Score) (aridad = 6)
% mesaDeJuego(Game, Mesa) (aridad = 2)
% mostrarMesa(Cartas, Entero, Mesa) (aridad = 3)
% jugadoresString(Game, N, X, Lista, Jugadores) (aridad = 5)

% Metas primarias
% getNumPlayer, getCS, obtenerElemento, largo, calculo, getMode, stringAtom, aleatorizar
% cantidadPuntajes

% Metas secundarias
% Todos los demás predicados

% -------------------------------------REPRESENTACION----------------------------------------------

% El TDA Game se representa con 1 lista. Dentro de esta lista existen 6 sublistas inicialmente, las cuales son:
% Sublista 1 (posición 0) : Datos del juego, tales como cantidad de jugadores, el mazo y el modo de juego
% Sublista 2 (posición 1) : Jugadores registrados
% Sublista 3 (posición 2) : Turno Actual representado por la posición del jugador en la lista de registros
% Sublista 4 (posición 3) : Mesa de juego (aquí se muestra el par de cartas)
% Sublista 5 (posición 4) : Puntajes de los jugadores ordenados en la misma posición que sus registros
% Sublista 6 (posición 5) : Estado del juego
% Cuando se finaliza el juego se agrega una sublista
% Sublista 7 (posición 6) : Señala ganadores o se señala empate

% -------------------------------CONSTRUCTOR Y PERTENENCIA-----------------------------------------

% Clausulas
% Hechos

% Dominio: numero de jugadores (int), set de cartas (list), modo de juego (string), seed (int) y un game creado
% Descripción: Predicado que permite crear un nuevo juego con los datos dados por el usuario
dobbleGame(NumPlayers, CS, Mode, Seed, Game) :-
    getNumPlayers([NumPlayers, CS, Mode, Seed], D1),
    getCS([NumPlayers, CS, Mode, Seed], D2),
    obtenerElemento(D2, 0, Carta),
    largo(D2, N),
    largo(Carta, N1),
    calculo(N1, TC),
    N == TC,
    getMode([NumPlayers, CS, Mode, Seed], D3),
    stringAtom(D3, ModoAtom),
    ModoAtom == stackMode,
    aleatorizar(D2, R1),
    agregarFinal([], D1, L1),
    agregarFinal(L1, R1, L2),
    agregarFinal(L2, D3, L3),
    append([L3], [[]], L4),
    append(L4, [[0]], L5),
    append(L5, [[]], L6),
    append(L6, [[]], L7),
    append(L7, [["Juego no Iniciado"]], L8),
    cantidadPuntajes(L8, D1, Game), !.

% --------------------------------------SELECTORES--------------------------------------------------

% Dominio: Una lista (list) y el número de jugadores (entero)
% Descripción: Predicado que obtiene la cantidad de jugadores
getNumPlayers(Lista, TC) :-
    obtenerElemento(Lista, 0, TC).

% Dominio: Una lista (list) y la lista de cartas (list)
% Descripción: Predicado que obtiene el set de cartas
getCS(Lista, TC) :-
    obtenerElemento(Lista, 1, TC).

% Dominio: Una lista (list) y un string
% Descripción: Predicado que obtiene el modo de juego
getMode(Lista, TC) :-
    obtenerElemento(Lista, 2, TC).

% ---------------------------------------OTROS PREDICADOS------------------------------------------

% Dominio: 2 listas y 1 entero
% Descripción: Predicado encargado de introducer el sistema de puntajes al juego, iniciando cada
% puntaje en 0
cantidadPuntajes(L8, 0, L8).
cantidadPuntajes(L8, N, L9) :-
    obtenerElemento(L8, 4, Puntajes),
    agregarFinal(Puntajes, 0, P1),
    eliminarLista(4, L8, GameAct),
    insertar(P1, GameAct, 5, G1),
    N1 is N - 1,
    cantidadPuntajes(G1, N1, L9), !.

% Dominio: Un usuario (string), 2 TDA Game y un entero representativo
% Descripción: Predicado que determina lo que busca el usuario, con 2 opciones: la primera es
% que se registre un nuevo usuario y el segundo es que se muestre el game previo al registro
determinarFuncion(User, GameIn, GameOut, 0) :-
    obtenerElemento(GameIn, 0, D1),
    getNumPlayers(D1, N),
    obtenerElemento(GameIn, 1, D2),
    largo(D2, N1),
    N1 < N,
    register(D2, GameIn, User, N1, 0, GameOut), !.
determinarFuncion(Usuario, GameIn, GameOut, 1) :-
    obtenerElemento(GameOut, 1, D1),
    largo(D1, N),
    Neliminada is N - 1,
    obtenerElemento(D1, Neliminada, UltUsuario),
    stringAtom(UltUsuario, UserAtom),
    stringAtom(Usuario, UserAtom2),
    UserAtom == UserAtom2,
    eliminarLista(Neliminada, D1, D2),
    eliminarLista(1, GameOut, D3),
    insertar(D2, D3, 2, GameIn), !.

% Dominio: 3 TDA Game, un usuario (string) y 2 enteros
% Descripción: Predicado que se encarga de registrar a un usuario nuevo
register(Game, GameIn, User, 0, _, GameOut) :-
    agregarFinal(Game, User, D1),
    eliminarLista(1, GameIn, L1),
    insertar(D1, L1, 2, GameOut), !.
register(Game, GameIn, User, N, X, GameOut) :-
    compararUsuarios(User, Game, N, X),
    X == 0,
    agregarFinal(Game, User, D1),
    eliminarLista(1, GameIn, L1),
    insertar(D1, L1, 2, GameOut), !.

% Dominio: Un usuario (string), un Game y 2 enteros
% Descripción: Predicado que compara a 2 usuarios para verificar si ya está registrado
compararUsuarios(_, _, 0, 0).
compararUsuarios(User, Game, N, _) :-
    Naux is N - 1,
    obtenerElemento(Game, Naux, L1),
    stringAtom(L1, UserAtom),
    stringAtom(User, UserAtom2),
    UserAtom \== UserAtom2,
    Nnueva is N - 1,
    compararUsuarios(User, Game, Nnueva, 0), !. 

% Dominio: 2 elementos
% Descripción: Predicado que permite transformar un atom-symbol en string y viceversa
stringAtom(X, Y) :-
    atom_string(Y, X).

% Dominio: 1 elemento, posicion (entero) y 2 listas (list)
% Descripción: Predicado que ubica la posición y elimina el elemento de una lista
insertaIdx(X, 0, L1, [X|L1]).
insertaIdx(X, Pos, [C|R], [C|R2]) :-
    Pos1 is Pos - 1,
    insertaIdx(X, Pos1, R, R2).

% Dominio: Posición (entero) y 2 listas
% Descripción: Predicado que elimina un elemento de una lista que se encuentra en una posición dada
eliminarLista(Posicion, L, L1) :-
    insertaIdx(_, Posicion, L1, L).

% Dominio: Una lista (list) y 1 entero
% Descripción: Predicado que permite obtener la longitud de una lista
largo([], 0).
largo([_|Xs], N) :-
    largo(Xs, N1), N is N1 + 1.

% Dominio: 2 enteros
% Descripción: Predicado que permite calcular cuantas cartas son necesarias para un set válido
calculo(N, TC) :-
    TC is ((N - 1) ** 2) + (N - 1) + 1.

% Dominio: Una lista del Game (list), un usuario (string) y un entero representativo
% Descripción: Predicado que realiza 1 de las 2 posibilidades que tiene el predicado inicial
% la primera es verificar si es el turno de la persona de la cual se ingresó el nombre y la segunda
% obtiene el nombre de la persona a la que le corresponde el turno
defineFuncionTurn(Game, Username, 0) :-
    obtenerElemento(Game, 2, Turno),
    obtenerElemento(Turno, 0, Num),
    obtenerElemento(Game, 1, Jugadores),
    obtenerElemento(Jugadores, Num, User),
    stringAtom(User, UserAtom),
    stringAtom(Username, UserAtom2),
    UserAtom == UserAtom2, !.
defineFuncionTurn(Game, Username, 1) :-
    obtenerElemento(Game, 2, Turno),
    obtenerElemento(Turno, 0, Num),
    obtenerElemento(Game, 1, Jugadores),
    obtenerElemento(Jugadores, Num, Username), !.

% Dominio: Un elemento, una lista (list), posición (entero) y una lista con el elemento insertado
% Descripción: Predicado que inserta un elemento en una lista dada una posición
insertar(El, L, 1, [El | L]).
insertar(El, [G | R], P, [G | Res]):-
	P1 is P - 1,
	insertar(El, R, P1, Res).

% Dominio: TDA Game (list), Accion (string o list dependiendo de lo escogido), entero representativo y TDA Game actualizado (list)
% Descripción: Predicado que determina las acciones a seguir dependiendo de la acción escogida por el usuario
ejecutarJuego(Game, _, 0, GameOut) :-
    obtenerElemento(Game, 0, D1),
    obtenerElemento(D1, 1, CS),
    largo(CS, N),
    quedanCartas(Game, N, CS, GameOut), !.
ejecutarJuego(Game, Action, 1, GameOut) :-
    obtenerElemento(Action, 1, Usuario),
    obtenerElemento(Action, 2, Elemento),
    obtenerElemento(Game, 2, Dato),
    obtenerElemento(Dato, 0, Turno),
    obtenerElemento(Game, 1, UsuariosRegistrados),
    obtenerElemento(UsuariosRegistrados, Turno, UsuarioTurno),
    stringAtom(Usuario, UserAtom),
    stringAtom(UsuarioTurno, UserAtom2),
    UserAtom == UserAtom2,
    verificarElemento(Game, Elemento, GameOut), !.
ejecutarJuego(Game, _, 2, GameOut) :-
    obtenerElemento(Game, 2, Turno),
    obtenerElemento(Turno, 0, N),
    obtenerElemento(Game, 0, D),
    obtenerElemento(D, 0, N1),
    N2 is N1 - 1,
    turnoCompletado(Game, N, N2, GameOut), !.
ejecutarJuego(Game, _, 3, GameOut) :-
    obtenerElemento(Game, 4, Puntajes),
    obtenerElemento(Game, 1, Jugadores),
    largo(Puntajes, N),
    listaMax(PtsGanador, Puntajes),
    obtenerGanadores(Puntajes, Jugadores, PtsGanador, N, 0, [], Ganadores),
    largo(Ganadores, N1),
    esEmpateoVictoria(Ganadores, N1, Resultado),
    insertar([Resultado], Game, 7, GameOut), !.

% Dominio: Lista de ganadores (list), entero y una lista
% Descripción: Predicado que determina el resultado de la partida finalizada, en caso de existir
% más de 1 ganador, se considera que la partida terminó en empate
esEmpateoVictoria(Ganadores, 0, Ganadores).
esEmpateoVictoria(Ganadores, 1, Resultado) :-
    obtenerElemento(Ganadores, 0, Ganador),
    stringAtom(GanadorString, Ganador),
    string_concat("La victoria es de: ", GanadorString, R),
    esEmpateoVictoria(R, 0, Resultado).
esEmpateoVictoria(_, _, Resultado) :-
    string_concat("La partida terminó en", " empate", R),
    esEmpateoVictoria(R, 0, Resultado).

% Dominio: Lista de puntajes (list), lista de jugadores (list), la mayor cantidad de pts (entero)
% 2 enteros, 1 lista y un Game actualizado (list)
% Descripción: Predicado que obtiene los ganadores de una partida
obtenerGanadores(_, _, _, N, N, ListaGanadores, ListaGanadores).
obtenerGanadores(Puntajes, Jugadores, PtsGanador, N, X, ListaGanadores, GameOut) :-
    obtenerElemento(Puntajes, X, PtsUsuario),
    ((PtsUsuario == PtsGanador, Y = 1) ; (PtsUsuario \== PtsGanador, Y = 0)),
    ganadores(Puntajes, Jugadores, PtsGanador, N, X, ListaGanadores, Y, GameOut).

% Dominio: Lista de puntajes, jugadores (list), pts del ganador (entero), 2 enteros y 2 listas
% Descripción: Predicado que determina si un jugador tuvo el puntaje ganador, de ser así se agrega
% a la lista de ganadores
ganadores(Puntajes, Jugadores, PtsGanador, N, X, ListaGanadores, 0, GameOut) :-
    X1 is X + 1,
    obtenerGanadores(Puntajes, Jugadores, PtsGanador, N, X1, ListaGanadores, GameOut).
ganadores(Puntajes, Jugadores, PtsGanador, N, X, ListaGanadores, 1, GameOut) :-
    obtenerElemento(Jugadores, X, Jugador),
    agregarFinal(ListaGanadores, Jugador, ListaAct),
    X1 is X + 1,
    obtenerGanadores(Puntajes, Jugadores, PtsGanador, N, X1, ListaAct, GameOut).

% Dominio: Puntaje ganador (entero), lista de puntajes (list)
% Descripción: Predicado que obtiene el puntaje mayor dentro de la lista de puntajes
listaMax(M, [X|Xs]):-
          listaMax2(M, X, Xs).
listaMax2(M, M, []):- !.
listaMax2(X, Y, [Z|Zs]):-
          Z >= Y,
          !,
          listaMax2(X, Z, Zs).
listaMax2(X, Y, [Z|Zs]):-
          Z =< Y,
          listaMax2(X, Y, Zs).

% Dominio: TDA Game (list), elemento, y el TDA Game actualizado
% Descripción: Predicado que permite comprobar si el elemento que ingresó el usuario es el que
% se encuentra repetido en las 2 cartas mostradas en la mesa
verificarElemento(Game, Elemento, GameOut) :-
    obtenerElemento(Game, 3, Mesa),
    obtenerElemento(Mesa, 0, C1),
    obtenerElemento(Mesa, 1, C2),
    largo(C1, N),
    compruebaElemento(C1, C2, N, 0, 0, Elemento, ER),
    obtenerRepetido(C1, ER, Repetido),
    ((Elemento == Repetido, X = 0) ; (Elemento \== Repetido, X = 1)),
    modificaPuntaje(Game, X, GameOut).

% Dominio: TDA Game (list), entero representativo, TDA Game actualizado (list)
% Descripción: Predicado que determina si aumenta o no el puntaje del jugador, si el entero representativo
% es 1 no aumenta el puntaje, si es 0 aumenta su puntaje en 2 pts (porque obtiene las 2 cartas de la mesa)
modificaPuntaje(Game, 1, GameOut) :-
    obtenerElemento(Game, 3, Mesa),
    obtenerElemento(Game, 0, Datos),
    obtenerElemento(Datos, 1, Mazo),
    devolverCartas(Mesa, 0, Mazo, MazoNuevo),
    eliminarLista(1, Datos, Datos2),
    insertar(MazoNuevo, Datos2, 2, DatosNuevos),
    eliminarLista(0, Game, GameAct),
    insertar(DatosNuevos, GameAct, 1, GameAct2),
    eliminarLista(3, GameAct2, GameAct3),
    insertar([], GameAct3, 4, GameAct4),
    obtenerElemento(Datos, 0, CantJugadores),
    obtenerElemento(Game, 2, DatoTurnos),
    obtenerElemento(DatoTurnos, 0, Turno),
    Cant is CantJugadores - 1,
    verificaTurno(GameAct4, Cant, Turno, GameOut).
modificaPuntaje(Game, 0, GameOut) :-
    obtenerElemento(Game, 2, Dato),
    obtenerElemento(Dato, 0, Turno),
    obtenerElemento(Game, 4, Puntajes),
    obtenerElemento(Puntajes, Turno, PuntajeUsuario),
    PuntajeNuevo is PuntajeUsuario + 2,
    eliminarLista(Turno, Puntajes, Puntajes2),
    TurnoNuevo is Turno + 1,
    insertar(PuntajeNuevo, Puntajes2, TurnoNuevo, PuntajesAct),
    eliminarLista(4, Game, Game2),
    insertar(PuntajesAct, Game2, 5, GameAct),
    eliminarLista(3, GameAct, GameAct2),
    insertar([], GameAct2, 4, GameAct3),
    obtenerElemento(Game, 0, Datos),
    obtenerElemento(Datos, 0, CantJugadores),
    Cant is CantJugadores - 1,
    verificaTurno(GameAct3, Cant, Turno, GameOut).

% Dominio: TDA Game (list), 2 enteros y el TDA Game actualizado (list)
% Descripción: Predicado que indica el siguiente turno de la partida
verificaTurno(Game, Cant, Cant, GameOut) :-
    eliminarLista(2, Game, Game2),
    agregarFinal([], 0, TurnoAct),
    insertar(TurnoAct, Game2, 3, GameOut).
verificaTurno(Game, _, Turno, GameOut) :-
    obtenerElemento(Game, 2, Dato),
    obtenerElemento(Dato, 0, Turno),
    TurnoNuevo is Turno + 1,
    eliminarLista(2, Game, Game2),
    agregarFinal([], TurnoNuevo, TurnoAct),
    insertar(TurnoAct, Game2, 3, GameOut).
    
% Dominio: Una carta y 2 elementos
% Descripción: Predicado encargado de obtener el elemento repetido de las 2 cartas de la mesa
obtenerRepetido(_, 0, null).
obtenerRepetido(C1, ER, GameOut) :-
    ER1 is ER - 1,
    obtenerElemento(C1, ER1, GameOut).
    
% Dominio: 2 cartas, 3 enteros y 2 elementos
% Descripción: Predicado que determina cuál es el elemento repetido
compruebaElemento(_, _, _, N1, 1, _, N1).
compruebaElemento(_, _, N, N, _, _, 0).
compruebaElemento(C1, C2, N, N1, 0, Elemento, ER) :-
    obtenerElemento(C1, N1, E1),
    ((member(E1, C2), X = 1) ; (not(member(E1, C2)), X = 0)),
    Aux is N1 + 1,
    compruebaElemento(C1, C2, N, Aux, X, Elemento, ER).
    
% Dominio: TDA Game (list), 2 entero y el TDA Game actualizado (list)
% Descripción: Predicado que realiza el cambio de turno, y en caso de haber completado la pasada
% por todos los jugadores, resetea los turnos, es decir, le vuelve a tocar al primer jugador
turnoCompletado(Game, N2, N2, GameOut) :-
    obtenerElemento(Game, 3, Mesa),
    obtenerElemento(Game, 0, Datos),
    obtenerElemento(Datos, 1, Mazo),
    devolverCartas(Mesa, 0, Mazo, MazoNuevo),
    eliminarLista(1, Datos, Datos2),
    insertar(MazoNuevo, Datos2, 2, DatosNuevos),
    eliminarLista(0, Game, GameAct),
    insertar(DatosNuevos, GameAct, 1, GameAct2),
    agregarFinal([], 0, Turno),
    eliminarLista(2, GameAct2, TurnoCambiar),
    insertar(Turno, TurnoCambiar, 3, GameAct3),
    eliminarLista(3, GameAct3, GameAct4),
    insertar([], GameAct4, 4, GameOut), !.
turnoCompletado(Game, N, _, GameOut) :-
    obtenerElemento(Game, 3, Mesa),
    obtenerElemento(Game, 0, Datos),
    obtenerElemento(Datos, 1, Mazo),
    devolverCartas(Mesa, 0, Mazo, MazoNuevo),
    eliminarLista(1, Datos, Datos2),
    insertar(MazoNuevo, Datos2, 2, DatosNuevos),
    eliminarLista(0, Game, GameAct),
    insertar(DatosNuevos, GameAct, 1, GameAct2),
    N1 is N + 1,
    agregarFinal([], N1, Turno),
    eliminarLista(2, GameAct2, TurnoCambiar),
    insertar(Turno, TurnoCambiar, 3, GameAct3),
    eliminarLista(3, GameAct3, GameAct4),
    insertar([], GameAct4, 4, GameOut), !.

% Dominio: Cartas de la mesa (list), entero, 2 listas 
% Descripción: Predicado que permite devolver las cartas al mazo en caso de que el usuario
% se equivoque en indicar el elemento repetido
devolverCartas(_, 2, Mazo, Mazo).
devolverCartas(Mesa, N, Mazo, CS) :-
    obtenerElemento(Mesa, N, Carta),
    insertar(Carta, Mazo, 1, Mnuevo),
    Nnueva is N + 1,
    devolverCartas(Mesa, Nnueva, Mnuevo, CS).
    
% Dominio: TDA Game (list), entero, set de cartas, TDA Game actualizado
% Descripción: Predicado que verifica si quedan cartas en el mazo para seguir jugando, en caso de
% quedar cartas se ingresan a la mesa
quedanCartas(Game, 0, _, Game).
quedanCartas(Game, 1, _, Game).
quedanCartas(Game, N, CS, GameOut) :-
    N1 is N - 1,
    N2 is N - 2,
    obtenerElemento(CS, N1, C1),
    obtenerElemento(CS, N2, C2),
    obtenerElemento(Game, 3, Mesa),
    obtenerElemento(Game, 0, Game2),
    eliminarLista(N1, CS, CS2),
    eliminarLista(N2, CS2, CS3),
    eliminarLista(1, Game2, Game3),
    insertar(CS3, Game3, 2, Game4),
    eliminarLista(0, Game, GameAnt),
    insertar(Game4, GameAnt, 1, GameAct),
    agregarFinal(Mesa, C1, M2),
    agregarFinal(M2, C2, Cartas),
    eliminarLista(3, GameAct, G1),
    insertar(Cartas, G1, 4, GameOut).
    
% Dominio: Lista de jugadores (list), usuario (String) y 4 enteros
% Descripción: Predicado que obtiene la posición del usuario del cual se ingresó el nombre
ubicarUsuario(_, _, N, N, Registro, Registro).
ubicarUsuario(Jugadores, UserAtom, N, X, Registro, Posicion) :-
    obtenerElemento(Jugadores, X, Jugador),
    stringAtom(Jugador, UserAtom2),
    ((UserAtom == UserAtom2, ubicarUsuario(Jugadores, UserAtom, N, N, Registro, Posicion)) ;
    (UserAtom \== UserAtom2, R1 is Registro + 1, X1 is X + 1, ubicarUsuario(Jugadores, UserAtom, N, X1, R1, Posicion))).
    
% Dominio: Listad de jugadores, usuario (string) y 4 enteros
% Descripción: Predicado que obtiene la puntuación de un usuario y por ende se confirma su registro dentro
% de la lista de jugadores
verificarRegistro(_, _, N, N, Registro, Registro).
verificarRegistro(Jugadores, UserAtom, N, X, _, Score) :-
    obtenerElemento(Jugadores, X, Jugador),
    stringAtom(Jugador, UserAtom2),
    ((UserAtom == UserAtom2, verificarRegistro(Jugadores, UserAtom, N, N, 1, Score)) ;
    (UserAtom \== UserAtom2, X1 is X + 1, verificarRegistro(Jugadores, UserAtom, N, X1, 0, Score))).

% Dominio: TDA Game (list), string
% Descripción: Predicado que permite la representación en base string de la mesa de juego
mesaDeJuego(Game, Mesa) :-
    obtenerElemento(Game, 3, Cartas),
    largo(Cartas, N),
    ((N == 0, X = 0) ; (N == 2, X = 1)),
    mostrarMesa(Cartas, X, Mesa).

% Dominio: lista con cartas de la mesa (list), entero representativo, string
% Descripción: Predicado que muestra la mesa de juego con una representación de string
mostrarMesa(_, 0, Mesa) :-
    string_concat("Mesa de Juego", ": ", Mesa).
mostrarMesa(Cartas, 1, Mesa) :-
    obtenerElemento(Cartas, 0, C1),
    atomics_to_string(C1, ",", Carta1),
    string_concat("Mesa de Juego: ", Carta1, Aux),
    string_concat(Aux, " | ", Aux2),
    obtenerElemento(Cartas, 1, C2),
    atomics_to_string(C2, ",", Carta2),
    string_concat(Aux2, Carta2, Mesa).
    
% Dominio: TDA Game (list), 2 enteros y 2 string
% Descripción: Predicado que permite obtener la representación en base string de los jugadores
% de la partida
jugadoresString(_, N, N, Lista, Lista).
jugadoresString(Game, N, 0, _, Jugadores) :-
    obtenerElemento(Game, 1, ListaJugadores),
    obtenerElemento(ListaJugadores, 0, JugadorString),
    string_concat("Jugadores: ", JugadorString, Aux),
    jugadoresString(Game, N, 1, Aux, Jugadores), !.
jugadoresString(Game, N, X, Lista, Jugadores) :-
    obtenerElemento(Game, 1, ListaJugadores),
    obtenerElemento(ListaJugadores, X, JugadorString),
    string_concat(", ", JugadorString, Aux),
    string_concat(Lista, Aux, Aux2),
    X1 is X + 1,
    jugadoresString(Game, N, X1, Aux2, Jugadores), !.
    
% PREDICADOS OBLIGATORIOS

% Dominio
% Game, GameIn, GameOut : Games
% SetCartas, Card, CS, CardsSet : List
% Entero, TC, Score : Integer
% String, Action, Status : String

% Predicados 
% cardsSetlsDobble(SetCartas) (aridad = 1)
% cardsSetNthCard([_|Xs], Entero, Card) (aridad = 3)
% cardsSetFindTotalCards(Card, TC) (aridad = 2)
% cardsSetMissingCards(SetCartas, CS) (aridad = 2)
% cardsSetToString(CardsSet, String) (aridad = 2)
% dobbleGameRegister(User, GameIn, GameOut) (aridad = 3)
% dobbleGameWhoseTurnIsIt(Game, Username) (aridad = 2)
% dobbleGamePlay(Game, Action, GameOut) (aridad = 3)
% dobbleGameStatus(Game, Status) (aridad = 2)
% dobbleGameScore(Game, Username, Score) (aridad = 3)
% dobbleGameToString(Game, String) (aridad = 2)

% Metas primarias
% cardsSetlsDobble, cardsSetNthCard, cardsSetFindTotalCards, cardsSetMissingCards,
% cardsSetToString, dobbleGameRegister, dobbleGameWhoseTurnIsIt, dobbleGamePlay,
% dobbleGameStatus, dobbleGameScore, dobbleGameToString

% Metas Secundarias: Todos los predicados de los TDAs. 

% --------------------------------------PREDICADO CARDSSETISDOBBLE---------------------------------

% Clausulas
% Reglas

% Dominio: Un set de cartas (list)
% Descripción: Predicado que verifica si un set de cartas es válido o no, esto se realiza inicialmente
% obteniendo la cantidad de cartas y verificando si es una cantidad acorde para iniciar un juego. Luego se
% comprueba que entre un par de cartas cualquiera exista un solo elemento igual, ya que, de lo contrario el juego 
% no podría desarollarse de manera correcta. El predicado retorna true en caso de ser válido y false de no serlo
cardsSetIsDobble(SetCartas) :-
    obtenerElemento(SetCartas, 0, Carta),
    largo(Carta, Tamanio),
    calculo(Tamanio, CantCartas),
    largo(SetCartas, LargoMazo),
    CantCartas == LargoMazo,
    seleccionCartas(SetCartas, 0, 1, CantCartas, 0, Contador),
    Contador == 1, !.

% --------------------------------------PREDICADO CARDSSETNTHCARD---------------------------------

% Dominio: Un set de cartas (list), la posición (entero) y la carta obtenida (list)
% Descripción: Predicado que btiene la n-ésima carta desde el conjunto de cartas 
% partiendo desde 0 hasta (totalCartas-1). Esto se obtiene recorriendo el set de cartas y obteniendo la carta
% que se encuentre en la posición indicada por el usuario
cardsSetNthCard([Y|_], 0, Y).
cardsSetNthCard([_|Xs], Entero, Card):-
          N is Entero - 1,
          cardsSetNthCard(Xs, N, Card), !.

% --------------------------------------PREDICADO CARDSSETFINDTOTALCARDS---------------------------------

% Dominio: Una carta de muestra (list) y un entero (int)
% Descripción: Predicado que a partir de una carta de muestra, determina la cantidad total de cartas que se deben 
% crear para obtener un conjunto válido. Esto se obtine a partir de un cálculo en el que se obtiene la cantidad necesaria
% de cartas que debe tener un set válido
cardsSetFindTotalCards(Card, TC) :-
    largo(Card, N),
    calculo(N, TC), !.

% --------------------------------------PREDICADO CARDSSETMISSINGCARDS---------------------------------

% Dominio: Un set de cartas (list) y las cartas que faltan para completar el mazo (list)
% Descripción: Predicado que a partir de un conjunto de cartas retorna las cartas que hacen falta para 
% que el set sea válido. Primero se obtiene los elementos usados en el set de cartas que otorga el usuario.
% Una vez obtenido los elementos, se crea un set de cartas válidos. Luego se compara el set creado con el dado por el usuario.
% En esta comparación se verifica si la carta ya pertenece al mazo, en caso de no estar presente se agrega al set de cartas faltantes
cardsSetMissingCards(SetCartas, CS) :-
    obtenerElemento(SetCartas, 0, Carta),
    largo(Carta, Tamanio),
    calculo(Tamanio, CantCartas),
    largo(SetCartas, LargoMazo),
    comprueba(SetCartas, CantCartas, LargoMazo, L1),
    largo(L1, Tamanio2),
    CantCartas == Tamanio2,
    formarMazoMissingCards(L1, Tamanio, L2),
    missingCards(SetCartas, L2, CantCartas, LargoMazo, 0, 0, LargoMazo, 0, [], CS), !.

% --------------------------------------PREDICADO CARDSSETTOSTRING---------------------------------

% Dominio: Un set de cartas (list) y un string
% Descripción: Predicado que convierte un conjunto de cartas a una representación basada en strings que posteriormente pueda 
% visualizarse a través de la función write.
cardsSetToString(CardsSet, String) :-
    largo(CardsSet, N),
    stringCartas(CardsSet, N, 0, _, String), !.

% --------------------------------------PREDICADO DOBBLEGAMEREGISTER---------------------------------

% Dominio: Un usuario (string), un game y un game actualizado
% Descripción: Predicado que puede obtener 2 resultados. El primero es registrar a un jugador con su nombre,
% siempre y cuando no exista un jugador con ese mismo nombre ya registrado y que no supere la cantidad de jugadores establecidos en
% el game. La segunda es obtener el game previo al registro del usuario del cual se ingresó el nombre
dobbleGameRegister(User, GameIn, GameOut) :-
    ((is_list(GameIn), Y = 0); (is_list(GameOut), Y = 1)),
    determinarFuncion(User, GameIn, GameOut, Y), !.

% --------------------------------------PREDICADO DOBBLEGAMEWHOSETURNISIT---------------------------------

% Dominio: Un game y un usuario (string)
% Descripción: Predicado que determina a quién le toca jugar. Esto puede ser ingresando el game para comprobar
% a que jugador le pertenece el turno o ingresando un usuario y en caso de que le toque jugar el predicado retorna
% true, caso contrario retorna false
dobbleGameWhoseTurnIsIt(Game, Username) :-
    ((string(Username), X = 0) ; (is_list(Game), X = 1)),
    defineFuncionTurn(Game, Username, X), !.

% --------------------------------------PREDICADO DOBBLEGAMEPLAY---------------------------------

% Dominio: Un game, una accion (string o list según elección) y un game actualizado
% Descripción: Predicado que permite realizar una acción en el juego, pueden ser 4 opciones: La primera es
% "null" la cual toma las 2 cartas de la pila del mazo y las muestra en la mesa. La segunda es "[spotIt, usuario, elemento]",
% la cual se verifica si el usuario ingresado es al que le corresponde el turno, en caso de ser así se comprueba si el elemento
% ingresado es el repetido entre las 2 cartas mostradas en la mesa, si el elemento es el correcto se agrega 2 pts a su puntaje, ya
% que, son 2 las cartas que se lleva a su mazo personal, en caso contrario no se suma puntaje. Una vez finalizado este proceso
% se pasa el turno al siguiente jugador. La tercera es "pass", la cual realiza el pase de turno al siguiente jugador, con esto
% se devuelven las cartas ubicadas en la mesa al mazo para comenzar el siguiente turno. Finalmente la cuarta opción es
% "finish", la cual termina el juego y muestra si la partida tuvo un ganador o fue un empate
dobbleGamePlay(Game, Action, GameOut) :-
    obtenerElemento(Game, 1, Jugadores),
    obtenerElemento(Game, 0, Datos),
    obtenerElemento(Datos, 0, CantJugadores),
    largo(Jugadores, N),
    N == CantJugadores,
    ((is_list(Action), X = 1) ; (Action == null, X = 0) ; (Action == pass, X = 2) ; (Action == finish, X = 3)),
    ejecutarJuego(Game, Action, X, GameAct),
    ((X == 3, eliminarLista(5, GameAct, GameAct2), insertar(["Terminado"], GameAct2, 6, GameOut)) ;
    (X \== 3, eliminarLista(5, GameAct, GameAct2), insertar(["Jugando"], GameAct2, 6, GameOut))), !.

% --------------------------------------PREDICADO DOBBLEGAMESTATUS---------------------------------

% Dominio: Un game y estado del juego
% Descripción: Predicado que permite obtener el estado del juego que puede ser "Juego no Iniciado", "Jugando" o
% "Terminado"
dobbleGameStatus(Game, Status) :-
    obtenerElemento(Game, 5, Estado),
    obtenerElemento(Estado, 0, Status), !.

% --------------------------------------PREDICADO DOBBLEGAMESCORE---------------------------------

% Dominio: Un game, un usuario (string) y el puntaje del usuario (int)
% Descripción: Predicado que permite obtener el puntaje asociado al nombre de un usuario
dobbleGameScore(Game, Username, Score) :-
    obtenerElemento(Game, 4, Puntajes),
    obtenerElemento(Game, 1, Jugadores),
    largo(Jugadores, N),
    stringAtom(Username, UserAtom),
    verificarRegistro(Jugadores, UserAtom, N, 0, _, Resultado),
    Resultado == 1,
    ubicarUsuario(Jugadores, UserAtom, N, 0, 0, Posicion),
    obtenerElemento(Puntajes, Posicion, Score), !.

% --------------------------------------PREDICADO DOBBLEGAMETOSTRING---------------------------------

% Dominio: Un game y un string
% Descripción: Predicado que representa en base a string el juego ejecutado
dobbleGameToString(Game, String) :-
    obtenerElemento(Game, 1, Jugadores),
    largo(Jugadores, N),
    jugadoresString(Game, N, 0, _, J),
    string_concat(J, " \n ", Aux),
    mesaDeJuego(Game, Mesa),
    string_concat(Aux, Mesa, String), !.

% EJEMPLOS

% --------------------------------Ejemplos Predicado cardsSet------------------------------------------------

% EJ1: Se crea un set incompleto
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l], 3, 3, _, CS).
% EJ2: Se crea un set completo con 3 elementos por carta
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l], 3, 7, _, CS).
% EJ3: Se crea un set completo con 4 elementos por carta
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 4, 13, _, CS).
% EJ4: Se retorna false, ya que, el set no se puede generar a partir de la cantidad de elementos dados.
% En este predicado, se prioriza el generar un set completo y partir de ese set, dar la cantidad solicitada
% por el usuario
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 6, 3, _, CS).

% --------------------------------Ejemplos Predicado cardsSetIsDobble------------------------------------------------

% EJ1: Debe retornar false, ya que, es un set inválido, porque faltan cartas
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l], 3, 3, _, CS), cardsSetIsDobble(CS).
% EJ2: Debe retornar true, ya que, es un set válido
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l], 3, 7, _, CS), cardsSetIsDobble(CS).
% EJ3: Debe retornar true, ya que, es un set válido
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 4, 13, _, CS), cardsSetIsDobble(CS)
% EJ4: Se encuentran 2 elementos repetidos en el primer par de cartas (b y a), por ende retorna false
% cardsSetIsDobble([[b,a,d],[b,a,f],[b,e,h],[a,c,e],[a,f,h],[d,c,h],[d,f,e]]).

% --------------------------------Ejemplos Predicado cardsSetNthCard------------------------------------------------

% EJ1: Se obtiene la tercera carta del mazo (recordando que la primera se toma como la posición 0)
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), cardsSetNthCard(CS, 2, C2).
% EJ2: Se obtiene la sexta carta del mazo (recordando que la primera se toma como la posición 0)
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), cardsSetNthCard(CS, 5, C2).
% EJ3: Se retorna false debido a que la posición buscada no existe
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), cardsSetNthCard(CS, 10, C2).

% --------------------------------Ejemplos Predicado cardsSetFindTotalCards------------------------------------------------

% EJ1: Se obtiene la cantidad necesaria de cartas para tener un set válido (en este caso es 7)
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 3, _, CS), cardsSetNthCard(CS, 2, C2), cardsSetFindTotalCards(C2, TC).
% EJ2: Se obtiene la cantidad necesaria de cartas para tener un set válido (en este caso es 13)
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 4, 4, _, CS), cardsSetNthCard(CS, 2, C2), cardsSetFindTotalCards(C2, TC).
% EJ3: Se obtiene la cantidad necesaria de cartas para tener un set válido (en este caso es 21)
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 5, 5, _, CS), cardsSetNthCard(CS, 2, C2), cardsSetFindTotalCards(C2, TC).

% --------------------------------Ejemplos Predicado cardsSetMissingCards------------------------------------------------

% EJ1: Se obtiene una lista vacía, ya que, el set está completo
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), cardsSetMissingCards( CS, CS2).
% EJ2: Se obtiene las 3 cartas faltantes para que el set esté completo
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 4, 10, _, CS), cardsSetMissingCards(CS, CS2).
% EJ3: Se obtiene las 5 cartas faltantes para que el set esté completo
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 5, 16, _, CS), cardsSetMissingCards(CS, CS2).

% --------------------------------Ejemplos Predicado cardsSetToString------------------------------------------------

% IMPORTANTE, La representación es de la siguiente forma: Carta 1: a,b,c | Carta 2: a,e,f , etc.

% EJ1: Muestra el set completo de cartas
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), cardsSetToString(  CS, CS_STR), write(CS_STR).
% EJ2: Muetsra las 5 cartas del set
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 4, 5, _, CS), cardsSetToString(  CS, CS_STR), write(CS_STR).
% EJ3: Muestra las 8 cartas del set
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 4, 8, _, CS), cardsSetToString(  CS, CS_STR), write(CS_STR).

% --------------------------------Ejemplos Predicado dobbleGame------------------------------------------------

% IMPORTANTE, Se debe tener un set de cartas válido para poder jugar, además en el modo de juego siempre
% se debe señalar "stackMode", ya que, es el modo de juego elaborado.
% El game se compone inicialmente de 1 lista con 6 sublistas dentro de ella.
% La explicación de cada sublista es la siguiente:
% Sublista 1 (posición 0) : Datos del juego, tales como cantidad de jugadores, el mazo y el modo de juego
% Sublista 2 (posición 1) : Jugadores registrados
% Sublista 3 (posición 2) : Turno Actual representado por la posición del jugador en la lista de registros
% Sublista 4 (posición 3) : Mesa de juego (aquí se muestra el par de cartas)
% Sublista 5 (posición 4) : Puntajes de los jugadores ordenados en la misma posición que sus registros
% Sublista 6 (posición 5) : Estado del juego

% EJ1: Se obtiene un game de manera correcta
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G).
% EJ2: Retorna false, ya que, el modo de juego es "stack" y se debe colocar "stackMode"
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 4, 13, _, CS), dobbleGame(4, CS, "stack", _, G).
% EJ3: Se retorna false, ya que, el set de cartas está imcompleto
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 4, 10, _, CS), dobbleGame(4, CS, "stackMode", _, G).

% --------------------------------Ejemplos Predicado dobbleGameRegister------------------------------------------------

% EJ1: Se registra correctamente al "Jugador1"
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister( "Jugador1", G, G2).
% EJ2: Se retorna false, ya que, no se puede registrar a 2 jugadores con el mismo nombre
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador1", G, G2), dobbleGameRegister("Jugador1", G2, G3).
% EJ3: Se muestra el game previo al registro del "Jugador2"
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador1", G, G2), dobbleGameRegister("Jugador2", G2, G3), dobbleGameRegister("Jugador2", G_prev, G3).
% EJ4: Se retorna false, ya que, el "Jugador3" no existe en la lista de registros
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador1", G, G2), dobbleGameRegister("Jugador2", G2, G3), dobbleGameRegister("Jugador3", G_prev, G3).

% --------------------------------Ejemplos Predicado dobbleGameWhoseTurnIsIt------------------------------------------------

% EJ1: Se obtiene el nombre del jugador a quien le pertenece el turno "Jugador1"
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador1", G, G2), dobbleGameRegister("Jugador2", G2, G3), dobbleGameWhoseTurnIsIt(G3, U).
% EJ2: Se obtiene el nombre del jugador a quien le pertenece el turno "Jugador2"
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador2", G, G2), dobbleGameRegister("Jugador1", G2, G3), dobbleGameWhoseTurnIsIt(G3, U).
% EJ3: Se retorna false, ya que, al "Jugador1" no le corresponde el turno
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador2", G, G2), dobbleGameRegister("Jugador1", G2, G3), dobbleGameWhoseTurnIsIt(G3, "Jugador1").
% EJ4: Se retorna false, ya que, al "Jugador2" Le corresponde el turno
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador2", G, G2), dobbleGameRegister("Jugador1", G2, G3), dobbleGameWhoseTurnIsIt(G3, "Jugador2").

% --------------------------------Ejemplos Predicado dobbleGamePlay------------------------------------------------

% IMPORTANTE, siempre se debe inicial el juego con la accion null, si no se inicia con esta acción, se retornará false. Se debe indicar null luego de ejecutar pass o spotIt.
% Si se aplica la accion null y el mazo no tiene disponible 2 cartas o más, se obtiene el mismo game que el anterior y el usuario debe terminar el juego con la acción finish

% EJ1: Se retorna false, ya que, no se han registrado la cantidad de jugadores especificada
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador2", G, G2), dobbleGameRegister("Jugador1", G2, G3), dobbleGamePlay(G3, null, G4).
% EJ2: Se coge las 2 cartas del mazo y se muestran en la mesa de juego
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador1", G, G2), dobbleGameRegister("Jugador2", G2, G3), dobbleGameRegister("Jugador3", G3, G4), dobbleGameRegister("Jugador4", G4, G5), dobbleGamePlay(G5, null, G6).
% EJ3: Se pasa al siguiente turno y las cartas mostradas en la mesa vuelven al mazo
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador1", G, G2), dobbleGameRegister("Jugador2", G2, G3), dobbleGameRegister("Jugador3", G3, G4), dobbleGameRegister("Jugador4", G4, G5), dobbleGamePlay(G5, null, G6), dobbleGamePlay(G6, pass, G7).
% EJ4: Se señala la igualdad correcta y el turno del jugador es el correcto, por ende se suman 2 pts a su puntaje, se pasa de turno automáticamente y se eliminan las cartas del mazo
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador1", G, G2), dobbleGameRegister("Jugador2", G2, G3), dobbleGameRegister("Jugador3", G3, G4), dobbleGameRegister("Jugador4", G4, G5), dobbleGamePlay(G5, null, G6), dobbleGamePlay(G6, [spotIt, "Jugador1", f], G7).
% EJ5: Se señala la igualdad incorrecta y el turno del jugador es el correcto, por ende se mantiene su punataje, se pasa de turno automáticamente y se devuelven las cartas al mazo
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador1", G, G2), dobbleGameRegister("Jugador2", G2, G3), dobbleGameRegister("Jugador3", G3, G4), dobbleGameRegister("Jugador4", G4, G5), dobbleGamePlay(G5, null, G6), dobbleGamePlay(G6, [spotIt, "Jugador1", d], G7).
% EJ6: Se retorna false, ya que, el nombre ingresado no es el del turno actual
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador1", G, G2), dobbleGameRegister("Jugador2", G2, G3), dobbleGameRegister("Jugador3", G3, G4), dobbleGameRegister("Jugador4", G4, G5), dobbleGamePlay(G5, null, G6), dobbleGamePlay(G6, [spotIt, "Jugador2", f], G7).
% EJ7: Se termina el juego y se da a los ganadores en la sublista agregada al final
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador1", G, G2), dobbleGameRegister("Jugador2", G2, G3), dobbleGameRegister("Jugador3", G3, G4), dobbleGameRegister("Jugador4", G4, G5), dobbleGamePlay(G5, null, G6), dobbleGamePlay(G6, [spotIt, "Jugador1", f], G7), dobbleGamePlay(G7, finish, G8).
% EJ8: Se termina el juego y queda en empate
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador1", G, G2), dobbleGameRegister("Jugador2", G2, G3), dobbleGameRegister("Jugador3", G3, G4), dobbleGameRegister("Jugador4", G4, G5), dobbleGamePlay(G5, null, G6), dobbleGamePlay(G6, [spotIt, "Jugador1", f], G7), dobbleGamePlay(G7, null, G8), dobbleGamePlay(G8, [spotIt, "Jugador2", h], G9), dobbleGamePlay(G9, finish, G10).

% --------------------------------Ejemplos Predicado dobbleGameStatus------------------------------------------------

% EJ1: El juego aún no inicia
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador1", G, G2), dobbleGameRegister("Jugador2", G2, G3), dobbleGameRegister("Jugador3", G3, G4), dobbleGameRegister("Jugador4", G4, G5), dobbleGameStatus(G5, Status).
% EJ2: El juego esta en progreso
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador1", G, G2), dobbleGameRegister("Jugador2", G2, G3), dobbleGameRegister("Jugador3", G3, G4), dobbleGameRegister("Jugador4", G4, G5), dobbleGamePlay(G5, null, G6), dobbleGamePlay(G6, [spotIt, "Jugador1", f], G7), dobbleGamePlay(G7, null, G8), dobbleGamePlay(G8, [spotIt, "Jugador2", h], G9), dobbleGameStatus(G9, Status).
% EJ3: El juego terminó
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador1", G, G2), dobbleGameRegister("Jugador2", G2, G3), dobbleGameRegister("Jugador3", G3, G4), dobbleGameRegister("Jugador4", G4, G5), dobbleGamePlay(G5, null, G6), dobbleGamePlay(G6, [spotIt, "Jugador1", f], G7), dobbleGamePlay(G7, null, G8), dobbleGamePlay(G8, [spotIt, "Jugador2", h], G9), dobbleGamePlay(G9, finish, G10), dobbleGameStatus(G10, Status).

% --------------------------------Ejemplos Predicado dobbleGameScore------------------------------------------------

% EJ1: Se muestra el puntaje del "Jugador1"
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador1", G, G2), dobbleGameRegister("Jugador2", G2, G3), dobbleGameRegister("Jugador3", G3, G4), dobbleGameRegister("Jugador4", G4, G5), dobbleGamePlay(G5, null, G6), dobbleGamePlay(G6, [spotIt, "Jugador1", f], G7), dobbleGamePlay(G7, null, G8), dobbleGamePlay(G8, [spotIt, "Jugador2", h], G9), dobbleGameScore(G9, "Jugador1", Score).
% EJ2: Se muestra el puntaje del "Jugador2"
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador1", G, G2), dobbleGameRegister("Jugador2", G2, G3), dobbleGameRegister("Jugador3", G3, G4), dobbleGameRegister("Jugador4", G4, G5), dobbleGamePlay(G5, null, G6), dobbleGamePlay(G6, [spotIt, "Jugador1", f], G7), dobbleGamePlay(G7, null, G8), dobbleGamePlay(G8, [spotIt, "Jugador2", h], G9), dobbleGameScore(G9, "Jugador2", Score).
% EJ3: Se muestra el puntaje del "Jugador3"
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador1", G, G2), dobbleGameRegister("Jugador2", G2, G3), dobbleGameRegister("Jugador3", G3, G4), dobbleGameRegister("Jugador4", G4, G5), dobbleGamePlay(G5, null, G6), dobbleGamePlay(G6, [spotIt, "Jugador1", f], G7), dobbleGamePlay(G7, null, G8), dobbleGamePlay(G8, [spotIt, "Jugador2", h], G9), dobbleGameScore(G9, "Jugador3", Score).

% --------------------------------Ejemplos Predicado dobbleGameToString------------------------------------------------

% EJ1: Se muestra una mesa vacía, ya que, no se aplicó la accion null después de spotIt
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador1", G, G2), dobbleGameRegister("Jugador2", G2, G3), dobbleGameRegister("Jugador3", G3, G4), dobbleGameRegister("Jugador4", G4, G5), dobbleGamePlay(G5, null, G6), dobbleGamePlay(G6, [spotIt, "Jugador1", f], G7), dobbleGamePlay(G7, null, G8), dobbleGamePlay(G8, [spotIt, "Jugador2", h], G9), dobbleGameToString(G9, Str), display(Str).
% EJ2: Se muestra los jugadores y la mesa con las 2 cartas
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador1", G, G2), dobbleGameRegister("Jugador2", G2, G3), dobbleGameRegister("Jugador3", G3, G4), dobbleGameRegister("Jugador4", G4, G5), dobbleGamePlay(G5, null, G6), dobbleGamePlay(G6, [spotIt, "Jugador1", f], G7), dobbleGamePlay(G7, null, G8), dobbleGamePlay(G8, [spotIt, "Jugador2", h], G9), dobbleGamePlay(G9, null, G10), dobbleGameToString(G10, Str), display(Str).
% EJ3: Se muestra una lista vacía, ya que, no se puede obtener más cartas del mazo, por ende se debe terminar el juego con finish
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 3, 7, _, CS), dobbleGame(4, CS, "stackMode", _, G), dobbleGameRegister("Jugador1", G, G2), dobbleGameRegister("Jugador2", G2, G3), dobbleGameRegister("Jugador3", G3, G4), dobbleGameRegister("Jugador4", G4, G5), dobbleGamePlay(G5, null, G6), dobbleGamePlay(G6, [spotIt, "Jugador1", f], G7), dobbleGamePlay(G7, null, G8), dobbleGamePlay(G8, [spotIt, "Jugador2", h], G9), dobbleGamePlay(G9, null, G10), dobbleGamePlay(G10, [spotIt, "Jugador3", a], G11), dobbleGamePlay(G11, null, G12), dobbleGameToString(G12, Str), display(Str).

% EJEMPLO DE UN JUEGO REALIZADO
% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v], 4, 13, _, CS), dobbleGame(2, CS, "stackMode", _, G), dobbleGameRegister("Jean", G, G2), dobbleGameRegister("Antonella", G2, G3), dobbleGamePlay(G3, null, G4), dobbleGamePlay(G4, [spotIt, "Jean", c], G5), dobbleGamePlay(G5, null, G6), dobbleGamePlay(G6, [spotIt, "Antonella", e], G7), dobbleGamePlay(G7, null, G8), dobbleGamePlay(G8, pass, G9), dobbleGamePlay(G9, null, G10), dobbleGamePlay(G10, [spotIt, "Antonella", f], G11), dobbleGamePlay(G11, null, G12), dobbleGamePlay(G12, pass, G13), dobbleGamePlay(G13, null, G14), dobbleGamePlay(G14, finish, G15).

