% LAB PROLOG

% TDA CARDSSET
cardsSet(Elements, NumC, MaxC, Seed, CS) :-
    getElements([Elements, NumC, MaxC, Seed], 0, D1),
    getNumC([Elements, NumC, MaxC, Seed], 1, D2),
    getMaxC([Elements, NumC, MaxC, Seed], 2, D3),
    acortarListaElementos(D1, D2, LE),
    D4 is D2 - 1,
    firstCard(LE, D4, [], C1),
    agregarMazo([], C1, C2),
    D5 is D2 - 1,
    nextCards(LE, D2, 0, [], 1, 0, D5, [], C3),
    append(C2, C3, C4),
    lastCards(LE, D4, 0, [], 0, 0, 0, D4, [], C5),
    append(C4, C5, CS).

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

agregarMazo(Lista, Carta, Mazo) :-
    agregarFinal(Lista, Carta, Mazo).

agregarInicio(X, L1, [X|L1]).

agregarFinal([], X, [X]).

agregarFinal([H|T], X, [H|L]) :- agregarFinal(T, X, L).

getElements(Lista, Entero, TC) :-
    obtenerElemento(Lista, Entero, TC).

getNumC(Lista, Entero, TC) :-
    obtenerElemento(Lista, Entero, TC).

getMaxC(Lista, Entero, TC) :-
    obtenerElemento(Lista, Entero, TC).

getSeed(Lista, Entero, TC) :-
    obtenerElemento(Lista, Entero, TC).

acortarListaElementos(D1, D2, L) :-
    reverse(D1, R1, []),
    largo(R1, C1),
    calculo(D2, C2),
    C3 is (C1 - 1) - C2,
    obtenerElementoNecesarios(R1, C3, E1),
    reverse(E1, L, []).

obtenerElemento([Y|_], 0, Y).

obtenerElemento([_|Xs], Entero, TC):-
          N is Entero - 1,
          obtenerElemento(Xs, N, TC).

obtenerElementoNecesarios([_|Y], 0, Y).

obtenerElementoNecesarios([_|Xs], Entero, TC):-
          N is Entero - 1,
          obtenerElementoNecesarios(Xs, N, TC).
                             
reverse([],Z,Z).

reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).

%FUNCIÓN 2 LISTA

cardsSetlsDobble(SetCartas) :-
    obtenerElemento(SetCartas, 0, Carta),
    largo(Carta, Tamanio),
    calculo(Tamanio, CantCartas),
    largo(SetCartas, LargoMazo),
    CantCartas == LargoMazo,
    seleccionCartas(SetCartas, 0, 1, CantCartas, 0, Contador),
    Contador == 1.

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

comparaCartas(SetCartas, N, N1, X, C1, C2, _, Contador) :-
    intersecta(C1, C2, ContadorAux2),
    esValida(SetCartas, N, N1, X, ContadorAux2, Contador), !.

esValida(SetCartas, N, N1, X, 1, Contador) :-
    N1nueva is N1 + 1,
    seleccionCartas(SetCartas, N, N1nueva, X, 1, Contador), !.

esValida(_, _, _, _, ContadorAux, Contador) :-
    seleccionCartas(_, _, _, 1, ContadorAux, Contador).
    
miembro(H,[H|_]).
                    
miembro(H,[_|T]):-
    miembro(H,T).

intersecta([],_,0):- !.

intersecta([H|T], L2, Count):-
    miembro(H, L2),
    intersecta(T, L2, CountAux),
    Count is CountAux + 1, !.

intersecta([_|T], L2, Count):-
    intersecta(T, L2, Count).

% FUNCIÓN 3 LISTA

cardsSetNthCard([Y|_], 0, Y).

cardsSetNthCard([_|Xs], Entero, Card):-
          N is Entero - 1,
          cardsSetNthCard(Xs, N, Card).

% FUNCIÓN 4 LISTA

cardsSetFindTotalCards(Card, TC) :-
    largo(Card, N),
    calculo(N, TC).

% FUNCIÓN 5 LISTA

cardsSetMissingCards(SetCartas, CS) :-
    obtenerElemento(SetCartas, 0, Carta),
    largo(Carta, Tamanio),
    calculo(Tamanio, CantCartas),
    largo(SetCartas, LargoMazo),
    comprueba(SetCartas, CantCartas, LargoMazo, L1),
    largo(L1, Tamanio2),
    CantCartas == Tamanio2,
    formarMazoMissingCards(L1, Tamanio, L2),
    missingCards(SetCartas, L2, CantCartas, LargoMazo, 0, 0, LargoMazo, 0, [], CS).

comprueba(_, CantCartas, CantCartas, []).

comprueba(SetCartas, _, LargoMazo, CS) :-
    obtenerListaElementos(SetCartas, 0, LargoMazo, [], CS).

obtenerListaElementos(_, LargoMazo, LargoMazo, Lista, Lista).

obtenerListaElementos(SetCartas, N, LargoMazo, Lista, CS) :-
    obtenerElemento(SetCartas,  N, Elementos),
    append(Lista, Elementos, L1),
    Nnueva is N + 1,
    obtenerListaElementos(SetCartas, Nnueva, LargoMazo, L1, E1),
    eliminaDuplicados(E1, CS).

eliminaDuplicados([], [], _).

eliminaDuplicados([T|C], S, D) :- 
    eliminaDuplicados(C,Z,[T|D]), ((miembro(T,D), S=Z,!) ; S=[T|Z]).

eliminaDuplicados(L, S) :- 
    eliminaDuplicados(L,S,[]).

formarMazoMissingCards(Lista, Tamanio, CS) :-
    Tamanio2 is Tamanio - 1,
    firstCard(Lista, Tamanio2, [], C1),
    agregarMazo([], C1, C2),
    nextCards(Lista, Tamanio, 0, [], 1, 0, Tamanio2, [], C3),
    append(C2, C3, C4),
    lastCards(Lista, Tamanio2, 0, [], 0, 0, 0, Tamanio2, [], C5),
    append(C4, C5, CS).

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

comparaCartasMissing(MazoOriginal, MazoCreado, E1, E2, CantCartas, LargoMazo, Num, Num2, Num3, Aux, Lista, CS) :-
    intersecta(E1, E2, Contador),
    compruebaMazo(MazoOriginal, MazoCreado, Contador, CantCartas, LargoMazo, Num, Num2, Num3, Aux, Lista, CS), !.

compruebaMazo(MazoOriginal, MazoCreado, 3, CantCartas, LargoMazo, Num, Num2, Num3, _, Lista, CS) :-
    Num2nuevo is Num2 + 1,
    missingCards(MazoOriginal, MazoCreado, CantCartas, LargoMazo, Num, Num2nuevo, Num3, 3, Lista, CS), !.

compruebaMazo(MazoOriginal, MazoCreado, _, CantCartas, LargoMazo, Num, Num2, Num3, Aux, Lista, CS) :-
    Num2nuevo is Num2 + 1,
    missingCards(MazoOriginal, MazoCreado, CantCartas, LargoMazo, Num, Num2nuevo, Num3, Aux, Lista, CS), !.

% FUNCIÓN 6 NO TERMINADA

cardsSetToString(Mazo, String) :-
    atomics_to_string(Mazo , "," , String).

% TDA GAME

dobbleGame(NumPlayers, CS, Mode, Seed, Game) :-
    getNumPlayers([NumPlayers, CS, Mode, Seed], D1),
    getCS([NumPlayers, CS, Mode, Seed], D2),
    getMode([NumPlayers, CS, Mode, Seed], D3),
    getSeed2([NumPlayers, CS, Mode, Seed], D4),
    agregarFinal([], D1, L1),
    agregarFinal(L1, D2, L2),
    agregarFinal(L2, D3, L3),
    agregarFinal(L3, D4, L4),
    append([L4], [[]], Game).
    
getNumPlayers(Lista, TC) :-
    obtenerElemento(Lista, 0, TC).

getCS(Lista, TC) :-
    obtenerElemento(Lista, 1, TC).

getMode(Lista, TC) :-
    obtenerElemento(Lista, 2, TC).

getSeed2(Lista, TC) :-
    obtenerElemento(Lista, 3, TC).

% FUNCIÓN 7 LISTA

dobbleGameRegister(User, GameIn, GameOut) :-
    obtenerElemento(GameIn, 0, D1),
    getNumPlayers(D1, N),
    obtenerElemento(GameIn, 1, D2),
    largo(D2, N1),
    N1 < N,
    register(D2, GameIn, User, N1, 0, GameOut).

register(Game, GameIn, User, 0, _, GameOut) :-
    agregarFinal(Game, User, D1),
    eliminarLista(1, GameIn, L1),
    agregarFinal(L1, D1, GameOut), !.

register(Game, GameIn, User, N, X, GameOut) :-
    compararUsuarios(User, Game, N, X),
    X == 0,
    agregarFinal(Game, User, D1),
    eliminarLista(1, GameIn, L1),
    agregarFinal(L1, D1, GameOut), !.

compararUsuarios(_, _, 0, 0).

compararUsuarios(User, Game, N, _) :-
    Naux is N - 1,
    obtenerElemento(Game, Naux, L1),
    stringAtom(L1, UserAtom),
    stringAtom(User, UserAtom2),
    UserAtom \== UserAtom2,
    Nnueva is N - 1,
    compararUsuarios(User, Game, Nnueva, 0), !. 
    
stringAtom(X, Y) :-
    atom_string(Y, X).

insertaIdx(X, 0, L1, [X|L1]).

insertaIdx(X, Pos, [C|R], [C|R2]) :-
    Pos1 is Pos - 1,
    insertaIdx(X, Pos1, R, R2).

eliminarLista(Posicion, L, L1) :-
    insertaIdx(_, Posicion, L1, L).

largo([], 0).

largo([_|Xs], N) :-
    largo(Xs, N1), N is N1 + 1.

calculo(N, TC) :-
    TC is ((N - 1) ** 2) + (N - 1) + 1.

% EJEMPLOS (PRONTO SE AGREGARÁN EJEMPLOS)

% cardsSet: cardsSet([a,b,c,d,e,f,g,h,i,j,k,l], 3, 3, 92175, CS)