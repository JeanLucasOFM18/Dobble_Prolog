% LAB PROLOG

cardsSet(Elements, NumC, MaxC, Seed, CS) :-
    getElements([Elements, NumC, MaxC, Seed], 0, D1),
    getNumC([Elements, NumC, MaxC, Seed], 1, D2),
    getMaxC([Elements, NumC, MaxC, Seed], 1, D3),
    acortarListaElementos(D1, D2, LE),
    D4 is D2 - 1,
    firstCard(LE, D4, [], C1),
    agregarMazo([], C1, C2),
    D5 is D2 - 1,
    nextCards(LE, D2, 0, [], 1, 0, D5, [], C3),
    append(C2, C3, CS).

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
    nextCards(Lista, Num, 0, [], Jnueva, 0, X, Mazo, Mazo2), !. %OJO CON MAZO

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

%FUNCIÓN 2

cardsSetlsDobble(Lista, C) :-
    mkset(Lista, C),
    largo(C, N),
    N = 3.

% FUNCIÓN 3

cardsSetNthCard([Y|_], 0, Y).

cardsSetNthCard([_|Xs], Entero, Card):-
          N is Entero - 1,
          cardsSetNthCard(Xs, N, Card).

% FUNCIÓN 4

cardsSetFindTotalCards(Card, TC) :-
    largo(Card, N),
    calculo(N, TC).

largo([], 0).

largo([_|Xs], N) :-
    largo(Xs, N1), N is N1 + 1.

calculo(N, TC) :-
    TC is ((N - 1) ** 2) + (N - 1) + 1.

% EJEMPLOS

% cardsSet: cardsSet([a,b,c,d,e,f,g,h,i,j,k,l], 3, 3, 92175, CS)

% cardsSetNthCard: cardsSetNthCard([[A,B],[A,C],[B,C]], 1, C1)

% cardsSetFindTotalCards: cardsSetFindTotalCards([A,B,C], TC)