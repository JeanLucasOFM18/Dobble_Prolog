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
    Contador == 1,
    write(true).

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
                    
miembro(H,[_|T]):-miembro(H,T).

intersecta([],_,0):-!.

intersecta([H|T],L2,Count):-
    miembro(H,L2),
    intersecta(T,L2,CountAux),
    Count is CountAux + 1, !.

intersecta([_|T],L2,Count):-
    intersecta(T,L2,Count).

% FUNCIÓN 3 LISTA

cardsSetNthCard([Y|_], 0, Y).

cardsSetNthCard([_|Xs], Entero, Card):-
          N is Entero - 1,
          cardsSetNthCard(Xs, N, Card).

% FUNCIÓN 4 LISTA

cardsSetFindTotalCards(Card, TC) :-
    largo(Card, N),
    calculo(N, TC).

% FUNCIÓN 5 SIN TERMINAR

cardsSetMissingCards(SetCartas, CS) :-
    obtenerElemento(SetCartas, 0, Carta),
    largo(Carta, Tamanio),
    calculo(Tamanio, CantCartas),
    largo(SetCartas, LargoMazo),
    comprueba(SetCartas, CantCartas, LargoMazo, CS).

comprueba(_, CantCartas, CantCartas, []).

% EXTRAS

largo([], 0).

largo([_|Xs], N) :-
    largo(Xs, N1), N is N1 + 1.

calculo(N, TC) :-
    TC is ((N - 1) ** 2) + (N - 1) + 1.

% EJEMPLOS

% cardsSet: cardsSet([a,b,c,d,e,f,g,h,i,j,k,l], 3, 3, 92175, CS)

% cardsSetNthCard: cardsSetNthCard([[A,B],[A,C],[B,C]], 1, C1)

% cardsSetFindTotalCards: cardsSetFindTotalCards([A,B,C], TC)

% cardsSet([a,b,c,d,e,f,g,h,i,j,k,l], 3, 3, 92175, CS), cardsSetNthCard(CS, 2, C2), cardsSetFindTotalCards(C2, TC).