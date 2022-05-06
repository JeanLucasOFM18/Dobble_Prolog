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
    append([L4], [[]], L5),
    append(L5, [[0]], L6),
    append(L6, [[]], L7),
    append(L7, [[]], L8),
    append(L8, [["Juego no Iniciado"]], L9),
    cantidadPuntajes(L9, D1, Game).
    
getNumPlayers(Lista, TC) :-
    obtenerElemento(Lista, 0, TC).

getCS(Lista, TC) :-
    obtenerElemento(Lista, 1, TC).

getMode(Lista, TC) :-
    obtenerElemento(Lista, 2, TC).

getSeed2(Lista, TC) :-
    obtenerElemento(Lista, 3, TC).

cantidadPuntajes(L8, 0, L8).

cantidadPuntajes(L8, N, L9) :-
    obtenerElemento(L8, 4, Puntajes),
    agregarFinal(Puntajes, 0, P1),
    eliminarLista(4, L8, GameAct),
    insertar(P1, GameAct, 5, G1),
    N1 is N - 1,
    cantidadPuntajes(G1, N1, L9), !.

cartasPorUsuario(L10, 0, L10).

cartasPorUsuario(L10, N, Game) :-
    obtenerElemento(L10, 5, CartaUsuarios),
    agregarFinal(CartaUsuarios, [], P1),
    eliminarLista(5, L10, GameAct),
    insertar(P1, GameAct, 6, G1),
    N1 is N - 1,
    cartasPorUsuario(G1, N1, Game), !.
    
% FUNCIÓN 7 LISTA

dobbleGameRegister(User, GameIn, GameOut) :-
    ((is_list(GameIn), Y = 0); (is_list(GameOut), Y = 1)),
    determinarFuncion(User, GameIn, GameOut, Y).

determinarFuncion(User, GameIn, GameOut, 0) :-
    obtenerElemento(GameIn, 0, D1),
    getNumPlayers(D1, N),
    obtenerElemento(GameIn, 1, D2),
    largo(D2, N1),
    N1 < N,
    register(D2, GameIn, User, N1, 0, GameOut).

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
    insertar(D2, D3, 2, GameIn).
    
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

compararUsuarios(_, _, 1, 0).

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

% FUNCIÓN 8

dobbleGameWhoseTurnIsIt(Game, Username) :-
    ((string(Username), X = 0) ; (is_list(Game), X = 1)),
    defineFuncionTurn(Game, Username, X), !.

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

insertar(El, L, 1, [El | L]).

insertar(El, [G | R], P, [G | Res]):-
	P1 is P - 1,
	insertar(El, R, P1, Res).

% FUNCIÓN 9

dobbleGamePlay(Game, Action, GameOut) :-
    ((is_list(Action), X = 1) ; (Action == null, X = 0) ; (Action == pass, X = 2) ; (Action == finish, X = 3)),
    ejecutarJuego(Game, Action, X, GameAct),
    ((X == 3, eliminarLista(5, GameAct, GameAct2), insertar(["Terminado"], GameAct2, 6, GameOut)) ;
    (X \== 3, eliminarLista(5, GameAct, GameAct2), insertar(["Jugando"], GameAct2, 6, GameOut))).

ejecutarJuego(Game, _, 0, GameOut) :-
    obtenerElemento(Game, 0, D1),
    obtenerElemento(D1, 1, CS),
    largo(CS, N),
    quedanCartas(Game, N, CS, GameOut).

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
    verificarElemento(Game, Elemento, GameOut).
    
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
    insertar(Ganadores, Game, 7, GameOut).

obtenerGanadores(_, _, _, N, N, ListaGanadores, ListaGanadores).

obtenerGanadores(Puntajes, Jugadores, PtsGanador, N, X, ListaGanadores, GameOut) :-
    obtenerElemento(Puntajes, X, PtsUsuario),
    ((PtsUsuario == PtsGanador, Y = 1) ; (PtsUsuario \== PtsGanador, Y = 0)),
    ganadores(Puntajes, Jugadores, PtsGanador, N, X, ListaGanadores, Y, GameOut).

ganadores(Puntajes, Jugadores, PtsGanador, N, X, ListaGanadores, 0, GameOut) :-
    X1 is X + 1,
    obtenerGanadores(Puntajes, Jugadores, PtsGanador, N, X1, ListaGanadores, GameOut).

ganadores(Puntajes, Jugadores, PtsGanador, N, X, ListaGanadores, 1, GameOut) :-
    obtenerElemento(Jugadores, X, Jugador),
    agregarFinal(ListaGanadores, Jugador, ListaAct),
    X1 is X + 1,
    obtenerGanadores(Puntajes, Jugadores, PtsGanador, N, X1, ListaAct, GameOut).
    
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

verificarElemento(Game, Elemento, GameOut) :-
    obtenerElemento(Game, 3, Mesa),
    obtenerElemento(Mesa, 0, C1),
    obtenerElemento(Mesa, 1, C2),
    largo(C1, N),
    compruebaElemento(C1, C2, N, 0, 0, Elemento, ER),
    obtenerRepetido(C1, ER, Repetido),
    ((Elemento == Repetido, X = 0) ; (Elemento \== Repetido, X = 1)),
    modificaPuntaje(Game, X, GameOut).

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

verificaTurno(Game, Cant, Cant, GameOut) :-
    eliminarLista(2, Game, Game2),
    agregarFinal([], 0, TurnoAct),
    insertar(TurnoAct, Game2, 3, GameOut).

verificaTurno(Game, Cant, Turno, GameOut) :-
    obtenerElemento(Game, 2, Dato),
    obtenerElemento(Dato, 0, Turno),
    TurnoNuevo is Turno + 1,
    eliminarLista(2, Game, Game2),
    agregarFinal([], TurnoNuevo, TurnoAct),
    insertar(TurnoAct, Game2, 3, GameOut).
    
obtenerRepetido(_, 0, null).

obtenerRepetido(C1, ER, GameOut) :-
    ER1 is ER - 1,
    obtenerElemento(C1, ER1, GameOut).
    
compruebaElemento(_, _, _, N1, 1, _, N1).

compruebaElemento(_, _, N, N, _, _, 0).

compruebaElemento(C1, C2, N, N1, 0, Elemento, ER) :-
    obtenerElemento(C1, N1, E1),
    ((member(E1, C2), X = 1) ; (not(member(E1, C2)), X = 0)),
    Aux is N1 + 1,
    compruebaElemento(C1, C2, N, Aux, X, Elemento, ER).
    
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

turnoCompletado(Game, N, N2, GameOut) :-
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

devolverCartas(_, 2, Mazo, Mazo).
   
devolverCartas(Mesa, N, Mazo, CS) :-
    obtenerElemento(Mesa, N, Carta),
    insertar(Carta, Mazo, 1, Mnuevo),
    Nnueva is N + 1,
    devolverCartas(Mesa, Nnueva, Mnuevo, CS).
    
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

% FUNCIÓN 10

dobbleGameStatus(Game, Status) :-
    obtenerElemento(Game, 5, Estado),
    obtenerElemento(Estado, 0, Status).

% FUNCIÓN 11

dobbleGameScore(Game, Username, Score).
    
% EJEMPLOS (PRONTO SE AGREGARÁN EJEMPLOS)

% cardsSet: cardsSet([a,b,c,d,e,f,g,h,i,j,k,l], 3, 3, 92175, CS)