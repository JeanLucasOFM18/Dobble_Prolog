% TDA CARDSSET

cardsSet(Elements, NumC, MaxC, Seed, [Elements, NumC, MaxC, Seed]).

getElements([Elements, NumC, MaxC, Seed], Elements) :-
    cardsSet(Elements, NumC, MaxC, Seed, [Elements, NumC, MaxC, Seed]).

getNumC([Elements, NumC, MaxC, Seed], NumC) :-
    cardsSet(Elements, NumC, MaxC, Seed, [Elements, NumC, MaxC, Seed]).

getMaxC([Elements, NumC, MaxC, Seed], MaxC) :-
    cardsSet(Elements, NumC, MaxC, Seed, [Elements, NumC, MaxC, Seed]).

getSeed([Elements, NumC, MaxC, Seed], Seed) :-
    cardsSet(Elements, NumC, MaxC, Seed, [Elements, NumC, MaxC, Seed]).

cardsSetNthCard([Y|_], 0, Y).

cardsSetNthCard([_|Xs], Entero, Card):-
          N is Entero - 1,
          cardsSetNthCard(Xs, N, Card).

cardsSetFindTotalCards(Card, TC) :-
    largo(Card, N),
    calculo(N, TC).

largo([], 0).

largo([_|Xs], N) :-
    largo(Xs, N1), N is N1 + 1.

calculo(N, TC) :-
    TC is ((N - 1) ** 2) + (N - 1) + 1.

% EJEMPLOS

% cardsSet: cardsSet([a,b,c,d,f,g,h], 3, 3, 92175, CS)
% getElements: cardsSet([a,b,c,d,f,g,h], 3, 3, 92175, CS), getElements(CS, Elements)
% getNumC: cardsSet([a,b,c,d,f,g,h], 3, 3, 92175, CS), getNumC(CS, NumC)
% getMaxC: cardsSet([a,b,c,d,f,g,h], 3, 3, 92175, CS), getMaxC(CS, MaxC)
% getSeed: cardsSet([a,b,c,d,f,g,h], 3, 3, 92175, CS), getSeed(CS, Seed)

% cardsSetNthCard: cardsSetNthCard([[A,B],[A,C],[B,C]], 1, C1)

% cardsSetFindTotalCards: cardsSetFindTotalCards([A,B,C], TC)