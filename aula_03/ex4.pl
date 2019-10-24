inverter(L1, L2):-
    inverterAux(L1, [], L2).

inverterAux([], L, L).

inverterAux([X | L1], L2, L3):-
    inverterAux(L1, [X | L2], L3).
