
/*
L e constituido pela concatenacao de L1 com L2
*/
myAppend([], L, L).

myAppend([X | L1], L2, [X | L3]):-
    myAppend(L1, L2, L3).


