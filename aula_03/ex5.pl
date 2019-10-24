membro(X, [X | _]).

membro(X, [_ | L]):-
    membro(X, L).

membroB(X, L):-
    append(_, [X | _], L).

myLast(L, X):-
    append(_, [X], L).
    