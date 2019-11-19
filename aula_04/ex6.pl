ligacao(1,2).
ligacao(1,3).
ligacao(2,4).
ligacao(3,4).
ligacao(3,6).
ligacao(4,6).
ligacao(5,6).

% grafo bidirecional
ligacao2(X, Y):- ligacao(X, Y).
ligacao2(X, Y):- ligacao(Y, X).

% a)
caminho(+NoInicio, +NoFim, -Lista):-
    caminho(NoInicio, NoFim, [NoInicio], Lista, 5).

caminho(NoInicio, NoFim, Lista, ListaFim,_):-
    ligacao2(NoInicio, NoFim),
    append(Lista, [NoFim], ListaFim).

caminho(NoInicio, NoFim, Lista, ListaFim, N):-
    N>0,
    ligacao2(NoInicio, NoInterm),
    NoInterm \= NoFim,
    \+member(NoInterm, Lista),
    append(Lista, [NoInterm], Lista2),
    N2 is N-1,
    caminho(NoInterm, NoFim, Lista2, ListaFim, N2).

% b)
ciclos(No, Comp, Lista):-
    findall(Ciclo, caminho(No, No, [], Ciclo, Comp), Lista).

% cenas aleatorias :)
father(j, a).
father(j, b).
father(j, c).
father(j, d).
father(j, e).

children(X,Kids) :- children(X,[],Kids).
children(X,Cs,Kids) :-
    father(X,Kid), 
    \+member(Kid,Cs),
    !,
    children(X,[Kid|Cs],Kids).
children(X,Cs,Cs).

children2(X,Kids) :- assert(kids(X,[])), fail.
children2(X,Kids) :-
 father(X,Kid),
 retract(kids(X,Cs)), assert(kids(X,[Kid|Cs])),
 fail.
children2(X,Kids) :- retract(kids(X,Kids)).