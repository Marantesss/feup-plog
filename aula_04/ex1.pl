ligado(a,b).
ligado(a,c).
ligado(b,d).
ligado(b,e).
ligado(b,f).
ligado(c,g).
ligado(d,h).
ligado(d,i).
ligado(f,i).
ligado(f,j).
ligado(f,k).
ligado(g,l).
ligado(g,m).
ligado(k,n).
ligado(l,o).
ligado(i,f). 

membro(X, [X|_]):- !.
membro(X, [_|Y]):- membro(X,Y).

concatenar([], L, L).
concatenar([X | Y], L, [X | Lista]):-
    concatenar(Y, L, Lista).

inverter([X], [X]).
inverter([X | Y], Lista):-
    inverter(Y, Lista1),
    concatenar(Lista1, [X], Lista).


% pesquisa em profundidade

profundidade(Caminho, No_final, No_final, [No_final | Caminho]).
profundidade(Caminho, No, No_final, Sol):-
    ligado(No, No1),
    \+membro(No1, Caminho), % previne ciclos
    profundidade([No | Caminho], No1, No_final, Sol). 

pesq_prof(No_inicial, No_final, Solucao):-
    profundidade([], No_inicial, No_final, Sol_inv),
    inverter(Sol_inv, Solucao). 


