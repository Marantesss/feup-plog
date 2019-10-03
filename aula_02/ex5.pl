e_primo(2).
e_primo(3).

e_primo(N):-
    N > 3, % N tem que ser maior que 3
    integer(N), % N tem que ser inteiro
    N mod 2 =\= 0, % N nao pode ser par
    \+e_fator(N, 3).

e_fator(N, F):-
    N mod F =:= 0. % Para um N ser fator de F, N tem que ser divisivel por F

e_fator(N, F):-
    F * F < N, % apenas precisamos de testar ate a raiz quadrada do numero que queremos saber se e primo
    F1 is F + 2, e_fator(N, F1). % incrementa em 2 e volta a testar
