% --- factorial

% is -> atribui um valor a uma variavel (X is 1)
% =:= -> comparacao de expressoes numericas (6-4 =:= 5-3)
% = -> Verifica se dois termos sao unificaveis
% == -> Verifica se dois termos sao do mesmo tipo e têm o mesmo valor ( 'Variavel' == 'Atomos' e sempre falso)
% -> -> implicacao logica (desnecessario - EVITAR)

% a)
factorial(1, 1).
factorial(0, 1).

factorial(N, Valor):-
    N > 0, % Ao pedir outra resposta, o prolog vai tentar refazer. Como já fez factorial(1, 1) vai passar para baixo e fazer o factorial(0, 0). Dando origem a um ciclo sem fim
    N1 is N - 1, factorial(N1, V1),
    Valor is N * V1.

% b)
fib(0, 1).
fib(1, 1).

fib(N, Valor):-
    N > 1, % Mesma coisa que no factorial
    N1 is N - 1, fib(N1, V1),
    N2 is N - 2, fib(N2, V2),
    Valor is V1 + V2.
