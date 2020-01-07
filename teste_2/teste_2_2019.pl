% EX 1
/*
Usando uma matriz de emparelhamento de conjuntos binários de N presentes
para K pessoas, sem qualquer tipo de relação (i.e. podemos ter pessoas
com mais que um presente, ou sem presente nenhum), modelando este problema
num PSR (problema de satisfação por restrições), a dimenção do espaço de
procura é: 2^NK.
*/

% EX 2
/*
No entanto, é mais natural modelar este problema da seguinte forma:
 - considerar como variaveis de dominio os presentes (N) a atribuir
 - e como dominio as pessoas (K)
Com este modelação a dimensão no espaço de procura é: K^N.
*/

% EX 3
:- use_module(library(lists)).
:- use_module(library(clpfd)).

pres(N, K, Vars):-
    length(Vars, N),
    domain(Vars, 1, K),
    %
    indices(1, Vars),
    %
    labeling([], Vars).
    
indices(I, [V | Vs]):-
    V mod 2 #\= I mod 2,
    I1 is I + 1,
    indices(I1, Vs).
indices(_, []).
/*
O programa obtém soluções em que a paridade entre os indices dos presentes
e os indices das pessoas é diferente. Ou seja, para cada indice par na lista
de pessoas, está associado um indice impar na lista de presentes.
Isto faz com que nenhum presente seguido vá para a mesma pessoa.
*/

% EX 4
/*
A colocação de restrições no ex. 3 provoca alterações no domínio das
variáveis, antes da fase de pesquisa?
Não.
*/

% EX 5
/*
For K = 2 and N = 2,
pres(N, K, Vars).
Vars = [1, 2, 2, 1].
Then binary matrix will be:
Binary_Matrix = [
    [0, 1, 0, 1], % person 1
    [1, 0, 1, 0]  % person 2
]
*/
constroi_bins(_, [], []).
constroi_bins(I, [V | Vs], [LBin | LBins]):-
    I #= V #<=> LBin,
    constroi_bins(I, Vs, LBins).
% EX 6
/*
Variaveis de decisão:
     - lista em que o elemento de indice i indica em que compartimento o objeto correspondente deve ficar.
Domínios:
     - Entre 1 e nº de compartimentos

No exemplo:
prat([[30, 6], [75, 15]], [176-40, 396-24, 474-35, 250-8, 149-5, 479-5], Vars).
Vars = [3, 1, 3, 4, 1, 4], significa que:
     - compartimento 1 vai conter o objeto de indice 2 e 5;
     - compartimento 2 nao vai conter objetos;
     - compartimento 3 vai conter o objeto de indice 1 e 3;
     - compartimento 4 vai conter o objeto de indice 4 e 6.

Para resolver o problema é necessário restringir o volume de cada compartimento,
pelo que é necesário obter a soma dos volumes dos objetos cujo valor das variaveis
em Vars sejam iguais, e restringir que essa soma não é maior que o volume máximo
desse comportimento.
Depois para aplicar a restrição do peso de cada compartimento ser inferior ao que está
imediatamente abaixo, é necessário obter o peso de cada compartimento numa lista de
sequência e restringir que esse peso não pode ser inferior ao daquele que estiver
N casa antes, onde N é o número de compartimentos por linha.
*/

% EX 7
/*
prat(+Prateleiras, +Objetos, -Vars)

prat([[30, 6], [75, 15]], [176-40, 396-24, 474-35, 250-8, 149-5, 479-5], Vars).
Vars = [3, 1, 3, 4, 1, 4]

Matrix = [
    [0, 1, 0, 0, 1, 0],
    [0, 0, 0, 0, 0, 0],
    [1, 0, 1, 0, 0, 0],
    [0, 0, 0, 1, 0, 1]
]
*/
prat(Prateleiras, Objetos, Vars):-
    % ---- variables
    % obter numero de prateleiras por linha e coluna
    length(Prateleiras, NumeroLinhas),
    nth1(1, Prateleiras, Linha),
    length(Linha, NumeroColunas),
    % obter numero total de prateleiras
    NumeroPrateleiras is NumeroLinhas * NumeroColunas,
    % construir matrix
    length(Matrix, NumeroPrateleiras),
    maplist(same_length(Objetos), Matrix),

    % separar os objetos em duas listas (peso e dimensao)
    keys_and_values(Objetos, PesoObjetos, EspacoObjetos),
    
    % ---- domain
    % para cada prateleira
    maplist(shelf_domain, Matrix),
    % para cada objeto
    transpose(Matrix, TMatrix),
    maplist(object_domain, TMatrix),
    
    % ---- constraints
    % o total de espaco ocupado nao pode ser maior que a capacidade da prateleira
    append(Prateleiras, FlatPrateleiras),
    maplist(shelf_constraint(EspacoObjetos), FlatPrateleiras, Matrix),

    % ---- labeling
    append(Matrix, FlatMatrix),
    labeling([], FlatMatrix),
	findall(ShelfIndex, (nth1(_ObjectIndex, TMatrix, Line), nth1(ShelfIndex, Line, 1)), Vars).
    

% para cada prateleira
shelf_domain(Line):-
    domain(Line, 0, 1).

% para cada objeto
object_domain(Line):-
    domain(Line, 0, 1),
    % cada objeto tem apenas uma prateleira associada
    count(1, Line, #=, 1).

shelf_constraint(EspacoObjetos, DimensaoPrateleira, ObjetosPrateleira):-
    scalar_product(EspacoObjetos, ObjetosPrateleira, #=<, DimensaoPrateleira).
    
