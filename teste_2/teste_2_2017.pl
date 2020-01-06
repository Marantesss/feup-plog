% EX 1
:- use_module(library(lists)).

p1(L1,L2) :-
    gen(L1,L2),
    test(L2).
    gen([],[]).
    gen(L1,[X|L2]) :-
    select(X,L1,L3),
    gen(L3,L2).

test([_,_]).
test([X1,X2,X3|Xs]) :-
    (X1 < X2, X2 < X3; X1 > X2, X2 > X3),
    test([X2,X3|Xs]).
/*
O que faz?
    O predicado p1 recebe uma lista L1 instanciada, com tamanho mínimo de 3
    elemento, e ordena-a seja por ordem decrescente ou crescente,
    guardado o resultado em L2. Por exemplo:
    | ?- p1([1,2,3], L2).
    L2 = [3, 2, 1] ? ; % ordem decrescente
    L2 = [1, 2, 3] ? ; % ordem crescente
    no

Eficiencia?
    Este tipo de algoritmo é do tipo generate and test, o que não é muito
    eficiente uma vez que são geradas todas as lsitas possiveis, e o
    predicado test encarrega-se de verificar se a lista está ordenada
*/

% EX 2
/*
As variáveis de domínio estão a ser instanciadas antes da fase de pesquisa e nem todas as
restrições foram colocadas antes da fase da pesquisa.
*/

% EX 3
:- use_module(library(clpfd)).
p2(L1, L2) :-
    length(L1, N),
    length(L2, N),
    %
    pos(L1, L2, Is),
    all_distinct(Is),
    test2(L2),
    %
    labeling([], Is).

pos([], _, []).
pos([X | Xs], L2, [I | Is]) :-
    element(I, L2, X),
    pos(Xs, L2, Is).

test2([_, _]).
test2([X1, X2, X3 | Xs]):-
    (X1 #< X2 #/\ X2 #< X3) #\/ (X1 #> X2 #/\ X2 #> X3),
    test2([X2, X3 | Xs]).

% EX 4
% BIG THANKS TO msramalho (https://github.com/msramalho/feup-plog/blob/master/exams/moodelo_16_17_plr/16_17_4.pl)

% sweet_recipes(+MaxTime,+NEggs,+RecipeTimes,+RecipeEggs,-Cookings,-Eggs).
% sweet_recipes(60, 30, [20,50,10,20,15],[6,4,12,20,6], Cookings, Eggs).
% sweet_recipes(120, 30, [20,50,10,20,15],[6,4,12,20,6], Cookings, Eggs).

sweet_recipes(MaxTime, NEggs, RecipeTimes, RecipeEggs, Cookings, Eggs):-
    % ---- variables
    length(RecipeTimes, NRecipes),
    length(Pratos, NRecipes),

    % ---- domains
    % um prato e ou não escolhido
    domain(Pratos, 0, 1),

    % ---- constraints
    % O Bonifacio apenas pretende fazer 3 pratos
    count(1, Pratos, #=, 3),
    /* scalar_product(+Coeffs, +Xs, +RelOp, ?Value)
        where Coeffs is a list of length n of integers, Xs is a list of
        length n of integers or domain variables, RelOp is a relational
        symbol as above, and Value is an integer or a domain variable.
        True if sum(Coeffs*Xs) RelOp Value.
    */
    % os pratos escolhidos nao podem ultrapassar o tempo limite
    scalar_product(RecipeTimes, Pratos, #=<, MaxTime),
    % caclular o numero de ovos gastos pelos 3 pratos escolhidos
    scalar_product(RecipeEggs, Pratos, #=, Eggs),
    % numero de ovos gastos nao pode ser superior ao numero de ovos disponiveis
    Eggs #=< NEggs,

    % ---- labeling
    % queremos gastar o maior numero de ovos possivel
    labeling([maximize(Eggs)], Pratos),
    % buscar os indices dos pratos escolhidos (estao a 1)
    findall(Index, nth1(Index, Pratos, 1), Cookings).
    
% EX 5
% BIG THANKS TO msramalho (https://github.com/msramalho/feup-plog/blob/master/exams/moodelo_16_17_plr/16_17_5.pl)

% cut(+Shelves,+Boards,-SelectedBoards)
% cut([12,50,14,8,10,90,24], [100,45,70], S).

cut(Shelves, Boards, SelectedBoards):-
    % ---- variables
    % numero de shelves
    length(Shelves, NShelves),
    % numero de SelectedBoard tem o mesmo tamanho que o numero de shelves
    % tal que index = shelf, value = board index
    length(SelectedBoards, NShelves),

    % ---- domain
    length(Boards, NBoards),
    domain(SelectedBoard, 1, NBoards),

    % ---- constraints
    get_tasks(Shelves, SelectedBoards, Tasks),
    get_machines(Boards, Machines, 1),
    /*
    The following constraint can be thought of as constraining n tasks
    to be placed in time and on m machines. Each machine has a resource
    limit, which is interpreted as a lower or upper bound on the total
    amount of resource used on that machine at any point in time that
    intersects with some task.
    */
    cumulatives(Tasks, Machines, [bound(upper)]),

    % ---- labeling
    labeling([], SelectedBoards).
    
get_tasks([], [], []).
get_tasks([S | Shelves], [B | SelectedBoards], [T | Tasks]):-
    /*
    A task is represented by a term task(Oi,Di,Ei,Hi,Ti) where:
     - Oi is the start time
     - Di the non-negative duration
     - Ei the end time
     - Hi the non-negative resource consumption
     - Ti the task identifier.
    All fields are domain variables with bounded domains.
    */
    T = task(0, S, S, S, B),
    getTasks(Shelves, SelectedBoards, Tasks).

get_machines([], [], _).
get_machines([B | Boards], [M | Machines], Id):-
    /*
    A machine is represented by a term machine(Mj,Lj) where:
    - Mj is the identifier, an integer;
    - and Lj is the resource bound of the machine, which must be a domain variable with bounded domains
    */
    M = machine(Id, B),
    NewId is Id + 1,
    get_machines(Boards, Machines, NewId).