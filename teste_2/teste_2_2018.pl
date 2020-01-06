% EX 1
/*
O programa em Prolog gera duas listas (L1 e L2), em que L1 tem o tamanho N
e L2 tem o tamanho N - 1.
Cada elemento de cada uma das listas tem que pertencer obrogadtoriamente ao
intervalo [1, M]. O preenchimento destas listas é feito pelo predicado fill.
A soma de cada par de elementos da lista L1 é igual ao elemento da lista L2.

Por exemplo:
N = 4, M = 10
então
L1 = [1, 2, 4, 5] e L2 = [3, 6, 9]

O predicado check garante que: 1+2=3, 2+4=6, 4+5=9
*/

% EX 2
/*
M^(2N - 1)
*/

% EX 3
:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-use_module(library(between)).

check([_], []).
check([A, B | R], [X | Xs]) :-
    A + B #= X,
    check([B | R], Xs).

avoid_symmetry([_]).
avoid_symmetry([A, B | L2]):-
    A #=< B,
    avoid_symmetry([B | L2]).

prog2(N, M, L1, L2):-
    length(L1, N),
    N1 is N-1, length(L2, N1),
    domain(L1, 1, M),
    domain(L2, 1, M),
    check(L1, L2),
    avoid_symmetry(L2),
    labeling([], L1).

% EX 4
/*

BIG THANKS TO msramalho (https://github.com/msramalho/feup-plog/tree/master/exams/moodelo_16_17_plr)

gym_pairs(+MenHeights,+WomenHeights,+Delta,-Pairs)
 - emparelhamentos:
     - diferenca de alturas entre homem e mulher seja inferior a um delta
     - homem nunca pode ser maior que a mulher
*/
gym_pairs(MenHeights, WomenHeights, Delta, Pairs):-
	length(MenHeights, N),
	same_length(Hs, Ms, N),
	domain(Hs, 1, N),
	domain(Ms, 1, N),
	all_distinct(Hs),
    all_distinct(Ms),
    
    % scanlist/5 maps a ternary relation Pred down 2 lists.
    /*
    scanlist(Pred, Xs, Ys, V0, V) :-
        (   foreach(X,Xs),
            foreach(Y,Ys),
            fromto(V0,V1,V2,V),
            param(Pred)
        do  call(Pred, X, Y, V1, V2)
        ).
    */
	scanlist(height_rule(MenHeights, WomenHeights, Delta), Hs, Ms, 0, _),
	Hs = [First | Rest],
	scanlist(sort_it, Rest, First, _), % no symmetries
	
	append(Hs, Ms, Vars),
    labeling([], Vars),
    % keys_and_values(?[K1-V1,...,Kn-Vn], ?[K1,...,Kn], ?[V1,...,Vn])
	keys_and_values(Pairs, Hs, Ms).
	
height_rule(MenHeights, WomenHeights, Delta, Hi, Mi, 0, 0):-
	element(Hi, MenHeights, H),
	element(Mi, WomenHeights, M),
	H #> M #/\ H - M #=< Delta.
	
sort_it(Current, Prev, Current):-
    Prev #< Current.

% EX 5
/*
optimal_skating_pairs(+MenHeights,+WomenHeights,+Delta,-Pairs)
 - emparelhamentos:
     - diferenca de alturas entre homem e mulher seja inferior a um delta
     - homem nunca pode ser maior que a mulher
*/
optimal_skating_pairs(MenHeights, WomenHeights, Delta, Pairs):-
	same_length(Matrix, MenHeights),
    % maplist(:Pred, +List - succeeds when Pred(X) succeeds for each element X of List
	maplist(same_length(WomenHeights), Matrix),
	maplist(domain_and_constraint, Matrix),
    transpose(Matrix, TMatrix),
    maplist(domain_and_constraint, TMatrix),
    
	scanlist(height_rules(MenHeights, WomenHeights, Delta), Matrix, 1, _),
	scanlist(sum_line, Matrix, 0, CountPairs),
	append(Matrix, Vars),
    labeling([maximize(CountPairs), down], Vars),
	findall(H-M, (nth1(H, Matrix, Line), nth1(M, Line, 1)), Pairs).
	
height_rules(MenHeights, WomenHeights, Delta, Line, Hi, NextHi):-
	NextHi #= Hi + 1,
	scanlist(height_rule_cell(MenHeights, WomenHeights, Delta, Hi), Line, 1, _).
	
height_rule_cell(MenHeights, WomenHeights, Delta, Hi, Cell, Mi, NextMi):-
	NextMi #= Mi + 1,
	element(Hi, MenHeights, H),
	element(Mi, WomenHeights, M),
	Cell #=> H #> M #/\ H - M #=< Delta.

domain_and_constraint(Line):-
    domain(Line, 0, 1),
    % single match
	Match in 0..1,
	count(1, Line, #=, Match).

sum_line(Line, Prev, Sum):-
    sum(Line, #=, Acc),
    Sum #= Prev + Acc.
