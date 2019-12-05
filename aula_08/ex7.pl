:-use_module(library(clpfd)).

roastedTurkey(Price):-
    Price = [P1, P2, P3, P4, PricePerTurkey],
    NumberOfTurkeys = 72,
    % thousands
    P1 in 1..9,
    % hundreds
    P2 #= 6,
    % tens ?
    P3 #= 7,
    % units ?
    P4 in 1..9,
    % price per turkey
    PricePerTurkey * NumberOfTurkeys #= (P1 * 1000 + P2 * 100 + P3 * 10 + P4),
    % find solution
    labeling([], Price).
