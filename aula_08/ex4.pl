:-use_module(library(clpfd)).

/*
   SEND
 + MORE
 ------
  MONEY
*/

send(Vars):-
    Vars = [S, E, N, D, M, O, R, Y],
    % numbers are decimal
    domain(Vars, 0, 9),
    % no letter can have the same number
    all_different(Vars),
    % first letters cannot be 0
    S #\= 0, M #\= 0,
    % normal sum (SEND + MORE = MONEY)
    S*1000 + E*100 + N*10 + D + M*1000 + O*100 + R*10 + E #= M*10000 + O*1000 + N*100 + E*10 + Y,
    % find solutions
    labeling([],Vars).

efficientSend(Vars):-
    Vars = [S, E, N, D, M, O, R, Y],
    % numbers are decimal
    domain(Vars, 0, 9),
    % increments
    domains([C1, C2, C3, C4], 0, 1),
    % no letter can have the same number
    all_different(Vars),
    % first letters cannot be 0
    S #\= 0, M #\= 0,
    % sum units
    D + E #= Y + C1*10,
    % sum tens
    N + R + C1 #= E + C2*10,
    % sum hundreds
    E + O + C2 #= N + C3*10,
    % sum thousands
    S + M + C3 #= O + C4*10,
    % ten thousands can only be 1
    C4 #= M,
    % find solutions
    labeling([],Vars).
