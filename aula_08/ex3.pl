:-use_module(library(clpfd)).


nQueens(Cols):-
    Cols = [R1, R2, R3, R4],
    domain(Cols, 1, 4),
    all_distinct(Cols),
    % check first Row
    R1 #\= R2 + 1, R1 #\= R2 - 1, R1 #\= R3 + 2, R1 #\= R3 - 2, R1 #\= R4 + 3, R1 #\= R4 - 3,
    % check second Row
    R2 #\= R3 + 1, R2 #\= R3 - 1, R2 #\= R4 + 2, R2 #\= R4 + 2,
    % check third Row
    R3 #\= R4 + 1, R3 #\= R4 - 1,
    % find solutions
    labeling([], Cols).
    

    
