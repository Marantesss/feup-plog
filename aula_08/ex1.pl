:-use_module(library(clpfd)).

magicSquare3(Cells):-
    Cells = [C1, C2, C3, C4, C5, C6, C7, C8, C9],
    % Cells in 1..9,
    domain(Cells, 1, 9),
    % a number can only be used once
    all_distinct(Cells),
    % find the magic number for more efficiency
    Sum is (1+2+3+4+5+6+7+8+9)//3,
    % rows
    C1 + C2 + C3 #= Sum,
    C4 + C5 + C6 #= Sum,
    C7 + C8 + C9 #= Sum,
    % cols
    C1 + C4 + C7 #= Sum,
    C2 + C5 + C8 #= Sum,
    C3 + C6 + C9 #= Sum,
    % diagonals
    C1 + C5 + C9 #= Sum,
    C3 + C5 + C7 #= Sum,
    % avoid simmmetries
    C1 #< C2, C1 #< C3, C1 #< C4, C2 #< C4,
    % find solution
    labeling([], Cells).

    
