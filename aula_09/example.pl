:-use_module(library(clpfd)).


% 'Val' will appear on 'List' exactly 'Count' number of times
my_count_equals(_, [], 0).
my_count_equals(Val, [H | T], Count):-
    % B is 1 or 0 if Val = H (does not force constraint)
    (Val #= H) #<=> B,
    % when backtracking Count is equal to PreviousCount + B(1 or 0 if Val)
    Count #= PreviousCount + B,
    my_count_equals(Val, T, PreviousCount).
