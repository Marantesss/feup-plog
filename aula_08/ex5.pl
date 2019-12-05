:-use_module(library(clpfd)).

/*
                        
                          FORT PLOG

    |  S4_corner |     S5     |     S6     |  S7_corner |
    |     S3     |                         |     S8     |
    |     S2     |                         |     S9     |
    |  S1_corner |    S12     |    S11     | S10_corner |

*/

guards(Vars):-
    Vars = [S1_corner, S2, S3, S4_corner, S5, S6, S7_corner, S8, S9, S10_corner, S11, S12],
    % corner or side
    domain(Vars, 0, 12),
    % left side
    S1_corner + S2 + S3 + S4_corner #= 5,
    % up side
    S4_corner + S5 + S6 + S7_corner #= 5,
    % right side
    S7_corner + S8 + S9 + S10_corner #= 5,
    % down side
    S10_corner + S11 + S12 + S1_corner #= 5,
    % there are only 12 guards
    S1_corner + S2 + S3 + S4_corner + S5 + S6 + S7_corner + S8 + S9 + S10_corner + S11 + S12 #= 12,
    % find solutions
    labeling([],Vars).

