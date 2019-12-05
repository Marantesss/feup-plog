:-use_module(library(clpfd)).

/*
1 -> BLUE
2 -> YELLOW
3 -> GREEN
4 -> RED
*/

three_different([_, _]).
three_different([A, B, C | T]):-
    all_distinct([A, B, C]),
    three_different([B, C | T]).

sequence([_,_,_], 0).
sequence([A, B, C, D | T], N):-
    sequence([B, C, D | T], N2),
    N #= N2 + V,
    % amarelo-verde-vermelho-azul
    (A #= 2 #/\ B #= 3 #/\ C #= 4 #/\ D #= 1) #<=> V.
    

yet_another_car_traffic:-
    % 12 automóveis estão parados, em fila indiana, num cruzamento com semáforos.
    length(Color, 12), domain(Color, 1, 4),
    % Os automóveis têm a seguinte distribuição de cores:
    /*
    % 4 amarelos
    count_equals(2, Color, 4),
    % 2 verdes
    count_equals(3, Color, 2),
    % 3 vermelhos
    count_equals(4, Color, 3),
    % 3 azuis;
    count_equals(1, Color, 3),
    */
    % OU -> MORE EFFICIENCY
    global_cardinality(Color, [2-4, 3-2, 4-3, 1-3]),
    % O primeiro e o último automóvel são da mesma cor
    element(1, Color, LastAndFirstCarColor),
    element(12, Color, LastAndFirstCarColor),
    % O segundo e o penúltimo são da mesma cor
    element(2, Color, SecondCarColor),
    element(11, Color, SecondCarColor),
    % O quinto automóvel é azul
    element(5, Color, 1),
    % Todos os conjuntos de três automóveis consecutivos têm três cores distintas
    three_different(Color),
    % Partindo do primeiro automóvel para o último,
    % é possível visualizar a sequência amarelo-verde-vermelho-azul uma única vez.
    sequence(Color, 1),
    % find solution
    labeling([], Color),
    write(Color).
