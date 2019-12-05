:-use_module(library(clpfd)).

/*
1 -> BLUE
2 -> YELLOW
3 -> GREEN
4 -> BLACK
*/

car_traffic:-
    % 4 carros com cores e tamanhos diferentes
    length(Size, 4), domain(Size, 1, 4), all_distinct(Size),
    length(Color, 4), domain(Color, 1, 4), all_distinct(Color),
    % o carro que está IMEDIATAMENTE ANTES do carro azul é
    % menor do que o que está IMEDIATAMENTE DEPOIS do carro azul
    element(BlueCarPosition, Color, 1),
    AfterBlueCarPosition #= BlueCarPosition - 1,
    BeforeBlueCarPosition #= BlueCarPosition + 1,
    element(AfterBlueCarPosition, Size, AfterBlueCarSize),
    element(BeforeBlueCarPosition, Size, PreviousBlueCarSize),
    PreviousBlueCarSize #< AfterBlueCarSize,
    % carro verde é o menor de todos
    element(GreenCarPosition, Color, 3),
    element(GreenCarPosition, Size, 1), % 1 e o tamanho mais pequeno
    % carro verde está depois do carro azul
    GreenCarPosition #< BlueCarPosition,
    % carro amarelo está depois do preto
    element(YellowCarPosition, Color, 2),
    element(BlackCarPosition, Color, 4),
    YellowCarPosition #< BlackCarPosition,
    % find solutions
    append(Size, Color, Vars), labeling([], Vars),
    write(Size), write(Color).
    
    

    

    
