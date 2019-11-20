% participant(Id, Age, Performance)
participant(1234, 17, 'Pé coxinho').
participant(3423, 21, 'Programmar com os pés').
participant(3788, 20, 'Sing a Bit').
participant(4865, 22, 'Pontes de esparguete').
participant(8937, 19, 'Pontes de pen-drives').
participant(2564, 20, 'Moodle hack').

%performance(Id, Times)
performance(1234, [120, 120, 120, 120]).
performance(3423, [32, 120, 45, 120]).
performance(3788, [110, 2, 6, 43]).
performance(4865, [120, 120, 110, 120]).
performance(8937, [97, 101, 105, 110]).

% ex 1
madeItThrough(Id):-
    performance(Id, List),
    member(120, List).

% ex 2
getElement(1, [Element | _], Element).
getElement(N, [_ | List], Element):-
    N1 is N - 1,
    getElement(N1, List, Element).

getJuriTimes([], _, []).
getJuriTimes([P | Participants], JuriMember, [Time | List]):-
    performance(P, Times),
    getElement(JuriMember, Times, Time),
    getJuriTimes(Participants, JuriMember, List).

sumList([], SumAux, SumAux).
sumList([N | List], SumAux, Sum):-
    NextSumAux is SumAux + N,
    sumList(List, NextSumAux, Sum).

juriTimes(Participants, JuriMember, Times, Total):-
    getJuriTimes(Participants, JuriMember, Times),
    sumList(Times, 0, Total).

test2:-
    juriTimes([1234, 3423, 3788, 4865, 8937], 1, Times, Total),
    write(Times), nl,
    write(Total), nl,
    juriTimes([1234, 3423, 3788, 4865, 8937], 2, Times2, Total2),
    write(Times2), nl,
    write(Total2), nl.

% ex 3
getPerformances(AuxPerformances, Performances):-
    performance(P, _),
    \+member(P, AuxPerformances),
    !,
    getPerformances([P | AuxPerformances], Performances).
getPerformances(AuxPerformances, AuxPerformances).

getAbsences([], Acc, Acc).
getAbsences([Time | Times], Acc, N):-
    Time == 120,
    NextAcc is Acc + 1,
    getAbsences(Times, NextAcc, N).
getAbsences([_ | Times], Acc, N):-
    getAbsences(Times, Acc, N).

patientJuri(JuriMember):-
    getPerformances([], Performances),
    getJuriTimes(Performances, JuriMember, Times),
    getAbsences(Times, 0, N),
    N >= 2.

% ex 4
bestParticipant(P1, P2, P):-
    performance(P1, Times1),
    sumList(Times1, 0, Total1),
    performance(P2, Times2),
    sumList(Times2, 0, Total2),
    Total1 > Total2,
    P is P1.

bestParticipant(P1, P2, P):-
    performance(P1, Times1),
    sumList(Times1, 0, Total1),
    performance(P2, Times2),
    sumList(Times2, 0, Total2),
    Total1 < Total2,
    P is P2.

test4:-
    bestParticipant(3423, 1234, Z),
    write(Z).

% ex 5
printPerformances(Performances):-
    performance(P, Times),
    participant(P, _, Act),
    \+member(P, Performances),
    write(P), write(':'), write(Act), write(':'), write(Times), nl,
    !,
    printPerformances([P | Performances]).
printPerformances(_).

allPerfs:-
    printPerformances([]).

%ex 6
notClicked(P):-
    performance(P, Times),
    notClickedTimes(Times).

notClickedTimes([]).
notClickedTimes([Time | Times]):-
    Time == 120,
    notClickedTimes(Times).

nSuccessfulParticipants(T):-
    Goal = (
        performance(P, _),
        notClicked(P)
    ),
    findall(P, Goal, Success),
    length(Success, T).

test6:-
    nSuccessfulParticipants(T),
    write(T).
    
% ex 7
getJuris([], _, []).
getJuris([Time | Times], N, [N | Fans]):-
    Time == 120,
    N1 is N + 1,
    getJuris(Times, N1, Fans).
getJuris([Time | Times], N, Fans):-
    N1 is N + 1, 
    getJuris(Times, N1, Fans).

getFans(Participant, Fans):-
    performance(Participant, Times),
    getJuris(Times, 1, Fans).

juriFansAux([], []).
juriFansAux([P | Performances], [P-Fans | JuriFanList]):-
    performance(P, _),
    getFans(P, Fans),
    juriFansAux(Performances, JuriFanList).

reverse([],Z,Z).
reverse([H|T],Z,Acc):-
    reverse(T,Z,[H|Acc]).

juriFans(JuriFanList):-
    getPerformances([], Performances),
    reverse(Performances, List, []),
    juriFansAux(List, JuriFanList).

% ex 8
:- use_module(library(lists)).

eligibleOutcome(Id, Perf, TT):-
    performance(Id, Times),
    madeItThrough(Id),
    participant(Id, _, Perf),
    sumlist(Times, TT).
