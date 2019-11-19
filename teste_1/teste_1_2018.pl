%airport(Name, ICAO, Country)
airport('Aeroporto Francisco Sá Carneiro', 'LPPR', 'Portugal').
airport('Aeroporto Humberto Delgado', 'LPPT', 'Portugal').
airport('Aeropuerto Adolfo Suárez Madrid-Barajas', 'LEMD', 'Spain').
airport('Aéroport de Paris-Charles-de-Gaulle Roissy Airport', 'LFPG', 'France').
airport('Aeroporto Internazionale di Roma-Fiumicino - Leonardo da Vinci', 'LIRF', 'Italy').

% company(ICAO, Name, Year, Country)
company('TAP', 'TAP Air Portugal', 1945, 'Portugal').
company('RYR', 'Ryanair', 1984, 'Ireland').
company('AFR', 'Société Air France', 1933, 'France').
company('BAW', 'British Airways', 1974, 'United Kingdom').

% flight(Designation, Origin, Destination, DepartureTime, Duration, Company)
flight('TP1923', 'LPPR', 'LPPT', 1115, 55, 'TAP').
flight('TP1968', 'LPPT', 'LPPR', 2235, 55, 'TAP').
flight('TP842', 'LPPT', 'LIRF', 1450, 195, 'TAP').
flight('TP843', 'LIRF', 'LPPT', 1935, 195, 'TAP').
flight('FR5483', 'LPPR', 'LEMD', 630, 105, 'RYR').
flight('FR5484', 'LEMD', 'LPPR', 1935, 105, 'RYR').
flight('AF1024', 'LFPG', 'LPPT', 940, 155, 'AFR').
flight('AF1025', 'LPPT', 'LFPG', 1310, 155, 'AFR').

% ex 1
short(Flight):-
    flight(Flight, _, _, _, Time, _),
    Time < 90.

% ex 2
shorter(Flight1, Flight2, Flight1):-
    flight(Flight1, _, _, _, Time1, _),
    flight(Flight2, _, _, _, Time2, _),
    Time1 > Time2.
shorter(Flight1, Flight2, Flight2):-
    flight(Flight1, _, _, _, Time1, _),
    flight(Flight2, _, _, _, Time2, _),
    Time1 < Time2.
    
%ex3
arrivalTime(Flight, ArrivalTime):-
    flight(Flight, _, _, DepartureTime, Duration, _),
    DepartureTimeMinutes is DepartureTime mod 100,
    DepartureTimeHours is DepartureTime // 100,
    TotalTimeInMinutes is 60 * DepartureTimeHours + DepartureTimeMinutes + Duration,
    ArrivalTime is (TotalTimeInMinutes // 60) * 100 + (TotalTimeInMinutes mod 60).

%ex4
operates(Company, Country):-
    company(Company, _, _, _),
    flight(_, Id, _, _, _, Company),
    airport(_, Id, Country).

countries(Company, ListOfCountries):-
    countries(Company, [], ListOfCountries).

countries(Company, AuxList, ListOfCountries):-
    operates(Company, Country),
    \+member(Country, AuxList),
    !,
    countries(Company, [Country | AuxList], ListOfCountries).

countries(Company, AuxList, AuxList).

%ex5
timeDifference(Flight1, Flight2, TimeDifference):-
    flight(Flight2, _, _, DepartureTime, _, _),
    arrivalTime(Flight1, ArrivalTime),
    ArrivalTimeInMinutes is (ArrivalTime / 100) * 60 + (ArrivalTime // 60),
    DepartureTimeInMinutes is (DepartureTime / 100) * 60 + (DepartureTime // 60),
    TimeDifference is DepartureTimeInMinutes - ArrivalTimeInMinutes.

isPairableFlights(Flight1, Flight2):-
    flight(Flight1, _, Destination, _, _, _),
    flight(Flight2, Destination, _, _, _, _),
    timeDifference(Flight1, Flight2, DeltaTime),
    DeltaTime >= 30, DeltaTime =< 90.
     

getPairableFlights(AuxList, FinalList):-
    flight(Flight1, _, Destination, _, _, _),
    flight(Flight2, Destination, _, _, _, _),
    isPairableFlights(Flight1, Flight2),
    \+member(Destination-Flight1-Flight2, AuxList),
    write(Destination), write(' - '), write(Flight1), write(' \\ '), write(Flight2), nl,
    !,
    getPairableFlights([Destination-Flight1-Flight2 | AuxList], FinalList).

getPairableFlights(AuxList, AuxList).

pairableFlights:-
    getPairableFlights([], FinalList).
        

% ex 6
hasTime(ArrivalTime, DepartureTime):-
    ArrivalTimeInMinutes is (ArrivalTime / 100) * 60 + (ArrivalTime // 60),
    DepartureTimeInMinutes is (DepartureTime / 100) * 60 + (DepartureTime // 60),
    DepartureTimeInMinutes > ArrivalTimeInMinutes + 30.

getClosestFlight(Origin, Destination, Time, NextTime, Flight):-
    airport(_, OriginAir, Origin),
    airport(_, DestinationAir, Destination),
    flight(Flight, OriginAir, DestinationAir, DepartureTime, _, _),
    hasTime(Time, DepartureTime),
    arrivalTime(Flight, ArrivalTime),
    NextTime is ArrivalTime + 30.

getClosestFlight(Origin, Destination, Time, NextTime, Flight):-
    airport(_, OriginAir, Origin),
    airport(_, DestinationAir, Destination),
    flight(Flight, OriginAir, DestinationAir, DepartureTime, _, _),
    arrivalTime(Flight, ArrivalTime),
    NextTime is ArrivalTime + 30.

getFlightList([X], _, Aux, Aux).
getFlightList([A, B | Trip], Time, Aux, FlightTimes):-
    getClosestFlight(A, B, Time, NextTime, Flight),
    flight(Flight, _, _, DepartureTime, _, _),
    append(Aux, [DepartureTime], CurrentAux),
    getFlightList([B | Trip], NextTime, CurrentAux, FlightTimes).

getTripDays([X], Days, Days).
getTripDays([A, B | FlightTimes], BeginningDay, Days):-
    B < A,
    CurrentDays is BeginningDay + 1,
    getTripDays([B | FlightTimes], CurrentDays, Days).
getTripDays([A, B | FlightTimes], BeginningDay, Days):-
    getTripDays([B | FlightTimes], BeginningDay, Days).

tripDays(Trip, Time, FlightTimes, Days):-
    getFlightList(Trip, Time, [], FlightTimes),
    getTripDays(FlightTimes, 1, Days).

/**
 * Library use is now allowed
 */
:- use_module(library(lists)).

% ex 7
avgFlightLengthFromAirport(Airport, AvgLength):-
    Goal = (
        airport(_, Airport, _),
        flight(_, Airport, _, _, Duration, _)
    ),
    findall(Duration, Goal, Durations),
    sumlist(Durations, Sum),
    length(Durations, L),
    AvgLength is Sum / L.
    

% ex 8 - nao percebi o enunciado
mostInternational(Companies).

% ex 9
dif_max_2(X, Y):- X < Y, X >= Y - 2.

make_pairs([], _, []).
make_pairs(L, P, [X-Y | List]):-
    select(X, L, L2),
    select(Y, L2, L3),
    G =..[P, X, Y], G,
    make_pairs(L3, P, List).


% ex 10
make_pairs2([], _, []).
make_pairs2([X], _, []). % torna valido lista com length impar
make_pairs2(L, P, [X-Y | List]):-
    select(X, L, L2),
    select(Y, L2, L3),
    G =..[P, X, Y], G,
    make_pairs2(L3, P, List).

make_pairs2(L, P, List):-
    select(_X, L, L2),
    select(_Y, L2, L3),
    make_pairs(L3, P, List).

% ex 11 -  WTF?
make_max_pairs([], _, []).
make_max_pairs([X], _, []). % torna valido lista com length impar
make_max_pairs(L, P, [X-Y | List]):-
    select(X, L, L2),
    select(Y, L2, L3),
    G =..[P, X, Y], G,
    make_max_pairs(L3, P, List).

% ex 12 - Alguem percebeu o jogo?


    

