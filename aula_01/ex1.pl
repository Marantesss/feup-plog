/* This is a comment */
% this is a line comment

/*
When asking questions:
 - Use _ when the atom does not matter (it can be anything)
 - Use a variable when we want to know the answer (answer will be atoms)
*/

/*
Do this when documenting
pred(+arg1, -arg2, ?arg3).

 + input
 - output
 ? either way

 \+ negative

 , AND
 ; OR
*/

male(aldo).
male(lincoln).
male(michael).
male(lj).

female(christina).
female(lisa).
female(sara).
female(ella).

% parent(x, y). x is y's parent
parent(aldo, lincoln).
parent(aldo, michael).
parent(christina, lincoln).
parent(christina, michael).
parent(michael, ella).
parent(sara, ella).
parent(lincoln, lj).
parent(lisa, lj).

% a) Michael's parents: ?- parent(Parent, michael).
% b) Aldo's children: ?- parent(aldo, Child).
