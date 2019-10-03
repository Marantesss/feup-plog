exec(X,Y) :- p(X,Y).
exec(X,X) :- s(X).
p(X,Y) :- q(X), r(Y).
p(X,Y) :- s(X), r(Y).
q(a).
q(b).
r(c).
r(d).
s(e).

% Command 'trace' will enable call trace
% Command 'notrace' will disable trace
% Informacao na seccao 5 do manual de utilizador de prolog
