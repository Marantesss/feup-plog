aluno(joao, paradigmas).
aluno(maria, paradigmas).
aluno(joel, lab2).
aluno(joel, estruturas).

frequenta(joao, feup).
frequenta(maria, feup).
frequenta(joel, ist).

professor(carlos, paradigmas).
professor(ana_paula, estruturas).
professor(pedro, lab2).

funcionario(pedro, ist).
funcionario(ana_paula, feup).
funcionario(carlos, feup).

% a)
professor_aluno(Professor, Aluno):-
    aluno(Aluno, Cadeira),
    professor(Professor, Cadeira).

% b)
pessoal_universidade(Universidade, Pessoa):-
    frequenta(Pessoa, Universidade).

pessoal_universidade(Universidade, Pessoa):-
    funcionario(Pessoa, Universidade).

% c)
colega(Pessoa_1, Pessoa_2):-
    frequenta(Pessoa_1, Cadeira),
    frequenta(Pessoa_2, Cadeira),
    Pessoa_1 \= Pessoa_2.

colega(Pessoa_1, Pessoa_2):-
    aluno(Pessoa_1, Universidade),
    aluno(Pessoa_2, Universidade),
    Pessoa_1 \= Pessoa_2.

colega(Pessoa_1, Pessoa_2):-
    funcionario(Pessoa_1, Universidade),
    funcionario(Pessoa_2, Universidade),
    Pessoa_1 \= Pessoa_2.

