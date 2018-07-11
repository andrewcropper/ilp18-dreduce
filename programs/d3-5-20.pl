% learning f/1
% clauses: 1
% clauses: 2
% clauses: 3
%time,2.294818
f(A):-has_car(A,B),f_1(B).
f_1(A):-has_load(A,B),f_2(B).
f_2(A):-one_load(A),circle(A).
