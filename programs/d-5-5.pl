% learning f/1
% clauses: 1
% clauses: 2
% clauses: 3
%time,3.535855
f(A):-has_car(A,B),f_1(B).
f_1(A):-long(A),f_2(A).
f_2(A):-has_load(A,B),circle(B).
