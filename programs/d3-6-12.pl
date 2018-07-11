% learning f/1
% clauses: 1
% clauses: 2
% clauses: 3
%time,2.703625
f(A):-has_car(A,B),f_1(B).
f_1(A):-has_load(A,B),f_2(B).
f_2(A):-two_load(A),circle(A).
