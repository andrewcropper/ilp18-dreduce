% learning f/1
% clauses: 1
% clauses: 2
% clauses: 3
%time,0.022492
f(A):-has_car(A,B),f_1(B).
f_1(A):-roof_open(A),f_2(A).
f_2(A):-has_load(A,B),zero_load(B).
