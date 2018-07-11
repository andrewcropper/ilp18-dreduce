% learning f/1
% clauses: 1
% clauses: 2
% clauses: 3
%time,0.036920
f(A):-has_car(A,B),f_1(B).
f_1(A):-has_load(A,B),zero_load(B).
f_1(A):-three_wheels(A),roof_open(A).
