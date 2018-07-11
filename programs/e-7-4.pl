% learning f/1
% clauses: 1
% clauses: 2
% clauses: 3
% clauses: 4
%time,252.309898
f(A):-has_car(A,B),f_1(A,B).
f_1(A,B):-has_car(A,C),f_2(C,B).
f_2(A,B):-has_load(A,C),f_3(C,B).
f_3(A,B):-zero_load(A),roof_open(B).
