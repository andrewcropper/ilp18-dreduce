% learning f/1
% clauses: 1
% clauses: 2
% clauses: 3
%time,0.028211
f(A):-f_1(A),f_2(A).
f_1(A):-has_car(A,B),roof_open(B).
f_2(A):-has_car(A,B),three_wheels(B).
