% learning f/1
% clauses: 1
% clauses: 2
% clauses: 3
%time,0.007154
f(A):-has_car(A,B),f_1(A,B).
f_1(A,B):-has_car(A,C),f_2(C,B).
f_2(A,B):-roof_open(A),two_wheels(B).
