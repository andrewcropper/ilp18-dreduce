% learning f/1
% clauses: 1
% clauses: 2
% clauses: 3
% clauses: 4
%time,49.643578
f(A):-f_1(A),f_2(A).
f_1(A):-has_car(A,B),three_wheels(B).
f_2(A):-has_car(A,B),f_3(B).
f_3(A):-long(A),two_wheels(A).
