% learning f/1
% clauses: 1
% clauses: 2
% clauses: 3
% clauses: 4
%time,11033.971487
f(A):-f_1(A,B),f_2(A,B).
f_1(A,B):-f_2(A,C),f_2(C,B).
f_2(A,B):-one_load(A),zero_load(B).
f_2(A,B):-has_car(A,C),has_load(C,B).
