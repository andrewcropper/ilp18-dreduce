% learning f/1
% clauses: 1
% clauses: 2
%time,0.005766
f(A):-has_car(A,B),f_1(B).
f_1(A):-short(A),roof_closed(A).
