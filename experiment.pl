:- use_module('metagol').
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(library(timeout)).
:- use_module(library(random)).
:- use_module(library(system)).

:-['data/trains'].

max_time(600000). % 10 mins
%% max_time(1200000). % 20 mins
%% max_time(1800000). % 30 mins

prim(has_car/2).
prim(has_load/2).

prim(short/1).
prim(long/1).

prim(two_wheels/1).
prim(three_wheels/1).

prim(roof_open/1).
prim(roof_closed/1).

prim(zero_load/1).
prim(one_load/1).
prim(two_load/1).
%% prim(three_load/1).

prim(circle/1).
prim(triangle/1).
prim(rectangle/1).

%% prim(roof_flat/1).
%% prim(roof_jagged/1).
%% prim(roof_arc/1).
%% prim(roof_peaked/1).
%% prim(diamond/1).
%% prim(inverted_triangle/1).
%% prim(hexagon/1).

do_learn:-
    max_time(MaxTime),
    findall(f(X),(pos_ex(Xs),member(X,Xs)),Pos),
    findall(f(X),(neg_ex(Xs),member(X,Xs)),Neg),
    get_time(T1),!,
    time_out(learn(Pos,Neg,Prog),MaxTime,Result),
    ((Result = time_out) -> (!,Tmp is MaxTime/1000,format('%time,~f\n',[Tmp]),false); true),!,
    get_time(T2),
    Duration is T2-T1,
    format('%time,~f\n',[Duration]),
    pprint(Prog).

set_rand:-
  get_time(A),
  string_codes(A,B),sumlist(B,X),
  Y=X,
  Z=X,
  setrand(rand(X,Y,Z)).

do_test:-
    set_rand,
    findall(f(X),(pos_ex(Xs),member(X,Xs)),Pos),
    findall(f(X),(neg_ex(Xs),member(X,Xs)),Neg),
    (current_predicate(f/1) -> (maplist(do_test_pos,Pos),maplist(do_test_neg,Neg)); (maplist(write_balanced,Pos),maplist(write_balanced,Neg))).

do_test_pos(Goal):-
  (call(Goal) -> writeln(1); writeln(0)).

do_test_neg(Goal):-
  (call(Goal) -> writeln(0); writeln(1)).

write_balanced(_Goal):-
    random(N),
    (N > 0.5 -> writeln(1); writeln(0)).