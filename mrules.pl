%% :-['../../utils.pl'].

%% :- initialization main.

%% prim(has_car/2).
%% prim(has_load/2).
%% %% prim(next_to/2).

%% prim(short/1).
%% prim(long/1).

%% prim(one_wheel/1).
%% prim(two_wheels/1).
%% prim(three_wheels/1).

%% prim(roof_open/1).
%% prim(roof_closed/1).

%% prim(roof_flat/1).
%% prim(roof_jagged/1).
%% prim(roof_arc/1).
%% prim(roof_peaked/1).

%% prim(zero_load/1).
%% prim(one_load/1).
%% prim(two_load/1).
%% prim(three_load/1).

%% prim(circle/1).
%% prim(triangle/1).
%% prim(rectangle/1).
%% prim(diamond/1).
%% prim(inverted_triangle/1).
%% prim(hexagon/1).

%% bind([],_,_,[]).
%% bind([H|T],Monadics,Dyadics,[Bound|Out]):-
%%     bind_aux(H,Monadics,Dyadics,Bound),
%%     bind(T,Monadics,Dyadics,Out).

%% bind_aux([A],Monadics,_,[P,A]):-
%%     random_member(P,Monadics).
%% bind_aux([A,B],_,Dyadics,[P,A,B]):-
%%     random_member(P,Dyadics).

%% main:-
%%     findall(R,(mrule(R),R=[[_]|_]),Rs),
%%     findall(M,prim(M/1),Monadics),
%%     findall(D,prim(D/2),Dyadics),
%%     between(1,10,ID),
%%     random_member(R1,Rs),
%%     consts_to_vars(R1,[[H]|B1]),
%%     bind(B1,Monadics,Dyadics,B2),

%%     writeln(b2-B2),
%%     pprint_list_to_clause(B2,C2),
%%     L=(train(ID,H):-C2),
%%     format('~w.\n',L),
%%     false.
%% main.

%% pprint_list_to_clause(List1,Clause):-
%%     atomsaslists_to_atoms(List1,List2),
%%     list_to_clause(List2,Clause).

%% atomsaslists_to_atoms([],[]).
%% atomsaslists_to_atoms(['@'(Atom)|T1],Out):- !,
%%     (get_option(print_ordering) -> Out=[Atom|T2]; Out=T2),
%%     atomsaslists_to_atoms(T1,T2).
%% atomsaslists_to_atoms([AtomAsList|T1],[Atom|T2]):-
%%     atom_to_list(Atom,AtomAsList),
%%     atomsaslists_to_atoms(T1,T2).

%% list_to_clause([Atom],Atom):-!.
%% list_to_clause([Atom|T1],(Atom,T2)):-!,
%%     list_to_clause(T1,T2).
%% list_to_atom(AtomList,Atom):-
%%     Atom =..AtomList.
%% atom_to_list(Atom,AtomList):-
%%     Atom =..AtomList.


%% mrule([[a], [a]]).
%% mrule([[a, b], [a, b]]).
%% mrule([[a, b], [b, a]]).
%% mrule([[a], [a], [a]]).
%% mrule([[a, b], [a], [b]]).
%% mrule([[a, b], [a], [a, b]]).
%% mrule([[a, b], [a], [b, a]]).
%% mrule([[a, b], [b], [a, b]]).
%% mrule([[a], [b], [b, a]]).
%% mrule([[a, b], [b], [b, a]]).
%% mrule([[a, b], [a, b], [a, b]]).
%% mrule([[a, b], [a, b], [b, a]]).
%% mrule([[a, b], [a, c], [b, c]]).
%% mrule([[a, b], [a, c], [c, b]]).
%% mrule([[a], [b, a], [b, a]]).
%% mrule([[a, b], [b, a], [b, a]]).
%% mrule([[a, b], [b, c], [c, a]]).
%% mrule([[a, b], [c, a], [c, b]]).
%% mrule([[a], [a], [a], [a]]).
%% mrule([[a, b], [a], [a], [b]]).
%% mrule([[a, b], [a], [a], [a, b]]).
%% mrule([[a, b], [a], [a], [b, a]]).
%% mrule([[a, b], [a], [b], [b]]).
%% mrule([[a, b], [a], [b], [a, b]]).
%% mrule([[a], [a], [b], [b, a]]).
%% mrule([[a, b], [a], [b], [b, a]]).
%% mrule([[a, b], [a], [c], [b, c]]).
%% mrule([[a, b], [a], [c], [c, b]]).
%% mrule([[a, b], [a], [a, b], [a, b]]).
%% mrule([[a, b], [a], [a, b], [b, a]]).
%% mrule([[a, b], [a], [a, c], [b, c]]).
%% mrule([[a, b], [a], [a, c], [c, b]]).
%% mrule([[a], [a], [b, a], [b, a]]).
%% mrule([[a, b], [a], [b, a], [b, a]]).
%% mrule([[a, b], [a], [b, c], [b, c]]).
%% mrule([[a, b], [a], [b, c], [c, a]]).
%% mrule([[a, b], [a], [b, c], [c, b]]).
%% mrule([[a, b], [a], [c, a], [c, b]]).
%% mrule([[a, b], [a], [c, b], [c, b]]).
%% mrule([[a, b], [b], [b], [a, b]]).
%% mrule([[a], [b], [b], [b, a]]).
%% mrule([[a, b], [b], [b], [b, a]]).
%% mrule([[a, b], [b], [c], [a, c]]).
%% mrule([[a, b], [b], [c], [c, a]]).
%% mrule([[a, b], [b], [a, b], [a, b]]).
%% mrule([[a, b], [b], [a, b], [b, a]]).
%% mrule([[a, b], [b], [a, c], [a, c]]).
%% mrule([[a], [b], [a, c], [b, c]]).
%% mrule([[a, b], [b], [a, c], [b, c]]).
%% mrule([[a, b], [b], [a, c], [c, a]]).
%% mrule([[a], [b], [a, c], [c, b]]).
%% mrule([[a, b], [b], [a, c], [c, b]]).
%% mrule([[a], [b], [b, a], [b, a]]).
%% mrule([[a, b], [b], [b, a], [b, a]]).
%% mrule([[a], [b], [b, c], [c, a]]).
%% mrule([[a, b], [b], [b, c], [c, a]]).
%% mrule([[a, b], [b], [c, a], [c, a]]).
%% mrule([[a], [b], [c, a], [c, b]]).
%% mrule([[a, b], [b], [c, a], [c, b]]).
%% mrule([[a, b], [c], [a, b], [a, c]]).
%% mrule([[a, b], [c], [a, b], [b, c]]).
%% mrule([[a, b], [c], [a, b], [c, a]]).
%% mrule([[a, b], [c], [a, b], [c, b]]).
%% mrule([[a, b], [c], [a, c], [b, a]]).
%% mrule([[a, b], [c], [a, c], [b, c]]).
%% mrule([[a, b], [c], [a, c], [c, b]]).
%% mrule([[a, b], [c], [b, a], [b, c]]).
%% mrule([[a, b], [c], [b, a], [c, a]]).
%% mrule([[a, b], [c], [b, a], [c, b]]).
%% mrule([[a, b], [c], [b, c], [c, a]]).
%% mrule([[a, b], [c], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [a, b]]).
%% mrule([[a, b], [a, b], [a, b], [b, a]]).
%% mrule([[a, b], [a, b], [a, c], [a, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, c]]).
%% mrule([[a, b], [a, b], [a, c], [c, a]]).
%% mrule([[a, b], [a, b], [a, c], [c, b]]).
%% mrule([[a, b], [a, b], [b, a], [b, a]]).
%% mrule([[a, b], [a, b], [b, c], [b, c]]).
%% mrule([[a, b], [a, b], [b, c], [c, a]]).
%% mrule([[a, b], [a, b], [b, c], [c, b]]).
%% mrule([[a, b], [a, b], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [d, b], [d, c]]).
%% mrule([[a], [b, a], [b, a], [b, a]]).
%% mrule([[a, b], [b, a], [b, a], [b, a]]).
%% mrule([[a], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [b, a], [b, c], [b, c]]).
%% mrule([[a], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [b, a], [b, c], [c, a]]).
%% mrule([[a], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, a]]).
%% mrule([[a], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, b]]).
%% mrule([[a], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [b, a], [c, b], [c, b]]).
%% mrule([[a], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [b, c], [c, a], [c, a]]).
%% mrule([[a], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [c, b], [d, a], [d, c]]).
%% mrule([[a], [a], [a], [a], [a]]).
%% mrule([[a, b], [a], [a], [a], [b]]).
%% mrule([[a, b], [a], [a], [a], [a, b]]).
%% mrule([[a, b], [a], [a], [a], [b, a]]).
%% mrule([[a, b], [a], [a], [b], [b]]).
%% mrule([[a, b], [a], [a], [b], [a, b]]).
%% mrule([[a], [a], [a], [b], [b, a]]).
%% mrule([[a, b], [a], [a], [b], [b, a]]).
%% mrule([[a, b], [a], [a], [c], [b, c]]).
%% mrule([[a, b], [a], [a], [c], [c, b]]).
%% mrule([[a, b], [a], [a], [a, b], [a, b]]).
%% mrule([[a, b], [a], [a], [a, b], [b, a]]).
%% mrule([[a, b], [a], [a], [a, c], [b, c]]).
%% mrule([[a, b], [a], [a], [a, c], [c, b]]).
%% mrule([[a], [a], [a], [b, a], [b, a]]).
%% mrule([[a, b], [a], [a], [b, a], [b, a]]).
%% mrule([[a, b], [a], [a], [b, c], [b, c]]).
%% mrule([[a, b], [a], [a], [b, c], [c, a]]).
%% mrule([[a, b], [a], [a], [b, c], [c, b]]).
%% mrule([[a, b], [a], [a], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b], [b], [b]]).
%% mrule([[a, b], [a], [b], [b], [a, b]]).
%% mrule([[a], [a], [b], [b], [b, a]]).
%% mrule([[a, b], [a], [b], [b], [b, a]]).
%% mrule([[a, b], [a], [b], [c], [a, c]]).
%% mrule([[a, b], [a], [b], [c], [b, c]]).
%% mrule([[a, b], [a], [b], [c], [c, a]]).
%% mrule([[a, b], [a], [b], [c], [c, b]]).
%% mrule([[a, b], [a], [b], [a, b], [a, b]]).
%% mrule([[a, b], [a], [b], [a, b], [b, a]]).
%% mrule([[a, b], [a], [b], [a, c], [a, c]]).
%% mrule([[a], [a], [b], [a, c], [b, c]]).
%% mrule([[a, b], [a], [b], [a, c], [b, c]]).
%% mrule([[a, b], [a], [b], [a, c], [c, a]]).
%% mrule([[a], [a], [b], [a, c], [c, b]]).
%% mrule([[a, b], [a], [b], [a, c], [c, b]]).
%% mrule([[a], [a], [b], [b, a], [b, a]]).
%% mrule([[a, b], [a], [b], [b, a], [b, a]]).
%% mrule([[a, b], [a], [b], [b, c], [b, c]]).
%% mrule([[a], [a], [b], [b, c], [c, a]]).
%% mrule([[a, b], [a], [b], [b, c], [c, a]]).
%% mrule([[a, b], [a], [b], [b, c], [c, b]]).
%% mrule([[a, b], [a], [b], [c, a], [c, a]]).
%% mrule([[a], [a], [b], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b], [c, b], [c, b]]).
%% mrule([[a, b], [a], [c], [c], [b, c]]).
%% mrule([[a, b], [a], [c], [c], [c, b]]).
%% mrule([[a, b], [a], [c], [a, b], [a, c]]).
%% mrule([[a, b], [a], [c], [a, b], [b, c]]).
%% mrule([[a, b], [a], [c], [a, b], [c, a]]).
%% mrule([[a, b], [a], [c], [a, b], [c, b]]).
%% mrule([[a, b], [a], [c], [a, c], [b, a]]).
%% mrule([[a, b], [a], [c], [a, c], [b, c]]).
%% mrule([[a, b], [a], [c], [a, c], [c, b]]).
%% mrule([[a, b], [a], [c], [b, a], [b, c]]).
%% mrule([[a, b], [a], [c], [b, a], [c, a]]).
%% mrule([[a, b], [a], [c], [b, a], [c, b]]).
%% mrule([[a, b], [a], [c], [b, c], [b, c]]).
%% mrule([[a, b], [a], [c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [c], [b, c], [c, b]]).
%% mrule([[a, b], [a], [c], [b, d], [c, d]]).
%% mrule([[a, b], [a], [c], [b, d], [d, c]]).
%% mrule([[a, b], [a], [c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [c], [c, b], [c, b]]).
%% mrule([[a, b], [a], [c], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c], [d, b], [d, c]]).
%% mrule([[a, b], [a], [a, b], [a, b], [a, b]]).
%% mrule([[a, b], [a], [a, b], [a, b], [b, a]]).
%% mrule([[a, b], [a], [a, b], [a, c], [a, c]]).
%% mrule([[a, b], [a], [a, b], [a, c], [b, c]]).
%% mrule([[a, b], [a], [a, b], [a, c], [c, a]]).
%% mrule([[a, b], [a], [a, b], [a, c], [c, b]]).
%% mrule([[a, b], [a], [a, b], [b, a], [b, a]]).
%% mrule([[a, b], [a], [a, b], [b, c], [b, c]]).
%% mrule([[a, b], [a], [a, b], [b, c], [c, a]]).
%% mrule([[a, b], [a], [a, b], [b, c], [c, b]]).
%% mrule([[a, b], [a], [a, b], [c, a], [c, a]]).
%% mrule([[a, b], [a], [a, b], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a, b], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [a], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [a], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [a], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [a], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [a], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [a], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [a], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [a], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [a], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [a], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [a], [a, c], [d, b], [d, c]]).
%% mrule([[a], [a], [b, a], [b, a], [b, a]]).
%% mrule([[a, b], [a], [b, a], [b, a], [b, a]]).
%% mrule([[a], [a], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [a], [b, a], [b, c], [b, c]]).
%% mrule([[a], [a], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [a], [b, a], [b, c], [c, a]]).
%% mrule([[a], [a], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [a], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [a], [b, a], [c, a], [c, a]]).
%% mrule([[a], [a], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b, a], [c, a], [c, b]]).
%% mrule([[a], [a], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b, c], [b, c], [b, c]]).
%% mrule([[a], [a], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [a], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [a], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [a], [b, c], [c, a], [c, a]]).
%% mrule([[a], [a], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [a], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [a], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [a], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [a], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [a], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [b], [b], [b], [a, b]]).
%% mrule([[a], [b], [b], [b], [b, a]]).
%% mrule([[a, b], [b], [b], [b], [b, a]]).
%% mrule([[a, b], [b], [b], [c], [a, c]]).
%% mrule([[a, b], [b], [b], [c], [c, a]]).
%% mrule([[a, b], [b], [b], [a, b], [a, b]]).
%% mrule([[a, b], [b], [b], [a, b], [b, a]]).
%% mrule([[a, b], [b], [b], [a, c], [a, c]]).
%% mrule([[a], [b], [b], [a, c], [b, c]]).
%% mrule([[a, b], [b], [b], [a, c], [b, c]]).
%% mrule([[a, b], [b], [b], [a, c], [c, a]]).
%% mrule([[a], [b], [b], [a, c], [c, b]]).
%% mrule([[a, b], [b], [b], [a, c], [c, b]]).
%% mrule([[a], [b], [b], [b, a], [b, a]]).
%% mrule([[a, b], [b], [b], [b, a], [b, a]]).
%% mrule([[a], [b], [b], [b, c], [c, a]]).
%% mrule([[a, b], [b], [b], [b, c], [c, a]]).
%% mrule([[a, b], [b], [b], [c, a], [c, a]]).
%% mrule([[a], [b], [b], [c, a], [c, b]]).
%% mrule([[a, b], [b], [b], [c, a], [c, b]]).
%% mrule([[a, b], [b], [c], [c], [a, c]]).
%% mrule([[a, b], [b], [c], [c], [c, a]]).
%% mrule([[a, b], [b], [c], [a, b], [a, c]]).
%% mrule([[a, b], [b], [c], [a, b], [b, c]]).
%% mrule([[a, b], [b], [c], [a, b], [c, a]]).
%% mrule([[a, b], [b], [c], [a, b], [c, b]]).
%% mrule([[a, b], [b], [c], [a, c], [a, c]]).
%% mrule([[a], [b], [c], [a, c], [b, a]]).
%% mrule([[a, b], [b], [c], [a, c], [b, a]]).
%% mrule([[a], [b], [c], [a, c], [b, c]]).
%% mrule([[a, b], [b], [c], [a, c], [b, c]]).
%% mrule([[a, b], [b], [c], [a, c], [c, a]]).
%% mrule([[a], [b], [c], [a, c], [c, b]]).
%% mrule([[a, b], [b], [c], [a, c], [c, b]]).
%% mrule([[a, b], [b], [c], [a, d], [c, d]]).
%% mrule([[a, b], [b], [c], [a, d], [d, c]]).
%% mrule([[a], [b], [c], [b, a], [b, c]]).
%% mrule([[a, b], [b], [c], [b, a], [b, c]]).
%% mrule([[a], [b], [c], [b, a], [c, a]]).
%% mrule([[a, b], [b], [c], [b, a], [c, a]]).
%% mrule([[a], [b], [c], [b, a], [c, b]]).
%% mrule([[a, b], [b], [c], [b, a], [c, b]]).
%% mrule([[a], [b], [c], [b, c], [c, a]]).
%% mrule([[a, b], [b], [c], [b, c], [c, a]]).
%% mrule([[a, b], [b], [c], [c, a], [c, a]]).
%% mrule([[a], [b], [c], [c, a], [c, b]]).
%% mrule([[a, b], [b], [c], [c, a], [c, b]]).
%% mrule([[a, b], [b], [c], [c, d], [d, a]]).
%% mrule([[a, b], [b], [c], [d, a], [d, c]]).
%% mrule([[a, b], [b], [a, b], [a, b], [a, b]]).
%% mrule([[a, b], [b], [a, b], [a, b], [b, a]]).
%% mrule([[a, b], [b], [a, b], [a, c], [a, c]]).
%% mrule([[a, b], [b], [a, b], [a, c], [b, c]]).
%% mrule([[a, b], [b], [a, b], [a, c], [c, a]]).
%% mrule([[a, b], [b], [a, b], [a, c], [c, b]]).
%% mrule([[a, b], [b], [a, b], [b, a], [b, a]]).
%% mrule([[a, b], [b], [a, b], [b, c], [b, c]]).
%% mrule([[a, b], [b], [a, b], [b, c], [c, a]]).
%% mrule([[a, b], [b], [a, b], [b, c], [c, b]]).
%% mrule([[a, b], [b], [a, b], [c, a], [c, a]]).
%% mrule([[a, b], [b], [a, b], [c, a], [c, b]]).
%% mrule([[a, b], [b], [a, b], [c, b], [c, b]]).
%% mrule([[a, b], [b], [a, c], [a, c], [a, c]]).
%% mrule([[a], [b], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [b], [a, c], [a, c], [b, a]]).
%% mrule([[a], [b], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [b], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [b], [a, c], [a, c], [c, a]]).
%% mrule([[a], [b], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [b], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [b], [a, c], [a, d], [c, d]]).
%% mrule([[a, b], [b], [a, c], [a, d], [d, c]]).
%% mrule([[a], [b], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [b], [a, c], [b, a], [b, c]]).
%% mrule([[a], [b], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [b], [a, c], [b, a], [c, a]]).
%% mrule([[a], [b], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [b], [a, c], [b, a], [c, b]]).
%% mrule([[a], [b], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [b], [a, c], [b, c], [b, c]]).
%% mrule([[a], [b], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [b], [a, c], [b, c], [c, a]]).
%% mrule([[a], [b], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [b], [a, c], [b, c], [c, b]]).
%% mrule([[a], [b], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [b], [a, c], [b, d], [c, d]]).
%% mrule([[a], [b], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [b], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [b], [a, c], [c, a], [c, a]]).
%% mrule([[a], [b], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [b], [a, c], [c, a], [c, b]]).
%% mrule([[a], [b], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [b], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [b], [a, c], [c, d], [c, d]]).
%% mrule([[a, b], [b], [a, c], [c, d], [d, a]]).
%% mrule([[a], [b], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [b], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [b], [a, c], [c, d], [d, c]]).
%% mrule([[a, b], [b], [a, c], [d, a], [d, c]]).
%% mrule([[a], [b], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [d, c], [d, c]]).
%% mrule([[a], [b], [b, a], [b, a], [b, a]]).
%% mrule([[a, b], [b], [b, a], [b, a], [b, a]]).
%% mrule([[a], [b], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [b], [b, a], [b, c], [b, c]]).
%% mrule([[a], [b], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [b], [b, a], [b, c], [c, a]]).
%% mrule([[a], [b], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [b], [b, a], [b, c], [c, b]]).
%% mrule([[a], [b], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [b], [b, a], [c, a], [c, a]]).
%% mrule([[a], [b], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [b], [b, a], [c, a], [c, b]]).
%% mrule([[a], [b], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [b], [b, a], [c, b], [c, b]]).
%% mrule([[a], [b], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [b], [b, c], [b, c], [c, a]]).
%% mrule([[a], [b], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [b], [b, c], [c, a], [c, a]]).
%% mrule([[a], [b], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [b], [b, c], [c, a], [c, b]]).
%% mrule([[a], [b], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [b], [b, c], [c, d], [d, a]]).
%% mrule([[a], [b], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [b], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c, a], [c, a], [c, a]]).
%% mrule([[a], [b], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [b], [c, a], [c, a], [c, b]]).
%% mrule([[a], [b], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [b], [c, a], [c, d], [d, a]]).
%% mrule([[a], [b], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [b], [c, a], [d, a], [d, c]]).
%% mrule([[a], [b], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c, a], [d, c], [d, c]]).
%% mrule([[a], [b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b], [c, b], [c, d], [d, a]]).
%% mrule([[a], [b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [b], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [c], [c], [a, b], [a, c]]).
%% mrule([[a, b], [c], [c], [a, b], [b, c]]).
%% mrule([[a, b], [c], [c], [a, b], [c, a]]).
%% mrule([[a, b], [c], [c], [a, b], [c, b]]).
%% mrule([[a, b], [c], [c], [a, c], [b, a]]).
%% mrule([[a, b], [c], [c], [a, c], [b, c]]).
%% mrule([[a, b], [c], [c], [a, c], [c, b]]).
%% mrule([[a, b], [c], [c], [b, a], [b, c]]).
%% mrule([[a, b], [c], [c], [b, a], [c, a]]).
%% mrule([[a, b], [c], [c], [b, a], [c, b]]).
%% mrule([[a, b], [c], [c], [b, c], [c, a]]).
%% mrule([[a, b], [c], [c], [c, a], [c, b]]).
%% mrule([[a, b], [c], [d], [a, c], [b, d]]).
%% mrule([[a, b], [c], [d], [a, c], [d, b]]).
%% mrule([[a, b], [c], [d], [a, d], [b, c]]).
%% mrule([[a, b], [c], [d], [a, d], [c, b]]).
%% mrule([[a, b], [c], [d], [b, c], [d, a]]).
%% mrule([[a, b], [c], [d], [b, d], [c, a]]).
%% mrule([[a, b], [c], [d], [c, a], [d, b]]).
%% mrule([[a, b], [c], [d], [c, b], [d, a]]).
%% mrule([[a, b], [c], [a, b], [a, b], [a, c]]).
%% mrule([[a, b], [c], [a, b], [a, b], [b, c]]).
%% mrule([[a, b], [c], [a, b], [a, b], [c, a]]).
%% mrule([[a, b], [c], [a, b], [a, b], [c, b]]).
%% mrule([[a, b], [c], [a, b], [a, c], [a, c]]).
%% mrule([[a, b], [c], [a, b], [a, c], [b, a]]).
%% mrule([[a, b], [c], [a, b], [a, c], [b, c]]).
%% mrule([[a, b], [c], [a, b], [a, c], [c, a]]).
%% mrule([[a, b], [c], [a, b], [a, c], [c, b]]).
%% mrule([[a, b], [c], [a, b], [a, d], [c, d]]).
%% mrule([[a, b], [c], [a, b], [a, d], [d, c]]).
%% mrule([[a, b], [c], [a, b], [b, a], [b, c]]).
%% mrule([[a, b], [c], [a, b], [b, a], [c, a]]).
%% mrule([[a, b], [c], [a, b], [b, a], [c, b]]).
%% mrule([[a, b], [c], [a, b], [b, c], [b, c]]).
%% mrule([[a, b], [c], [a, b], [b, c], [c, a]]).
%% mrule([[a, b], [c], [a, b], [b, c], [c, b]]).
%% mrule([[a, b], [c], [a, b], [b, d], [c, d]]).
%% mrule([[a, b], [c], [a, b], [b, d], [d, c]]).
%% mrule([[a, b], [c], [a, b], [c, a], [c, a]]).
%% mrule([[a, b], [c], [a, b], [c, a], [c, b]]).
%% mrule([[a, b], [c], [a, b], [c, b], [c, b]]).
%% mrule([[a, b], [c], [a, b], [c, d], [d, a]]).
%% mrule([[a, b], [c], [a, b], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, b], [d, a], [d, c]]).
%% mrule([[a, b], [c], [a, b], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [c], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [c], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [c], [a, c], [a, d], [b, d]]).
%% mrule([[a, b], [c], [a, c], [a, d], [d, b]]).
%% mrule([[a, b], [c], [a, c], [b, a], [b, a]]).
%% mrule([[a, b], [c], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [c], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [c], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [c], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [c], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [c], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [c], [a, c], [b, d], [b, d]]).
%% mrule([[a, b], [c], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [c], [a, c], [b, d], [d, a]]).
%% mrule([[a, b], [c], [a, c], [b, d], [d, b]]).
%% mrule([[a, b], [c], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [c], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [c], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [c], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, c], [d, a], [d, b]]).
%% mrule([[a, b], [c], [a, c], [d, b], [d, b]]).
%% mrule([[a, b], [c], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, d], [a, d], [b, c]]).
%% mrule([[a, b], [c], [a, d], [a, d], [c, b]]).
%% mrule([[a, b], [c], [a, d], [b, a], [c, d]]).
%% mrule([[a, b], [c], [a, d], [b, a], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, c], [b, d]]).
%% mrule([[a, b], [c], [a, d], [b, c], [c, d]]).
%% mrule([[a, b], [c], [a, d], [b, c], [d, a]]).
%% mrule([[a, b], [c], [a, d], [b, c], [d, b]]).
%% mrule([[a, b], [c], [a, d], [b, c], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, a]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, b]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, d]]).
%% mrule([[a, b], [c], [a, d], [b, d], [d, c]]).
%% mrule([[a, b], [c], [a, d], [c, a], [d, b]]).
%% mrule([[a, b], [c], [a, d], [c, b], [c, d]]).
%% mrule([[a, b], [c], [a, d], [c, b], [d, a]]).
%% mrule([[a, b], [c], [a, d], [c, b], [d, b]]).
%% mrule([[a, b], [c], [a, d], [c, b], [d, c]]).
%% mrule([[a, b], [c], [a, d], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, d], [d, b], [d, c]]).
%% mrule([[a, b], [c], [b, a], [b, a], [b, c]]).
%% mrule([[a, b], [c], [b, a], [b, a], [c, a]]).
%% mrule([[a, b], [c], [b, a], [b, a], [c, b]]).
%% mrule([[a, b], [c], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [c], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [c], [b, a], [b, c], [c, b]]).
%% mrule([[a], [b], [c, a], [c, d], [b, d]]).
%% mrule([[a, b], [c], [b, a], [b, d], [c, d]]).
%% mrule([[a, b], [c], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [c], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [c], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [c], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [c], [b, a], [c, d], [d, a]]).
%% mrule([[a], [b], [c, a], [b, d], [d, c]]).
%% mrule([[a, b], [c], [b, a], [c, d], [d, b]]).
%% mrule([[a, b], [c], [b, a], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [c], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [c], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [c], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [c], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [c], [b, c], [d, a], [d, a]]).
%% mrule([[a, b], [c], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [c], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [c], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [c], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [c], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [c], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [c], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [c], [b, d], [c, d], [d, a]]).
%% mrule([[a, b], [c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [c], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [c], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [c], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [c], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [a, b], [a, b]]).
%% mrule([[a, b], [a, b], [a, b], [a, b], [b, a]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [a, c]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [b, c]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [c, a]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [b, a], [b, a]]).
%% mrule([[a, b], [a, b], [a, b], [b, c], [b, c]]).
%% mrule([[a, b], [a, b], [a, b], [b, c], [c, a]]).
%% mrule([[a, b], [a, b], [a, b], [b, c], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [a, b], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [a, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [c, a]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [b, a], [b, a], [b, a]]).
%% mrule([[a, b], [a, b], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [a, b], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [a, b], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [a, b], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [b, d]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [b, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [b, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [d, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [b, c]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [c, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [b, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [c, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [b, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [c, a]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [c, b]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [c, b]]).
%% mrule([[a], [a, b], [c, a], [c, d], [b, d]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [a, c], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [a, c], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [d, a]]).
%% mrule([[a], [a, b], [c, a], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [b, d]]).
%% mrule([[a], [a, b], [c, b], [c, d], [b, d]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [a, c], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a, c], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [d, a]]).
%% mrule([[a], [a, b], [c, b], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, b]]).
%% mrule([[a], [a, b], [c, d], [c, d], [b, d]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [c, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [d, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [d, a]]).
%% mrule([[a], [a, b], [c, d], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [a, c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [d, e], [c, e]]).
%% mrule([[a, b], [a, c], [d, b], [c, e], [e, d]]).
%% mrule([[a], [b, a], [b, a], [b, a], [b, a]]).
%% mrule([[a, b], [b, a], [b, a], [b, a], [b, a]]).
%% mrule([[a], [b, a], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [b, a], [b, a], [b, c], [b, c]]).
%% mrule([[a], [b, a], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [b, a], [b, a], [b, c], [c, a]]).
%% mrule([[a], [b, a], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [b, a], [b, a], [b, c], [c, b]]).
%% mrule([[a], [b, a], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [b, a], [b, a], [c, a], [c, a]]).
%% mrule([[a], [b, a], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [b, a], [b, a], [c, a], [c, b]]).
%% mrule([[a], [b, a], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [b, a], [b, a], [c, b], [c, b]]).
%% mrule([[a], [b, a], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [b, c]]).
%% mrule([[a], [b, a], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [c, a]]).
%% mrule([[a], [b, a], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [c, b]]).
%% mrule([[a], [b, a], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [c, d]]).
%% mrule([[a], [b, a], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [d, c]]).
%% mrule([[a], [b, a], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [b, a], [b, c], [c, a], [c, a]]).
%% mrule([[a], [b, a], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [b, a], [b, c], [c, a], [c, b]]).
%% mrule([[a], [b, a], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [b, a], [b, c], [c, b], [c, b]]).
%% mrule([[a], [b, a], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [c, d]]).
%% mrule([[a], [b, a], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [d, a]]).
%% mrule([[a], [b, a], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [d, b]]).
%% mrule([[a], [b, a], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [d, c]]).
%% mrule([[a], [b, a], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [d, a], [d, c]]).
%% mrule([[a], [b, a], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [d, b], [d, c]]).
%% mrule([[a], [b, a], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [c, a], [c, a]]).
%% mrule([[a], [b, a], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, a], [c, b]]).
%% mrule([[a], [b, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [d, a]]).
%% mrule([[a], [b, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [d, a], [d, c]]).
%% mrule([[a], [b, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [d, c], [d, c]]).
%% mrule([[a], [b, a], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [b, a], [c, b], [c, b], [c, b]]).
%% mrule([[a], [b, a], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [c, d]]).
%% mrule([[a], [b, a], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [d, a]]).
%% mrule([[a], [b, a], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [d, b]]).
%% mrule([[a], [b, a], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [d, c]]).
%% mrule([[a], [b, a], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b, a], [c, b], [d, a], [d, c]]).
%% mrule([[a], [b, a], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, b], [d, b], [d, c]]).
%% mrule([[a], [b, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [c, d], [c, d], [d, a]]).
%% mrule([[a], [b, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [c, d], [d, a], [d, c]]).
%% mrule([[a], [b, a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, d], [d, b], [d, c]]).
%% mrule([[a], [b, c], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [b, c], [b, c], [b, c], [c, a]]).
%% mrule([[a], [b, c], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [b, c], [b, c], [b, d], [d, a]]).
%% mrule([[a], [b, c], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [b, c], [b, c], [c, a], [c, a]]).
%% mrule([[a], [b, c], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [b, c], [b, c], [c, a], [c, b]]).
%% mrule([[a], [b, c], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [b, c], [d, a], [d, a]]).
%% mrule([[a], [b, c], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [b, c], [d, a], [d, b]]).
%% mrule([[a], [b, c], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [b, c], [d, a], [d, c]]).
%% mrule([[a], [b, c], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [b, c], [b, d], [b, d], [c, a]]).
%% mrule([[a], [b, c], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [c, d]]).
%% mrule([[a], [b, c], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [d, a]]).
%% mrule([[a], [b, c], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [d, b]]).
%% mrule([[a], [b, c], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [d, c]]).
%% mrule([[a], [b, c], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, b], [d, a]]).
%% mrule([[a], [b, c], [b, d], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, d], [d, a]]).
%% mrule([[a], [b, c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [c, a], [c, a]]).
%% mrule([[a], [b, c], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, a], [c, b]]).
%% mrule([[a], [b, c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [d, a]]).
%% mrule([[a], [b, c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [d, a], [d, a]]).
%% mrule([[a], [b, c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [d, a], [d, c]]).
%% mrule([[a], [b, c], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [d, b], [d, b]]).
%% mrule([[a], [b, c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [d, c], [d, c]]).
%% mrule([[a], [b, c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [c, b], [d, a], [d, a]]).
%% mrule([[a], [b, c], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [c, b], [d, a], [d, b]]).
%% mrule([[a], [b, c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [d, a]]).
%% mrule([[a], [b, c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [d, a], [d, c]]).
%% mrule([[a], [b, c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [d, e], [c, e]]).
%% mrule([[a, b], [b, c], [d, a], [c, e], [e, d]]).
%% mrule([[a, b], [c, a], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [c, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [c, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [c, a], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [c, a], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [c, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [c, a], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [c, a], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [c, a], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [c, a], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [c, a], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [c, a], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [c, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [c, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [c, a], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c, a], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [c, a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [c, a], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c, a], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [c, a], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [c, a], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [c, a], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [c, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [c, b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [c, b], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [c, b], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [c, b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [c, b], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [d, a]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [c, b], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [c, b], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [c, b], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [c, b], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [c, b], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [c, b], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [c, d], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c, d], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [c, d], [c, e], [d, b], [e, a]]).
%% mrule([[a, b], [c, d], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c, d], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [c, d], [d, b], [e, a], [e, c]]).
%% mrule([[a], [a], [a], [a], [a], [a]]).
%% mrule([[a, b], [a], [a], [a], [a], [b]]).
%% mrule([[a, b], [a], [a], [a], [a], [a, b]]).
%% mrule([[a, b], [a], [a], [a], [a], [b, a]]).
%% mrule([[a, b], [a], [a], [a], [b], [b]]).
%% mrule([[a, b], [a], [a], [a], [b], [a, b]]).
%% mrule([[a], [a], [a], [a], [b], [b, a]]).
%% mrule([[a, b], [a], [a], [a], [b], [b, a]]).
%% mrule([[a, b], [a], [a], [a], [c], [b, c]]).
%% mrule([[a, b], [a], [a], [a], [c], [c, b]]).
%% mrule([[a, b], [a], [a], [a], [a, b], [a, b]]).
%% mrule([[a, b], [a], [a], [a], [a, b], [b, a]]).
%% mrule([[a, b], [a], [a], [a], [a, c], [b, c]]).
%% mrule([[a, b], [a], [a], [a], [a, c], [c, b]]).
%% mrule([[a], [a], [a], [a], [b, a], [b, a]]).
%% mrule([[a, b], [a], [a], [a], [b, a], [b, a]]).
%% mrule([[a, b], [a], [a], [a], [b, c], [b, c]]).
%% mrule([[a, b], [a], [a], [a], [b, c], [c, a]]).
%% mrule([[a, b], [a], [a], [a], [b, c], [c, b]]).
%% mrule([[a, b], [a], [a], [a], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a], [a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a], [b], [b], [b]]).
%% mrule([[a, b], [a], [a], [b], [b], [a, b]]).
%% mrule([[a], [a], [a], [b], [b], [b, a]]).
%% mrule([[a, b], [a], [a], [b], [b], [b, a]]).
%% mrule([[a, b], [a], [a], [b], [c], [a, c]]).
%% mrule([[a, b], [a], [a], [b], [c], [b, c]]).
%% mrule([[a, b], [a], [a], [b], [c], [c, a]]).
%% mrule([[a, b], [a], [a], [b], [c], [c, b]]).
%% mrule([[a, b], [a], [a], [b], [a, b], [a, b]]).
%% mrule([[a, b], [a], [a], [b], [a, b], [b, a]]).
%% mrule([[a, b], [a], [a], [b], [a, c], [a, c]]).
%% mrule([[a], [a], [a], [b], [a, c], [b, c]]).
%% mrule([[a, b], [a], [a], [b], [a, c], [b, c]]).
%% mrule([[a, b], [a], [a], [b], [a, c], [c, a]]).
%% mrule([[a], [a], [a], [b], [a, c], [c, b]]).
%% mrule([[a, b], [a], [a], [b], [a, c], [c, b]]).
%% mrule([[a], [a], [a], [b], [b, a], [b, a]]).
%% mrule([[a, b], [a], [a], [b], [b, a], [b, a]]).
%% mrule([[a, b], [a], [a], [b], [b, c], [b, c]]).
%% mrule([[a], [a], [a], [b], [b, c], [c, a]]).
%% mrule([[a, b], [a], [a], [b], [b, c], [c, a]]).
%% mrule([[a, b], [a], [a], [b], [b, c], [c, b]]).
%% mrule([[a, b], [a], [a], [b], [c, a], [c, a]]).
%% mrule([[a], [a], [a], [b], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a], [b], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a], [b], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a], [c], [c], [b, c]]).
%% mrule([[a, b], [a], [a], [c], [c], [c, b]]).
%% mrule([[a, b], [a], [a], [c], [a, b], [a, c]]).
%% mrule([[a, b], [a], [a], [c], [a, b], [b, c]]).
%% mrule([[a, b], [a], [a], [c], [a, b], [c, a]]).
%% mrule([[a, b], [a], [a], [c], [a, b], [c, b]]).
%% mrule([[a, b], [a], [a], [c], [a, c], [b, a]]).
%% mrule([[a, b], [a], [a], [c], [a, c], [b, c]]).
%% mrule([[a, b], [a], [a], [c], [a, c], [c, b]]).
%% mrule([[a, b], [a], [a], [c], [b, a], [b, c]]).
%% mrule([[a, b], [a], [a], [c], [b, a], [c, a]]).
%% mrule([[a, b], [a], [a], [c], [b, a], [c, b]]).
%% mrule([[a, b], [a], [a], [c], [b, c], [b, c]]).
%% mrule([[a, b], [a], [a], [c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [a], [c], [b, c], [c, b]]).
%% mrule([[a, b], [a], [a], [c], [b, d], [c, d]]).
%% mrule([[a, b], [a], [a], [c], [b, d], [d, c]]).
%% mrule([[a, b], [a], [a], [c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a], [c], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a], [c], [c, d], [d, b]]).
%% mrule([[a, b], [a], [a], [c], [d, b], [d, c]]).
%% mrule([[a, b], [a], [a], [a, b], [a, b], [a, b]]).
%% mrule([[a, b], [a], [a], [a, b], [a, b], [b, a]]).
%% mrule([[a, b], [a], [a], [a, b], [a, c], [a, c]]).
%% mrule([[a, b], [a], [a], [a, b], [a, c], [b, c]]).
%% mrule([[a, b], [a], [a], [a, b], [a, c], [c, a]]).
%% mrule([[a, b], [a], [a], [a, b], [a, c], [c, b]]).
%% mrule([[a, b], [a], [a], [a, b], [b, a], [b, a]]).
%% mrule([[a, b], [a], [a], [a, b], [b, c], [b, c]]).
%% mrule([[a, b], [a], [a], [a, b], [b, c], [c, a]]).
%% mrule([[a, b], [a], [a], [a, b], [b, c], [c, b]]).
%% mrule([[a, b], [a], [a], [a, b], [c, a], [c, a]]).
%% mrule([[a, b], [a], [a], [a, b], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a], [a, b], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [a], [a], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [a], [a], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [a], [a], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [a], [a], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [a], [a], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [a], [a], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [a], [a], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [a], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [a], [a], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [a], [a], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [a], [a], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [a], [a], [a, c], [d, b], [d, c]]).
%% mrule([[a], [a], [a], [b, a], [b, a], [b, a]]).
%% mrule([[a, b], [a], [a], [b, a], [b, a], [b, a]]).
%% mrule([[a], [a], [a], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [a], [a], [b, a], [b, c], [b, c]]).
%% mrule([[a], [a], [a], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [a], [a], [b, a], [b, c], [c, a]]).
%% mrule([[a], [a], [a], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [a], [a], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [a], [a], [b, a], [c, a], [c, a]]).
%% mrule([[a], [a], [a], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a], [b, a], [c, a], [c, b]]).
%% mrule([[a], [a], [a], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a], [b, c], [b, c], [b, c]]).
%% mrule([[a], [a], [a], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [a], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [a], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [a], [a], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [a], [a], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [a], [a], [b, c], [c, a], [c, a]]).
%% mrule([[a], [a], [a], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [a], [a], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [a], [a], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [a], [a], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [a], [a], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [a], [a], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [a], [a], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [a], [a], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a], [a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [a], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a], [a], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a], [a], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a], [a], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a], [a], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a], [a], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a], [a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a], [a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a], [a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b], [b], [b], [b]]).
%% mrule([[a, b], [a], [b], [b], [b], [a, b]]).
%% mrule([[a], [a], [b], [b], [b], [b, a]]).
%% mrule([[a, b], [a], [b], [b], [b], [b, a]]).
%% mrule([[a, b], [a], [b], [b], [c], [a, c]]).
%% mrule([[a, b], [a], [b], [b], [c], [b, c]]).
%% mrule([[a, b], [a], [b], [b], [c], [c, a]]).
%% mrule([[a, b], [a], [b], [b], [c], [c, b]]).
%% mrule([[a, b], [a], [b], [b], [a, b], [a, b]]).
%% mrule([[a, b], [a], [b], [b], [a, b], [b, a]]).
%% mrule([[a, b], [a], [b], [b], [a, c], [a, c]]).
%% mrule([[a], [a], [b], [b], [a, c], [b, c]]).
%% mrule([[a, b], [a], [b], [b], [a, c], [b, c]]).
%% mrule([[a, b], [a], [b], [b], [a, c], [c, a]]).
%% mrule([[a], [a], [b], [b], [a, c], [c, b]]).
%% mrule([[a, b], [a], [b], [b], [a, c], [c, b]]).
%% mrule([[a], [a], [b], [b], [b, a], [b, a]]).
%% mrule([[a, b], [a], [b], [b], [b, a], [b, a]]).
%% mrule([[a, b], [a], [b], [b], [b, c], [b, c]]).
%% mrule([[a], [a], [b], [b], [b, c], [c, a]]).
%% mrule([[a, b], [a], [b], [b], [b, c], [c, a]]).
%% mrule([[a, b], [a], [b], [b], [b, c], [c, b]]).
%% mrule([[a, b], [a], [b], [b], [c, a], [c, a]]).
%% mrule([[a], [a], [b], [b], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b], [b], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b], [b], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b], [c], [c], [a, c]]).
%% mrule([[a, b], [a], [b], [c], [c], [b, c]]).
%% mrule([[a, b], [a], [b], [c], [c], [c, a]]).
%% mrule([[a, b], [a], [b], [c], [c], [c, b]]).
%% mrule([[a, b], [a], [b], [c], [a, b], [a, c]]).
%% mrule([[a, b], [a], [b], [c], [a, b], [b, c]]).
%% mrule([[a, b], [a], [b], [c], [a, b], [c, a]]).
%% mrule([[a, b], [a], [b], [c], [a, b], [c, b]]).
%% mrule([[a, b], [a], [b], [c], [a, c], [a, c]]).
%% mrule([[a], [a], [b], [c], [a, c], [b, a]]).
%% mrule([[a, b], [a], [b], [c], [a, c], [b, a]]).
%% mrule([[a], [a], [b], [c], [a, c], [b, c]]).
%% mrule([[a, b], [a], [b], [c], [a, c], [b, c]]).
%% mrule([[a, b], [a], [b], [c], [a, c], [c, a]]).
%% mrule([[a], [a], [b], [c], [a, c], [c, b]]).
%% mrule([[a, b], [a], [b], [c], [a, c], [c, b]]).
%% mrule([[a, b], [a], [b], [c], [a, d], [c, d]]).
%% mrule([[a, b], [a], [b], [c], [a, d], [d, c]]).
%% mrule([[a], [a], [b], [c], [b, a], [b, c]]).
%% mrule([[a, b], [a], [b], [c], [b, a], [b, c]]).
%% mrule([[a], [a], [b], [c], [b, a], [c, a]]).
%% mrule([[a, b], [a], [b], [c], [b, a], [c, a]]).
%% mrule([[a], [a], [b], [c], [b, a], [c, b]]).
%% mrule([[a, b], [a], [b], [c], [b, a], [c, b]]).
%% mrule([[a, b], [a], [b], [c], [b, c], [b, c]]).
%% mrule([[a], [a], [b], [c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [b], [c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [b], [c], [b, c], [c, b]]).
%% mrule([[a, b], [a], [b], [c], [b, d], [c, d]]).
%% mrule([[a, b], [a], [b], [c], [b, d], [d, c]]).
%% mrule([[a, b], [a], [b], [c], [c, a], [c, a]]).
%% mrule([[a], [a], [b], [c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b], [c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b], [c], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b], [c], [c, d], [d, a]]).
%% mrule([[a, b], [a], [b], [c], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b], [c], [d, a], [d, c]]).
%% mrule([[a, b], [a], [b], [c], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b], [a, b], [a, b], [a, b]]).
%% mrule([[a, b], [a], [b], [a, b], [a, b], [b, a]]).
%% mrule([[a, b], [a], [b], [a, b], [a, c], [a, c]]).
%% mrule([[a, b], [a], [b], [a, b], [a, c], [b, c]]).
%% mrule([[a, b], [a], [b], [a, b], [a, c], [c, a]]).
%% mrule([[a, b], [a], [b], [a, b], [a, c], [c, b]]).
%% mrule([[a, b], [a], [b], [a, b], [b, a], [b, a]]).
%% mrule([[a, b], [a], [b], [a, b], [b, c], [b, c]]).
%% mrule([[a, b], [a], [b], [a, b], [b, c], [c, a]]).
%% mrule([[a, b], [a], [b], [a, b], [b, c], [c, b]]).
%% mrule([[a, b], [a], [b], [a, b], [c, a], [c, a]]).
%% mrule([[a, b], [a], [b], [a, b], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b], [a, b], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b], [a, c], [a, c], [a, c]]).
%% mrule([[a], [a], [b], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [a], [b], [a, c], [a, c], [b, a]]).
%% mrule([[a], [a], [b], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [a], [b], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [a], [b], [a, c], [a, c], [c, a]]).
%% mrule([[a], [a], [b], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [a], [b], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [a], [b], [a, c], [a, d], [c, d]]).
%% mrule([[a, b], [a], [b], [a, c], [a, d], [d, c]]).
%% mrule([[a], [a], [b], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [a], [b], [a, c], [b, a], [b, c]]).
%% mrule([[a], [a], [b], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [a], [b], [a, c], [b, a], [c, a]]).
%% mrule([[a], [a], [b], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [a], [b], [a, c], [b, a], [c, b]]).
%% mrule([[a], [a], [b], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [a], [b], [a, c], [b, c], [b, c]]).
%% mrule([[a], [a], [b], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [b], [a, c], [b, c], [c, a]]).
%% mrule([[a], [a], [b], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [a], [b], [a, c], [b, c], [c, b]]).
%% mrule([[a], [a], [b], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [a], [b], [a, c], [b, d], [c, d]]).
%% mrule([[a], [a], [b], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [a], [b], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [a], [b], [a, c], [c, a], [c, a]]).
%% mrule([[a], [a], [b], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b], [a, c], [c, a], [c, b]]).
%% mrule([[a], [a], [b], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b], [a, c], [c, d], [c, d]]).
%% mrule([[a, b], [a], [b], [a, c], [c, d], [d, a]]).
%% mrule([[a], [a], [b], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b], [a, c], [c, d], [d, c]]).
%% mrule([[a, b], [a], [b], [a, c], [d, a], [d, c]]).
%% mrule([[a], [a], [b], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b], [a, c], [d, c], [d, c]]).
%% mrule([[a], [a], [b], [b, a], [b, a], [b, a]]).
%% mrule([[a, b], [a], [b], [b, a], [b, a], [b, a]]).
%% mrule([[a], [a], [b], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [a], [b], [b, a], [b, c], [b, c]]).
%% mrule([[a], [a], [b], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [a], [b], [b, a], [b, c], [c, a]]).
%% mrule([[a], [a], [b], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [a], [b], [b, a], [b, c], [c, b]]).
%% mrule([[a], [a], [b], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [a], [b], [b, a], [c, a], [c, a]]).
%% mrule([[a], [a], [b], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b], [b, a], [c, a], [c, b]]).
%% mrule([[a], [a], [b], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b], [b, c], [b, c], [b, c]]).
%% mrule([[a], [a], [b], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [b], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [b], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [a], [b], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [a], [b], [b, c], [b, d], [d, c]]).
%% mrule([[a], [a], [b], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [a], [b], [b, c], [c, a], [c, a]]).
%% mrule([[a], [a], [b], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b], [b, c], [c, d], [c, d]]).
%% mrule([[a], [a], [b], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [a], [b], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [a], [b], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b], [b, c], [c, d], [d, c]]).
%% mrule([[a], [a], [b], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [a], [b], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [a], [b], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [a], [b], [c, a], [c, a], [c, a]]).
%% mrule([[a], [a], [b], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b], [c, a], [c, a], [c, b]]).
%% mrule([[a], [a], [b], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [a], [b], [c, a], [c, d], [d, a]]).
%% mrule([[a], [a], [b], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [a], [b], [c, a], [d, a], [d, c]]).
%% mrule([[a], [a], [b], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [a], [b], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b], [c, b], [c, d], [c, d]]).
%% mrule([[a], [a], [b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a], [b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a], [b], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b], [c, b], [c, d], [d, c]]).
%% mrule([[a], [a], [b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a], [b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a], [b], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a], [b], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [a], [b], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [a], [b], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c], [c], [c], [b, c]]).
%% mrule([[a, b], [a], [c], [c], [c], [c, b]]).
%% mrule([[a, b], [a], [c], [c], [a, b], [a, c]]).
%% mrule([[a, b], [a], [c], [c], [a, b], [b, c]]).
%% mrule([[a, b], [a], [c], [c], [a, b], [c, a]]).
%% mrule([[a, b], [a], [c], [c], [a, b], [c, b]]).
%% mrule([[a, b], [a], [c], [c], [a, c], [b, a]]).
%% mrule([[a, b], [a], [c], [c], [a, c], [b, c]]).
%% mrule([[a, b], [a], [c], [c], [a, c], [c, b]]).
%% mrule([[a, b], [a], [c], [c], [b, a], [b, c]]).
%% mrule([[a, b], [a], [c], [c], [b, a], [c, a]]).
%% mrule([[a, b], [a], [c], [c], [b, a], [c, b]]).
%% mrule([[a, b], [a], [c], [c], [b, c], [b, c]]).
%% mrule([[a, b], [a], [c], [c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [c], [c], [b, c], [c, b]]).
%% mrule([[a, b], [a], [c], [c], [b, d], [c, d]]).
%% mrule([[a, b], [a], [c], [c], [b, d], [d, c]]).
%% mrule([[a, b], [a], [c], [c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [c], [c], [c, b], [c, b]]).
%% mrule([[a, b], [a], [c], [c], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c], [c], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c], [d], [a, c], [b, d]]).
%% mrule([[a, b], [a], [c], [d], [a, c], [d, b]]).
%% mrule([[a, b], [a], [c], [d], [a, d], [b, c]]).
%% mrule([[a, b], [a], [c], [d], [a, d], [c, b]]).
%% mrule([[a, b], [a], [c], [d], [b, c], [b, d]]).
%% mrule([[a, b], [a], [c], [d], [b, c], [c, d]]).
%% mrule([[a, b], [a], [c], [d], [b, c], [d, a]]).
%% mrule([[a, b], [a], [c], [d], [b, c], [d, b]]).
%% mrule([[a, b], [a], [c], [d], [b, c], [d, c]]).
%% mrule([[a, b], [a], [c], [d], [b, d], [c, a]]).
%% mrule([[a, b], [a], [c], [d], [b, d], [c, b]]).
%% mrule([[a, b], [a], [c], [d], [b, d], [c, d]]).
%% mrule([[a, b], [a], [c], [d], [b, d], [d, c]]).
%% mrule([[a, b], [a], [c], [d], [c, a], [d, b]]).
%% mrule([[a, b], [a], [c], [d], [c, b], [c, d]]).
%% mrule([[a, b], [a], [c], [d], [c, b], [d, a]]).
%% mrule([[a, b], [a], [c], [d], [c, b], [d, b]]).
%% mrule([[a, b], [a], [c], [d], [c, b], [d, c]]).
%% mrule([[a, b], [a], [c], [d], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c], [d], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c], [a, b], [a, b], [a, c]]).
%% mrule([[a, b], [a], [c], [a, b], [a, b], [b, c]]).
%% mrule([[a, b], [a], [c], [a, b], [a, b], [c, a]]).
%% mrule([[a, b], [a], [c], [a, b], [a, b], [c, b]]).
%% mrule([[a, b], [a], [c], [a, b], [a, c], [a, c]]).
%% mrule([[a, b], [a], [c], [a, b], [a, c], [b, a]]).
%% mrule([[a, b], [a], [c], [a, b], [a, c], [b, c]]).
%% mrule([[a, b], [a], [c], [a, b], [a, c], [c, a]]).
%% mrule([[a, b], [a], [c], [a, b], [a, c], [c, b]]).
%% mrule([[a, b], [a], [c], [a, b], [a, d], [c, d]]).
%% mrule([[a, b], [a], [c], [a, b], [a, d], [d, c]]).
%% mrule([[a, b], [a], [c], [a, b], [b, a], [b, c]]).
%% mrule([[a, b], [a], [c], [a, b], [b, a], [c, a]]).
%% mrule([[a, b], [a], [c], [a, b], [b, a], [c, b]]).
%% mrule([[a, b], [a], [c], [a, b], [b, c], [b, c]]).
%% mrule([[a, b], [a], [c], [a, b], [b, c], [c, a]]).
%% mrule([[a, b], [a], [c], [a, b], [b, c], [c, b]]).
%% mrule([[a, b], [a], [c], [a, b], [b, d], [c, d]]).
%% mrule([[a, b], [a], [c], [a, b], [b, d], [d, c]]).
%% mrule([[a, b], [a], [c], [a, b], [c, a], [c, a]]).
%% mrule([[a, b], [a], [c], [a, b], [c, a], [c, b]]).
%% mrule([[a, b], [a], [c], [a, b], [c, b], [c, b]]).
%% mrule([[a, b], [a], [c], [a, b], [c, d], [d, a]]).
%% mrule([[a, b], [a], [c], [a, b], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c], [a, b], [d, a], [d, c]]).
%% mrule([[a, b], [a], [c], [a, b], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [a], [c], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [a], [c], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [a], [c], [a, c], [a, d], [b, d]]).
%% mrule([[a, b], [a], [c], [a, c], [a, d], [d, b]]).
%% mrule([[a, b], [a], [c], [a, c], [b, a], [b, a]]).
%% mrule([[a, b], [a], [c], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [a], [c], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [a], [c], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [a], [c], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [a], [c], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [c], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [a], [c], [a, c], [b, d], [b, d]]).
%% mrule([[a, b], [a], [c], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [a], [c], [a, c], [b, d], [d, a]]).
%% mrule([[a, b], [a], [c], [a, c], [b, d], [d, b]]).
%% mrule([[a, b], [a], [c], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [a], [c], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [c], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [a], [c], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c], [a, c], [d, a], [d, b]]).
%% mrule([[a, b], [a], [c], [a, c], [d, b], [d, b]]).
%% mrule([[a, b], [a], [c], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c], [a, d], [a, d], [b, c]]).
%% mrule([[a, b], [a], [c], [a, d], [a, d], [c, b]]).
%% mrule([[a, b], [a], [c], [a, d], [b, a], [c, d]]).
%% mrule([[a, b], [a], [c], [a, d], [b, a], [d, c]]).
%% mrule([[a, b], [a], [c], [a, d], [b, c], [b, d]]).
%% mrule([[a, b], [a], [c], [a, d], [b, c], [c, d]]).
%% mrule([[a, b], [a], [c], [a, d], [b, c], [d, a]]).
%% mrule([[a, b], [a], [c], [a, d], [b, c], [d, b]]).
%% mrule([[a, b], [a], [c], [a, d], [b, c], [d, c]]).
%% mrule([[a, b], [a], [c], [a, d], [b, d], [c, a]]).
%% mrule([[a, b], [a], [c], [a, d], [b, d], [c, b]]).
%% mrule([[a, b], [a], [c], [a, d], [b, d], [c, d]]).
%% mrule([[a, b], [a], [c], [a, d], [b, d], [d, c]]).
%% mrule([[a, b], [a], [c], [a, d], [c, a], [d, b]]).
%% mrule([[a, b], [a], [c], [a, d], [c, b], [c, d]]).
%% mrule([[a, b], [a], [c], [a, d], [c, b], [d, a]]).
%% mrule([[a, b], [a], [c], [a, d], [c, b], [d, b]]).
%% mrule([[a, b], [a], [c], [a, d], [c, b], [d, c]]).
%% mrule([[a, b], [a], [c], [a, d], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c], [a, d], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c], [b, a], [b, a], [b, c]]).
%% mrule([[a, b], [a], [c], [b, a], [b, a], [c, a]]).
%% mrule([[a, b], [a], [c], [b, a], [b, a], [c, b]]).
%% mrule([[a, b], [a], [c], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [a], [c], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [a], [c], [b, a], [b, c], [c, b]]).
%% mrule([[a], [a], [b], [c, a], [c, d], [b, d]]).
%% mrule([[a, b], [a], [c], [b, a], [b, d], [c, d]]).
%% mrule([[a, b], [a], [c], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [a], [c], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [a], [c], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [a], [c], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [c], [b, a], [c, d], [d, a]]).
%% mrule([[a], [a], [b], [c, a], [b, d], [d, c]]).
%% mrule([[a, b], [a], [c], [b, a], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c], [b, a], [d, a], [d, c]]).
%% mrule([[a, b], [a], [c], [b, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [a], [c], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [c], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [a], [c], [b, c], [b, d], [b, d]]).
%% mrule([[a, b], [a], [c], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [a], [c], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [a], [c], [b, c], [b, d], [d, b]]).
%% mrule([[a, b], [a], [c], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [a], [c], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [a], [c], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [c], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [a], [c], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [a], [c], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [a], [c], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [a], [c], [b, c], [d, a], [d, a]]).
%% mrule([[a, b], [a], [c], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [a], [c], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [a], [c], [b, c], [d, b], [d, b]]).
%% mrule([[a, b], [a], [c], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [a], [c], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [a], [c], [b, d], [b, d], [c, b]]).
%% mrule([[a, b], [a], [c], [b, d], [b, d], [c, d]]).
%% mrule([[a, b], [a], [c], [b, d], [b, d], [d, c]]).
%% mrule([[a, b], [a], [c], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [a], [c], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [a], [c], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [a], [c], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [a], [c], [b, d], [c, b], [c, d]]).
%% mrule([[a, b], [a], [c], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [a], [c], [b, d], [c, b], [d, b]]).
%% mrule([[a, b], [a], [c], [b, d], [c, b], [d, c]]).
%% mrule([[a, b], [a], [c], [b, d], [c, d], [c, d]]).
%% mrule([[a, b], [a], [c], [b, d], [c, d], [d, a]]).
%% mrule([[a, b], [a], [c], [b, d], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c], [b, d], [c, d], [d, c]]).
%% mrule([[a, b], [a], [c], [b, d], [c, e], [d, e]]).
%% mrule([[a, b], [a], [c], [b, d], [c, e], [e, d]]).
%% mrule([[a, b], [a], [c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [a], [c], [b, d], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [a], [c], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [a], [c], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [a], [c], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [a], [c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [a], [c], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [a], [c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a], [c], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a], [c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a], [c], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a], [c], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [a], [c], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [a], [c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a], [c], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [a], [c], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a], [c], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a], [c], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [a], [c], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [a], [c], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [a], [c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a], [c], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a], [c], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a], [c], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a], [c], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a], [c], [d, b], [d, e], [c, e]]).
%% mrule([[a, b], [a], [c], [d, b], [c, e], [e, d]]).
%% mrule([[a, b], [a], [a, b], [a, b], [a, b], [a, b]]).
%% mrule([[a, b], [a], [a, b], [a, b], [a, b], [b, a]]).
%% mrule([[a, b], [a], [a, b], [a, b], [a, c], [a, c]]).
%% mrule([[a, b], [a], [a, b], [a, b], [a, c], [b, c]]).
%% mrule([[a, b], [a], [a, b], [a, b], [a, c], [c, a]]).
%% mrule([[a, b], [a], [a, b], [a, b], [a, c], [c, b]]).
%% mrule([[a, b], [a], [a, b], [a, b], [b, a], [b, a]]).
%% mrule([[a, b], [a], [a, b], [a, b], [b, c], [b, c]]).
%% mrule([[a, b], [a], [a, b], [a, b], [b, c], [c, a]]).
%% mrule([[a, b], [a], [a, b], [a, b], [b, c], [c, b]]).
%% mrule([[a, b], [a], [a, b], [a, b], [c, a], [c, a]]).
%% mrule([[a, b], [a], [a, b], [a, b], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a, b], [a, b], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a, b], [a, c], [a, c], [a, c]]).
%% mrule([[a, b], [a], [a, b], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [a], [a, b], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [a], [a, b], [a, c], [a, c], [c, a]]).
%% mrule([[a, b], [a], [a, b], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [a], [a, b], [a, c], [a, d], [c, d]]).
%% mrule([[a, b], [a], [a, b], [a, c], [a, d], [d, c]]).
%% mrule([[a, b], [a], [a, b], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [a], [a, b], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [a], [a, b], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [a], [a, b], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [a], [a, b], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [a, b], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [a], [a, b], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [a], [a, b], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [a], [a, b], [a, c], [c, a], [c, a]]).
%% mrule([[a, b], [a], [a, b], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a, b], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a, b], [a, c], [c, d], [c, d]]).
%% mrule([[a, b], [a], [a, b], [a, c], [c, d], [d, a]]).
%% mrule([[a, b], [a], [a, b], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [a], [a, b], [a, c], [c, d], [d, c]]).
%% mrule([[a, b], [a], [a, b], [a, c], [d, a], [d, c]]).
%% mrule([[a, b], [a], [a, b], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [a], [a, b], [a, c], [d, c], [d, c]]).
%% mrule([[a, b], [a], [a, b], [b, a], [b, a], [b, a]]).
%% mrule([[a, b], [a], [a, b], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [a], [a, b], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [a], [a, b], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [a], [a, b], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [a], [a, b], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a, b], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a, b], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [a], [a, b], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [a, b], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [a], [a, b], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [a], [a, b], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [a], [a, b], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [a], [a, b], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a, b], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a, b], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [a], [a, b], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [a], [a, b], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [a], [a, b], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [a], [a, b], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [a], [a, b], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [a], [a, b], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [a], [a, b], [c, a], [c, a], [c, a]]).
%% mrule([[a, b], [a], [a, b], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a, b], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a, b], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [a], [a, b], [c, a], [c, d], [d, a]]).
%% mrule([[a, b], [a], [a, b], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a], [a, b], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [a], [a, b], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [a], [a, b], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [a, b], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [a], [a, b], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a, b], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a], [a, b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a], [a, b], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a], [a, b], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a], [a, b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a], [a, b], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a], [a, b], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a], [a, b], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [a], [a, b], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a], [a, b], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [a], [a, b], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a], [a, c], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [a], [a, c], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [a], [a, c], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [a], [a, c], [a, c], [a, d], [b, d]]).
%% mrule([[a, b], [a], [a, c], [a, c], [a, d], [d, b]]).
%% mrule([[a, b], [a], [a, c], [a, c], [b, a], [b, a]]).
%% mrule([[a, b], [a], [a, c], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [a], [a, c], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [a], [a, c], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [a], [a, c], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [a], [a, c], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [a, c], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [a], [a, c], [a, c], [b, d], [b, d]]).
%% mrule([[a, b], [a], [a, c], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [a], [a, c], [a, c], [b, d], [d, a]]).
%% mrule([[a, b], [a], [a, c], [a, c], [b, d], [d, b]]).
%% mrule([[a, b], [a], [a, c], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [a], [a, c], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a, c], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a, c], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [a], [a, c], [a, c], [d, a], [d, b]]).
%% mrule([[a, b], [a], [a, c], [a, c], [d, b], [d, b]]).
%% mrule([[a, b], [a], [a, c], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [a], [a, c], [a, d], [a, d], [b, c]]).
%% mrule([[a, b], [a], [a, c], [a, d], [a, d], [c, b]]).
%% mrule([[a, b], [a], [a, c], [a, d], [b, a], [c, d]]).
%% mrule([[a, b], [a], [a, c], [a, d], [b, a], [d, c]]).
%% mrule([[a, b], [a], [a, c], [a, d], [b, c], [b, d]]).
%% mrule([[a, b], [a], [a, c], [a, d], [b, c], [c, d]]).
%% mrule([[a, b], [a], [a, c], [a, d], [b, c], [d, a]]).
%% mrule([[a, b], [a], [a, c], [a, d], [b, c], [d, b]]).
%% mrule([[a, b], [a], [a, c], [a, d], [b, c], [d, c]]).
%% mrule([[a, b], [a], [a, c], [a, d], [b, d], [c, a]]).
%% mrule([[a, b], [a], [a, c], [a, d], [b, d], [c, b]]).
%% mrule([[a, b], [a], [a, c], [a, d], [b, d], [c, d]]).
%% mrule([[a, b], [a], [a, c], [a, d], [b, d], [d, c]]).
%% mrule([[a, b], [a], [a, c], [a, d], [c, a], [d, b]]).
%% mrule([[a, b], [a], [a, c], [a, d], [c, b], [c, d]]).
%% mrule([[a, b], [a], [a, c], [a, d], [c, b], [d, a]]).
%% mrule([[a, b], [a], [a, c], [a, d], [c, b], [d, b]]).
%% mrule([[a, b], [a], [a, c], [a, d], [c, b], [d, c]]).
%% mrule([[a, b], [a], [a, c], [a, d], [c, d], [d, b]]).
%% mrule([[a, b], [a], [a, c], [a, d], [d, b], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, a], [b, a], [b, c]]).
%% mrule([[a, b], [a], [a, c], [b, a], [b, a], [c, a]]).
%% mrule([[a, b], [a], [a, c], [b, a], [b, a], [c, b]]).
%% mrule([[a, b], [a], [a, c], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [a], [a, c], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [a], [a, c], [b, a], [b, c], [c, b]]).
%% mrule([[a], [a], [a, b], [c, a], [c, d], [b, d]]).
%% mrule([[a, b], [a], [a, c], [b, a], [b, d], [c, d]]).
%% mrule([[a, b], [a], [a, c], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [a], [a, c], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a, c], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a, c], [b, a], [c, d], [c, d]]).
%% mrule([[a, b], [a], [a, c], [b, a], [c, d], [d, a]]).
%% mrule([[a], [a], [a, b], [c, a], [b, d], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, a], [c, d], [d, b]]).
%% mrule([[a, b], [a], [a, c], [b, a], [c, d], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, a], [d, a], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, a], [d, c], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [a], [a, c], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [a, c], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [a], [a, c], [b, c], [b, d], [b, d]]).
%% mrule([[a], [a], [a, b], [c, b], [c, d], [b, d]]).
%% mrule([[a, b], [a], [a, c], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [a], [a, c], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [a], [a, c], [b, c], [b, d], [d, b]]).
%% mrule([[a, b], [a], [a, c], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [a], [a, c], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a, c], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a, c], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [a], [a, c], [b, c], [c, d], [d, a]]).
%% mrule([[a], [a], [a, b], [c, b], [b, d], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [a], [a, c], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, c], [d, a], [d, a]]).
%% mrule([[a, b], [a], [a, c], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [a], [a, c], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, c], [d, b], [d, b]]).
%% mrule([[a, b], [a], [a, c], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [a], [a, c], [b, d], [b, d], [c, b]]).
%% mrule([[a], [a], [a, b], [c, d], [c, d], [b, d]]).
%% mrule([[a, b], [a], [a, c], [b, d], [b, d], [c, d]]).
%% mrule([[a, b], [a], [a, c], [b, d], [b, d], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [a], [a, c], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [a], [a, c], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [a], [a, c], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, d], [c, b], [c, d]]).
%% mrule([[a, b], [a], [a, c], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [a], [a, c], [b, d], [c, b], [d, b]]).
%% mrule([[a, b], [a], [a, c], [b, d], [c, b], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, d], [c, d], [c, d]]).
%% mrule([[a, b], [a], [a, c], [b, d], [c, d], [d, a]]).
%% mrule([[a], [a], [a, b], [c, d], [b, d], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, d], [c, d], [d, b]]).
%% mrule([[a, b], [a], [a, c], [b, d], [c, d], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, d], [c, e], [d, e]]).
%% mrule([[a, b], [a], [a, c], [b, d], [c, e], [e, d]]).
%% mrule([[a, b], [a], [a, c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, d], [d, b], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [a], [a, c], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [a], [a, c], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [a], [a, c], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [a], [a, c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a, c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a], [a, c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [a], [a, c], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [a], [a, c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [a, c], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a], [a, c], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a], [a, c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a], [a, c], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a], [a, c], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a], [a, c], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [a], [a, c], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [a], [a, c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a], [a, c], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [a], [a, c], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a], [a, c], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a], [a, c], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a], [a, c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a], [a, c], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [a], [a, c], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a], [a, c], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [a], [a, c], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [a], [a, c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [a, c], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a], [a, c], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a], [a, c], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a], [a, c], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a], [a, c], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a], [a, c], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a], [a, c], [d, b], [d, e], [c, e]]).
%% mrule([[a, b], [a], [a, c], [d, b], [c, e], [e, d]]).
%% mrule([[a], [a], [b, a], [b, a], [b, a], [b, a]]).
%% mrule([[a, b], [a], [b, a], [b, a], [b, a], [b, a]]).
%% mrule([[a], [a], [b, a], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [a], [b, a], [b, a], [b, c], [b, c]]).
%% mrule([[a], [a], [b, a], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [a], [b, a], [b, a], [b, c], [c, a]]).
%% mrule([[a], [a], [b, a], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [a], [b, a], [b, a], [b, c], [c, b]]).
%% mrule([[a], [a], [b, a], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [a], [b, a], [b, a], [c, a], [c, a]]).
%% mrule([[a], [a], [b, a], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b, a], [b, a], [c, a], [c, b]]).
%% mrule([[a], [a], [b, a], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b, a], [b, a], [c, b], [c, b]]).
%% mrule([[a], [a], [b, a], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [a], [b, a], [b, c], [b, c], [b, c]]).
%% mrule([[a], [a], [b, a], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [b, a], [b, c], [b, c], [c, a]]).
%% mrule([[a], [a], [b, a], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [a], [b, a], [b, c], [b, c], [c, b]]).
%% mrule([[a], [a], [b, a], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [a], [b, a], [b, c], [b, d], [c, d]]).
%% mrule([[a], [a], [b, a], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [a], [b, a], [b, c], [b, d], [d, c]]).
%% mrule([[a], [a], [b, a], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [a], [b, a], [b, c], [c, a], [c, a]]).
%% mrule([[a], [a], [b, a], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b, a], [b, c], [c, a], [c, b]]).
%% mrule([[a], [a], [b, a], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b, a], [b, c], [c, b], [c, b]]).
%% mrule([[a], [a], [b, a], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [a], [b, a], [b, c], [c, d], [c, d]]).
%% mrule([[a], [a], [b, a], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [a], [b, a], [b, c], [c, d], [d, a]]).
%% mrule([[a], [a], [b, a], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b, a], [b, c], [c, d], [d, b]]).
%% mrule([[a], [a], [b, a], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [a], [b, a], [b, c], [c, d], [d, c]]).
%% mrule([[a], [a], [b, a], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [a], [b, a], [b, c], [d, a], [d, c]]).
%% mrule([[a], [a], [b, a], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b, a], [b, c], [d, b], [d, c]]).
%% mrule([[a], [a], [b, a], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [a], [b, a], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [a], [b, a], [c, a], [c, a], [c, a]]).
%% mrule([[a], [a], [b, a], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b, a], [c, a], [c, a], [c, b]]).
%% mrule([[a], [a], [b, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b, a], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [a], [b, a], [c, a], [c, d], [d, a]]).
%% mrule([[a], [a], [b, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b, a], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [a], [b, a], [c, a], [d, a], [d, c]]).
%% mrule([[a], [a], [b, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b, a], [c, a], [d, c], [d, c]]).
%% mrule([[a], [a], [b, a], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b, a], [c, b], [c, b], [c, b]]).
%% mrule([[a], [a], [b, a], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a], [b, a], [c, b], [c, d], [c, d]]).
%% mrule([[a], [a], [b, a], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a], [b, a], [c, b], [c, d], [d, a]]).
%% mrule([[a], [a], [b, a], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b, a], [c, b], [c, d], [d, b]]).
%% mrule([[a], [a], [b, a], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a], [b, a], [c, b], [c, d], [d, c]]).
%% mrule([[a], [a], [b, a], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a], [b, a], [c, b], [d, a], [d, c]]).
%% mrule([[a], [a], [b, a], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b, a], [c, b], [d, b], [d, c]]).
%% mrule([[a], [a], [b, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a], [b, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a], [b, a], [c, d], [c, d], [d, a]]).
%% mrule([[a], [a], [b, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b, a], [c, d], [d, a], [d, c]]).
%% mrule([[a], [a], [b, a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b, a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b, c], [b, c], [b, c], [b, c]]).
%% mrule([[a], [a], [b, c], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [b, c], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a], [b, c], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [a], [b, c], [b, c], [b, d], [b, d]]).
%% mrule([[a, b], [a], [b, c], [b, c], [b, d], [c, d]]).
%% mrule([[a], [a], [b, c], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [a], [b, c], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [a], [b, c], [b, c], [b, d], [d, b]]).
%% mrule([[a, b], [a], [b, c], [b, c], [b, d], [d, c]]).
%% mrule([[a], [a], [b, c], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [a], [b, c], [b, c], [c, a], [c, a]]).
%% mrule([[a], [a], [b, c], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b, c], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b, c], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b, c], [b, c], [c, d], [c, d]]).
%% mrule([[a], [a], [b, c], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [a], [b, c], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [a], [b, c], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b, c], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [a], [b, c], [b, c], [d, a], [d, a]]).
%% mrule([[a], [a], [b, c], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [a], [b, c], [b, c], [d, a], [d, b]]).
%% mrule([[a], [a], [b, c], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [a], [b, c], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [a], [b, c], [b, c], [d, b], [d, b]]).
%% mrule([[a, b], [a], [b, c], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b, c], [b, c], [d, c], [d, c]]).
%% mrule([[a], [a], [b, c], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [a], [b, c], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [a], [b, c], [b, d], [b, d], [c, b]]).
%% mrule([[a, b], [a], [b, c], [b, d], [b, d], [c, d]]).
%% mrule([[a, b], [a], [b, c], [b, d], [b, d], [d, c]]).
%% mrule([[a], [a], [b, c], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [a], [b, c], [b, d], [c, a], [c, d]]).
%% mrule([[a], [a], [b, c], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [a], [b, c], [b, d], [c, a], [d, a]]).
%% mrule([[a], [a], [b, c], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [a], [b, c], [b, d], [c, a], [d, b]]).
%% mrule([[a], [a], [b, c], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [a], [b, c], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [a], [b, c], [b, d], [c, b], [c, d]]).
%% mrule([[a], [a], [b, c], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [a], [b, c], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [a], [b, c], [b, d], [c, b], [d, b]]).
%% mrule([[a, b], [a], [b, c], [b, d], [c, b], [d, c]]).
%% mrule([[a, b], [a], [b, c], [b, d], [c, d], [c, d]]).
%% mrule([[a], [a], [b, c], [b, d], [c, d], [d, a]]).
%% mrule([[a, b], [a], [b, c], [b, d], [c, d], [d, a]]).
%% mrule([[a, b], [a], [b, c], [b, d], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b, c], [b, d], [c, d], [d, c]]).
%% mrule([[a, b], [a], [b, c], [b, d], [c, e], [d, e]]).
%% mrule([[a, b], [a], [b, c], [b, d], [c, e], [e, d]]).
%% mrule([[a], [a], [b, c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [a], [b, c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [a], [b, c], [b, d], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b, c], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [a], [b, c], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [a], [b, c], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [a], [b, c], [c, a], [c, a], [c, a]]).
%% mrule([[a], [a], [b, c], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [a], [b, c], [c, a], [c, a], [c, b]]).
%% mrule([[a], [a], [b, c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b, c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b, c], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [a], [b, c], [c, a], [c, d], [d, a]]).
%% mrule([[a], [a], [b, c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b, c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b, c], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [a], [b, c], [c, a], [d, a], [d, a]]).
%% mrule([[a], [a], [b, c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [a], [b, c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [a], [b, c], [c, a], [d, a], [d, c]]).
%% mrule([[a], [a], [b, c], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [a], [b, c], [c, a], [d, b], [d, b]]).
%% mrule([[a], [a], [b, c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b, c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b, c], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [a], [b, c], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a], [b, c], [c, b], [c, d], [c, d]]).
%% mrule([[a], [a], [b, c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a], [b, c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a], [b, c], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b, c], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a], [b, c], [c, b], [d, a], [d, a]]).
%% mrule([[a], [a], [b, c], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [a], [b, c], [c, b], [d, a], [d, b]]).
%% mrule([[a], [a], [b, c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a], [b, c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a], [b, c], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [a], [b, c], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b, c], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a], [b, c], [c, d], [c, d], [c, d]]).
%% mrule([[a, b], [a], [b, c], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [a], [b, c], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a], [b, c], [c, d], [c, d], [d, c]]).
%% mrule([[a, b], [a], [b, c], [c, d], [c, e], [d, e]]).
%% mrule([[a, b], [a], [b, c], [c, d], [c, e], [e, d]]).
%% mrule([[a, b], [a], [b, c], [c, d], [d, a], [d, a]]).
%% mrule([[a], [a], [b, c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a], [b, c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a], [b, c], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [a], [b, c], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [a], [b, c], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b, c], [c, d], [d, c], [d, c]]).
%% mrule([[a, b], [a], [b, c], [c, d], [d, e], [d, e]]).
%% mrule([[a, b], [a], [b, c], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [a], [b, c], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [a], [b, c], [c, d], [d, e], [e, c]]).
%% mrule([[a, b], [a], [b, c], [c, d], [d, e], [e, d]]).
%% mrule([[a, b], [a], [b, c], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [a], [b, c], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [a], [b, c], [c, d], [e, c], [e, d]]).
%% mrule([[a, b], [a], [b, c], [c, d], [e, d], [e, d]]).
%% mrule([[a, b], [a], [b, c], [d, a], [d, a], [d, c]]).
%% mrule([[a], [a], [b, c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b, c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b, c], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [a], [b, c], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [a], [b, c], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [a], [b, c], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a], [b, c], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a], [b, c], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a], [b, c], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a], [b, c], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [a], [b, c], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [a], [b, c], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [a], [b, c], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a], [b, c], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [a], [b, c], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [a], [b, c], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [a], [b, c], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a], [b, c], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [a], [b, c], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [a], [b, c], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [a], [b, c], [d, e], [e, c], [e, d]]).
%% mrule([[a, b], [a], [b, c], [d, a], [d, e], [c, e]]).
%% mrule([[a, b], [a], [b, c], [d, a], [c, e], [e, d]]).
%% mrule([[a, b], [a], [b, c], [d, b], [d, e], [c, e]]).
%% mrule([[a, b], [a], [b, c], [d, b], [c, e], [e, d]]).
%% mrule([[a, b], [a], [b, c], [d, c], [d, e], [c, e]]).
%% mrule([[a, b], [a], [b, c], [d, c], [c, e], [e, d]]).
%% mrule([[a, b], [a], [b, c], [d, e], [d, e], [c, e]]).
%% mrule([[a, b], [a], [b, c], [d, e], [c, e], [e, d]]).
%% mrule([[a, b], [a], [c, a], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [a], [c, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a], [c, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c, a], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [a], [c, a], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [a], [c, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c, a], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a], [c, a], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a], [c, a], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a], [c, a], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c, a], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a], [c, a], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [a], [c, a], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [a], [c, a], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a], [c, a], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [a], [c, a], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a], [c, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c, a], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a], [c, a], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [a], [c, a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c, a], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [a], [c, a], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [a], [c, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c, a], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c, a], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a], [c, a], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a], [c, a], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a], [c, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a], [c, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a], [c, b], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a], [c, b], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a], [c, b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a], [c, b], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c, b], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a], [c, b], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [a], [c, b], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [a], [c, b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a], [c, b], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [a], [c, b], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c, b], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a], [c, b], [c, d], [c, d], [c, d]]).
%% mrule([[a, b], [a], [c, b], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [a], [c, b], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c, b], [c, d], [c, d], [d, c]]).
%% mrule([[a, b], [a], [c, b], [c, d], [c, e], [d, e]]).
%% mrule([[a, b], [a], [c, b], [c, d], [c, e], [e, d]]).
%% mrule([[a, b], [a], [c, b], [c, d], [d, a], [d, a]]).
%% mrule([[a, b], [a], [c, b], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a], [c, b], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [a], [c, b], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [a], [c, b], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c, b], [c, d], [d, c], [d, c]]).
%% mrule([[a, b], [a], [c, b], [c, d], [d, e], [d, e]]).
%% mrule([[a, b], [a], [c, b], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [a], [c, b], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [a], [c, b], [c, d], [d, e], [e, c]]).
%% mrule([[a, b], [a], [c, b], [c, d], [d, e], [e, d]]).
%% mrule([[a, b], [a], [c, b], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [a], [c, b], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [a], [c, b], [c, d], [e, c], [e, d]]).
%% mrule([[a, b], [a], [c, b], [c, d], [e, d], [e, d]]).
%% mrule([[a, b], [a], [c, b], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [a], [c, b], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c, b], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [a], [c, b], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [a], [c, b], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [a], [c, b], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c, b], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a], [c, b], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a], [c, b], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a], [c, b], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [a], [c, b], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [a], [c, b], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [a], [c, b], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a], [c, b], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [a], [c, b], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [a], [c, b], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [a], [c, b], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a], [c, b], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [a], [c, b], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [a], [c, b], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [a], [c, b], [d, e], [e, c], [e, d]]).
%% mrule([[a, b], [a], [c, d], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a], [c, d], [c, d], [c, e], [e, b]]).
%% mrule([[a, b], [a], [c, d], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a], [c, d], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [a], [c, d], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c, d], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [a], [c, d], [c, d], [e, b], [e, c]]).
%% mrule([[a, b], [a], [c, d], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [a], [c, d], [c, e], [c, e], [d, b]]).
%% mrule([[a, b], [a], [c, d], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [a], [c, d], [c, e], [d, b], [d, e]]).
%% mrule([[a, b], [a], [c, d], [c, e], [d, b], [e, a]]).
%% mrule([[a, b], [a], [c, d], [c, e], [d, b], [e, b]]).
%% mrule([[a, b], [a], [c, d], [c, e], [d, b], [e, c]]).
%% mrule([[a, b], [a], [c, d], [c, e], [d, b], [e, d]]).
%% mrule([[a, b], [a], [c, d], [c, e], [d, c], [e, b]]).
%% mrule([[a, b], [a], [c, d], [c, e], [d, e], [e, b]]).
%% mrule([[a, b], [a], [c, d], [c, e], [e, b], [e, d]]).
%% mrule([[a, b], [a], [c, d], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c, d], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [a], [c, d], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a], [c, d], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a], [c, d], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a], [c, d], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [a], [c, d], [d, b], [e, b], [e, c]]).
%% mrule([[a, b], [a], [c, d], [d, b], [e, c], [e, c]]).
%% mrule([[a, b], [a], [c, d], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a], [c, d], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a], [c, d], [d, c], [e, b], [e, c]]).
%% mrule([[a, b], [a], [c, d], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a], [c, d], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [a], [c, d], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [b], [b], [b], [b], [a, b]]).
%% mrule([[a], [b], [b], [b], [b], [b, a]]).
%% mrule([[a, b], [b], [b], [b], [b], [b, a]]).
%% mrule([[a, b], [b], [b], [b], [c], [a, c]]).
%% mrule([[a, b], [b], [b], [b], [c], [c, a]]).
%% mrule([[a, b], [b], [b], [b], [a, b], [a, b]]).
%% mrule([[a, b], [b], [b], [b], [a, b], [b, a]]).
%% mrule([[a, b], [b], [b], [b], [a, c], [a, c]]).
%% mrule([[a], [b], [b], [b], [a, c], [b, c]]).
%% mrule([[a, b], [b], [b], [b], [a, c], [b, c]]).
%% mrule([[a, b], [b], [b], [b], [a, c], [c, a]]).
%% mrule([[a], [b], [b], [b], [a, c], [c, b]]).
%% mrule([[a, b], [b], [b], [b], [a, c], [c, b]]).
%% mrule([[a], [b], [b], [b], [b, a], [b, a]]).
%% mrule([[a, b], [b], [b], [b], [b, a], [b, a]]).
%% mrule([[a], [b], [b], [b], [b, c], [c, a]]).
%% mrule([[a, b], [b], [b], [b], [b, c], [c, a]]).
%% mrule([[a, b], [b], [b], [b], [c, a], [c, a]]).
%% mrule([[a], [b], [b], [b], [c, a], [c, b]]).
%% mrule([[a, b], [b], [b], [b], [c, a], [c, b]]).
%% mrule([[a, b], [b], [b], [c], [c], [a, c]]).
%% mrule([[a, b], [b], [b], [c], [c], [c, a]]).
%% mrule([[a, b], [b], [b], [c], [a, b], [a, c]]).
%% mrule([[a, b], [b], [b], [c], [a, b], [b, c]]).
%% mrule([[a, b], [b], [b], [c], [a, b], [c, a]]).
%% mrule([[a, b], [b], [b], [c], [a, b], [c, b]]).
%% mrule([[a, b], [b], [b], [c], [a, c], [a, c]]).
%% mrule([[a], [b], [b], [c], [a, c], [b, a]]).
%% mrule([[a, b], [b], [b], [c], [a, c], [b, a]]).
%% mrule([[a], [b], [b], [c], [a, c], [b, c]]).
%% mrule([[a, b], [b], [b], [c], [a, c], [b, c]]).
%% mrule([[a, b], [b], [b], [c], [a, c], [c, a]]).
%% mrule([[a], [b], [b], [c], [a, c], [c, b]]).
%% mrule([[a, b], [b], [b], [c], [a, c], [c, b]]).
%% mrule([[a, b], [b], [b], [c], [a, d], [c, d]]).
%% mrule([[a, b], [b], [b], [c], [a, d], [d, c]]).
%% mrule([[a], [b], [b], [c], [b, a], [b, c]]).
%% mrule([[a, b], [b], [b], [c], [b, a], [b, c]]).
%% mrule([[a], [b], [b], [c], [b, a], [c, a]]).
%% mrule([[a, b], [b], [b], [c], [b, a], [c, a]]).
%% mrule([[a], [b], [b], [c], [b, a], [c, b]]).
%% mrule([[a, b], [b], [b], [c], [b, a], [c, b]]).
%% mrule([[a], [b], [b], [c], [b, c], [c, a]]).
%% mrule([[a, b], [b], [b], [c], [b, c], [c, a]]).
%% mrule([[a, b], [b], [b], [c], [c, a], [c, a]]).
%% mrule([[a], [b], [b], [c], [c, a], [c, b]]).
%% mrule([[a, b], [b], [b], [c], [c, a], [c, b]]).
%% mrule([[a, b], [b], [b], [c], [c, d], [d, a]]).
%% mrule([[a, b], [b], [b], [c], [d, a], [d, c]]).
%% mrule([[a, b], [b], [b], [a, b], [a, b], [a, b]]).
%% mrule([[a, b], [b], [b], [a, b], [a, b], [b, a]]).
%% mrule([[a, b], [b], [b], [a, b], [a, c], [a, c]]).
%% mrule([[a, b], [b], [b], [a, b], [a, c], [b, c]]).
%% mrule([[a, b], [b], [b], [a, b], [a, c], [c, a]]).
%% mrule([[a, b], [b], [b], [a, b], [a, c], [c, b]]).
%% mrule([[a, b], [b], [b], [a, b], [b, a], [b, a]]).
%% mrule([[a, b], [b], [b], [a, b], [b, c], [b, c]]).
%% mrule([[a, b], [b], [b], [a, b], [b, c], [c, a]]).
%% mrule([[a, b], [b], [b], [a, b], [b, c], [c, b]]).
%% mrule([[a, b], [b], [b], [a, b], [c, a], [c, a]]).
%% mrule([[a, b], [b], [b], [a, b], [c, a], [c, b]]).
%% mrule([[a, b], [b], [b], [a, b], [c, b], [c, b]]).
%% mrule([[a, b], [b], [b], [a, c], [a, c], [a, c]]).
%% mrule([[a], [b], [b], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [b], [b], [a, c], [a, c], [b, a]]).
%% mrule([[a], [b], [b], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [b], [b], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [b], [b], [a, c], [a, c], [c, a]]).
%% mrule([[a], [b], [b], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [b], [b], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [b], [b], [a, c], [a, d], [c, d]]).
%% mrule([[a, b], [b], [b], [a, c], [a, d], [d, c]]).
%% mrule([[a], [b], [b], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [b], [b], [a, c], [b, a], [b, c]]).
%% mrule([[a], [b], [b], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [b], [b], [a, c], [b, a], [c, a]]).
%% mrule([[a], [b], [b], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [b], [b], [a, c], [b, a], [c, b]]).
%% mrule([[a], [b], [b], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [b], [b], [a, c], [b, c], [b, c]]).
%% mrule([[a], [b], [b], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [b], [b], [a, c], [b, c], [c, a]]).
%% mrule([[a], [b], [b], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [b], [b], [a, c], [b, c], [c, b]]).
%% mrule([[a], [b], [b], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [b], [b], [a, c], [b, d], [c, d]]).
%% mrule([[a], [b], [b], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [b], [b], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [b], [b], [a, c], [c, a], [c, a]]).
%% mrule([[a], [b], [b], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [b], [b], [a, c], [c, a], [c, b]]).
%% mrule([[a], [b], [b], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [b], [b], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [b], [b], [a, c], [c, d], [c, d]]).
%% mrule([[a, b], [b], [b], [a, c], [c, d], [d, a]]).
%% mrule([[a], [b], [b], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [b], [b], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [b], [b], [a, c], [c, d], [d, c]]).
%% mrule([[a, b], [b], [b], [a, c], [d, a], [d, c]]).
%% mrule([[a], [b], [b], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [b], [b], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [b], [b], [a, c], [d, c], [d, c]]).
%% mrule([[a], [b], [b], [b, a], [b, a], [b, a]]).
%% mrule([[a, b], [b], [b], [b, a], [b, a], [b, a]]).
%% mrule([[a], [b], [b], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [b], [b], [b, a], [b, c], [b, c]]).
%% mrule([[a], [b], [b], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [b], [b], [b, a], [b, c], [c, a]]).
%% mrule([[a], [b], [b], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [b], [b], [b, a], [b, c], [c, b]]).
%% mrule([[a], [b], [b], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [b], [b], [b, a], [c, a], [c, a]]).
%% mrule([[a], [b], [b], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [b], [b], [b, a], [c, a], [c, b]]).
%% mrule([[a], [b], [b], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [b], [b], [b, a], [c, b], [c, b]]).
%% mrule([[a], [b], [b], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [b], [b], [b, c], [b, c], [c, a]]).
%% mrule([[a], [b], [b], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [b], [b], [b, c], [c, a], [c, a]]).
%% mrule([[a], [b], [b], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [b], [b], [b, c], [c, a], [c, b]]).
%% mrule([[a], [b], [b], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [b], [b], [b, c], [c, d], [d, a]]).
%% mrule([[a], [b], [b], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [b], [b], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [b], [b], [c, a], [c, a], [c, a]]).
%% mrule([[a], [b], [b], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [b], [b], [c, a], [c, a], [c, b]]).
%% mrule([[a], [b], [b], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b], [b], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b], [b], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [b], [b], [c, a], [c, d], [d, a]]).
%% mrule([[a], [b], [b], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b], [b], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b], [b], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [b], [b], [c, a], [d, a], [d, c]]).
%% mrule([[a], [b], [b], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [b], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [b], [c, a], [d, c], [d, c]]).
%% mrule([[a], [b], [b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b], [b], [c, b], [c, d], [d, a]]).
%% mrule([[a], [b], [b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b], [b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b], [b], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [b], [b], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c], [c], [c], [a, c]]).
%% mrule([[a, b], [b], [c], [c], [c], [c, a]]).
%% mrule([[a, b], [b], [c], [c], [a, b], [a, c]]).
%% mrule([[a, b], [b], [c], [c], [a, b], [b, c]]).
%% mrule([[a, b], [b], [c], [c], [a, b], [c, a]]).
%% mrule([[a, b], [b], [c], [c], [a, b], [c, b]]).
%% mrule([[a, b], [b], [c], [c], [a, c], [a, c]]).
%% mrule([[a], [b], [c], [c], [a, c], [b, a]]).
%% mrule([[a, b], [b], [c], [c], [a, c], [b, a]]).
%% mrule([[a], [b], [c], [c], [a, c], [b, c]]).
%% mrule([[a, b], [b], [c], [c], [a, c], [b, c]]).
%% mrule([[a, b], [b], [c], [c], [a, c], [c, a]]).
%% mrule([[a], [b], [c], [c], [a, c], [c, b]]).
%% mrule([[a, b], [b], [c], [c], [a, c], [c, b]]).
%% mrule([[a, b], [b], [c], [c], [a, d], [c, d]]).
%% mrule([[a, b], [b], [c], [c], [a, d], [d, c]]).
%% mrule([[a], [b], [c], [c], [b, a], [b, c]]).
%% mrule([[a, b], [b], [c], [c], [b, a], [b, c]]).
%% mrule([[a], [b], [c], [c], [b, a], [c, a]]).
%% mrule([[a, b], [b], [c], [c], [b, a], [c, a]]).
%% mrule([[a], [b], [c], [c], [b, a], [c, b]]).
%% mrule([[a, b], [b], [c], [c], [b, a], [c, b]]).
%% mrule([[a], [b], [c], [c], [b, c], [c, a]]).
%% mrule([[a, b], [b], [c], [c], [b, c], [c, a]]).
%% mrule([[a, b], [b], [c], [c], [c, a], [c, a]]).
%% mrule([[a], [b], [c], [c], [c, a], [c, b]]).
%% mrule([[a, b], [b], [c], [c], [c, a], [c, b]]).
%% mrule([[a, b], [b], [c], [c], [c, d], [d, a]]).
%% mrule([[a, b], [b], [c], [c], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c], [d], [a, c], [a, d]]).
%% mrule([[a, b], [b], [c], [d], [a, c], [b, d]]).
%% mrule([[a, b], [b], [c], [d], [a, c], [c, d]]).
%% mrule([[a, b], [b], [c], [d], [a, c], [d, a]]).
%% mrule([[a, b], [b], [c], [d], [a, c], [d, b]]).
%% mrule([[a, b], [b], [c], [d], [a, c], [d, c]]).
%% mrule([[a, b], [b], [c], [d], [a, d], [b, c]]).
%% mrule([[a, b], [b], [c], [d], [a, d], [c, a]]).
%% mrule([[a, b], [b], [c], [d], [a, d], [c, b]]).
%% mrule([[a, b], [b], [c], [d], [a, d], [c, d]]).
%% mrule([[a, b], [b], [c], [d], [a, d], [d, c]]).
%% mrule([[a, b], [b], [c], [d], [b, c], [d, a]]).
%% mrule([[a, b], [b], [c], [d], [b, d], [c, a]]).
%% mrule([[a, b], [b], [c], [d], [c, a], [c, d]]).
%% mrule([[a, b], [b], [c], [d], [c, a], [d, a]]).
%% mrule([[a, b], [b], [c], [d], [c, a], [d, b]]).
%% mrule([[a, b], [b], [c], [d], [c, a], [d, c]]).
%% mrule([[a, b], [b], [c], [d], [c, b], [d, a]]).
%% mrule([[a, b], [b], [c], [d], [c, d], [d, a]]).
%% mrule([[a, b], [b], [c], [d], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c], [a, b], [a, b], [a, c]]).
%% mrule([[a, b], [b], [c], [a, b], [a, b], [b, c]]).
%% mrule([[a, b], [b], [c], [a, b], [a, b], [c, a]]).
%% mrule([[a, b], [b], [c], [a, b], [a, b], [c, b]]).
%% mrule([[a, b], [b], [c], [a, b], [a, c], [a, c]]).
%% mrule([[a, b], [b], [c], [a, b], [a, c], [b, a]]).
%% mrule([[a, b], [b], [c], [a, b], [a, c], [b, c]]).
%% mrule([[a, b], [b], [c], [a, b], [a, c], [c, a]]).
%% mrule([[a, b], [b], [c], [a, b], [a, c], [c, b]]).
%% mrule([[a, b], [b], [c], [a, b], [a, d], [c, d]]).
%% mrule([[a, b], [b], [c], [a, b], [a, d], [d, c]]).
%% mrule([[a, b], [b], [c], [a, b], [b, a], [b, c]]).
%% mrule([[a, b], [b], [c], [a, b], [b, a], [c, a]]).
%% mrule([[a, b], [b], [c], [a, b], [b, a], [c, b]]).
%% mrule([[a, b], [b], [c], [a, b], [b, c], [b, c]]).
%% mrule([[a, b], [b], [c], [a, b], [b, c], [c, a]]).
%% mrule([[a, b], [b], [c], [a, b], [b, c], [c, b]]).
%% mrule([[a, b], [b], [c], [a, b], [b, d], [c, d]]).
%% mrule([[a, b], [b], [c], [a, b], [b, d], [d, c]]).
%% mrule([[a, b], [b], [c], [a, b], [c, a], [c, a]]).
%% mrule([[a, b], [b], [c], [a, b], [c, a], [c, b]]).
%% mrule([[a, b], [b], [c], [a, b], [c, b], [c, b]]).
%% mrule([[a, b], [b], [c], [a, b], [c, d], [d, a]]).
%% mrule([[a, b], [b], [c], [a, b], [c, d], [d, b]]).
%% mrule([[a, b], [b], [c], [a, b], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c], [a, b], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c], [a, c], [a, c], [a, c]]).
%% mrule([[a], [b], [c], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [b], [c], [a, c], [a, c], [b, a]]).
%% mrule([[a], [b], [c], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [b], [c], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [b], [c], [a, c], [a, c], [c, a]]).
%% mrule([[a], [b], [c], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [b], [c], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [b], [c], [a, c], [a, d], [a, d]]).
%% mrule([[a], [b], [c], [a, c], [a, d], [b, d]]).
%% mrule([[a, b], [b], [c], [a, c], [a, d], [b, d]]).
%% mrule([[a, b], [b], [c], [a, c], [a, d], [c, d]]).
%% mrule([[a, b], [b], [c], [a, c], [a, d], [d, a]]).
%% mrule([[a], [b], [c], [a, c], [a, d], [d, b]]).
%% mrule([[a, b], [b], [c], [a, c], [a, d], [d, b]]).
%% mrule([[a, b], [b], [c], [a, c], [a, d], [d, c]]).
%% mrule([[a], [b], [c], [a, c], [b, a], [b, a]]).
%% mrule([[a, b], [b], [c], [a, c], [b, a], [b, a]]).
%% mrule([[a], [b], [c], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [b], [c], [a, c], [b, a], [b, c]]).
%% mrule([[a], [b], [c], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [b], [c], [a, c], [b, a], [c, a]]).
%% mrule([[a], [b], [c], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [b], [c], [a, c], [b, a], [c, b]]).
%% mrule([[a], [b], [c], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [b], [c], [a, c], [b, c], [b, c]]).
%% mrule([[a], [b], [c], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [b], [c], [a, c], [b, c], [c, a]]).
%% mrule([[a], [b], [c], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [b], [c], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [b], [c], [a, c], [b, d], [b, d]]).
%% mrule([[a], [b], [c], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [b], [c], [a, c], [b, d], [c, d]]).
%% mrule([[a], [b], [c], [a, c], [b, d], [d, a]]).
%% mrule([[a, b], [b], [c], [a, c], [b, d], [d, a]]).
%% mrule([[a, b], [b], [c], [a, c], [b, d], [d, b]]).
%% mrule([[a], [b], [c], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [b], [c], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [b], [c], [a, c], [c, a], [c, a]]).
%% mrule([[a], [b], [c], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [b], [c], [a, c], [c, a], [c, b]]).
%% mrule([[a], [b], [c], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [b], [c], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [b], [c], [a, c], [c, d], [c, d]]).
%% mrule([[a, b], [b], [c], [a, c], [c, d], [d, a]]).
%% mrule([[a], [b], [c], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [b], [c], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [b], [c], [a, c], [c, d], [d, c]]).
%% mrule([[a, b], [b], [c], [a, c], [d, a], [d, a]]).
%% mrule([[a], [b], [c], [a, c], [d, a], [d, b]]).
%% mrule([[a, b], [b], [c], [a, c], [d, a], [d, b]]).
%% mrule([[a, b], [b], [c], [a, c], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c], [a, c], [d, b], [d, b]]).
%% mrule([[a], [b], [c], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c], [a, c], [d, c], [d, c]]).
%% mrule([[a, b], [b], [c], [a, d], [a, d], [b, c]]).
%% mrule([[a, b], [b], [c], [a, d], [a, d], [c, a]]).
%% mrule([[a, b], [b], [c], [a, d], [a, d], [c, b]]).
%% mrule([[a, b], [b], [c], [a, d], [a, d], [c, d]]).
%% mrule([[a, b], [b], [c], [a, d], [a, d], [d, c]]).
%% mrule([[a], [b], [c], [a, d], [b, a], [c, d]]).
%% mrule([[a, b], [b], [c], [a, d], [b, a], [c, d]]).
%% mrule([[a], [b], [c], [a, d], [b, a], [d, c]]).
%% mrule([[a, b], [b], [c], [a, d], [b, a], [d, c]]).
%% mrule([[a], [b], [c], [a, d], [b, c], [b, d]]).
%% mrule([[a, b], [b], [c], [a, d], [b, c], [b, d]]).
%% mrule([[a], [b], [c], [a, d], [b, c], [c, d]]).
%% mrule([[a, b], [b], [c], [a, d], [b, c], [c, d]]).
%% mrule([[a, b], [b], [c], [a, d], [b, c], [d, a]]).
%% mrule([[a], [b], [c], [a, d], [b, c], [d, b]]).
%% mrule([[a, b], [b], [c], [a, d], [b, c], [d, b]]).
%% mrule([[a], [b], [c], [a, d], [b, c], [d, c]]).
%% mrule([[a, b], [b], [c], [a, d], [b, c], [d, c]]).
%% mrule([[a], [b], [c], [a, d], [b, d], [c, a]]).
%% mrule([[a, b], [b], [c], [a, d], [b, d], [c, a]]).
%% mrule([[a], [b], [c], [a, d], [b, d], [c, b]]).
%% mrule([[a, b], [b], [c], [a, d], [b, d], [c, b]]).
%% mrule([[a], [b], [c], [a, d], [b, d], [c, d]]).
%% mrule([[a, b], [b], [c], [a, d], [b, d], [c, d]]).
%% mrule([[a], [b], [c], [a, d], [b, d], [d, c]]).
%% mrule([[a, b], [b], [c], [a, d], [b, d], [d, c]]).
%% mrule([[a, b], [b], [c], [a, d], [c, a], [c, d]]).
%% mrule([[a, b], [b], [c], [a, d], [c, a], [d, a]]).
%% mrule([[a], [b], [c], [a, d], [c, a], [d, b]]).
%% mrule([[a, b], [b], [c], [a, d], [c, a], [d, b]]).
%% mrule([[a, b], [b], [c], [a, d], [c, a], [d, c]]).
%% mrule([[a], [b], [c], [a, d], [c, b], [c, d]]).
%% mrule([[a, b], [b], [c], [a, d], [c, b], [c, d]]).
%% mrule([[a, b], [b], [c], [a, d], [c, b], [d, a]]).
%% mrule([[a], [b], [c], [a, d], [c, b], [d, b]]).
%% mrule([[a, b], [b], [c], [a, d], [c, b], [d, b]]).
%% mrule([[a], [b], [c], [a, d], [c, b], [d, c]]).
%% mrule([[a, b], [b], [c], [a, d], [c, b], [d, c]]).
%% mrule([[a, b], [b], [c], [a, d], [c, d], [c, d]]).
%% mrule([[a, b], [b], [c], [a, d], [c, d], [d, a]]).
%% mrule([[a], [b], [c], [a, d], [c, d], [d, b]]).
%% mrule([[a, b], [b], [c], [a, d], [c, d], [d, b]]).
%% mrule([[a, b], [b], [c], [a, d], [c, d], [d, c]]).
%% mrule([[a, b], [b], [c], [a, d], [c, e], [d, e]]).
%% mrule([[a, b], [b], [c], [a, d], [c, e], [e, d]]).
%% mrule([[a, b], [b], [c], [a, d], [d, a], [d, c]]).
%% mrule([[a], [b], [c], [a, d], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c], [a, d], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c], [a, d], [d, c], [d, c]]).
%% mrule([[a, b], [b], [c], [a, d], [d, e], [e, c]]).
%% mrule([[a, b], [b], [c], [a, d], [e, c], [e, d]]).
%% mrule([[a], [b], [c], [b, a], [b, a], [b, c]]).
%% mrule([[a, b], [b], [c], [b, a], [b, a], [b, c]]).
%% mrule([[a], [b], [c], [b, a], [b, a], [c, a]]).
%% mrule([[a, b], [b], [c], [b, a], [b, a], [c, a]]).
%% mrule([[a], [b], [c], [b, a], [b, a], [c, b]]).
%% mrule([[a, b], [b], [c], [b, a], [b, a], [c, b]]).
%% mrule([[a], [b], [c], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [b], [c], [b, a], [b, c], [b, c]]).
%% mrule([[a], [b], [c], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [b], [c], [b, a], [b, c], [c, a]]).
%% mrule([[a], [b], [c], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [b], [c], [b, a], [b, c], [c, b]]).
%% mrule([[a], [b], [c], [b, a], [b, d], [c, d]]).
%% mrule([[a, b], [b], [c], [b, a], [b, d], [c, d]]).
%% mrule([[a], [b], [c], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [b], [c], [b, a], [b, d], [d, c]]).
%% mrule([[a], [b], [c], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [b], [c], [b, a], [c, a], [c, a]]).
%% mrule([[a], [b], [c], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [b], [c], [b, a], [c, a], [c, b]]).
%% mrule([[a], [b], [c], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [b], [c], [b, a], [c, b], [c, b]]).
%% mrule([[a], [b], [c], [b, a], [c, d], [d, a]]).
%% mrule([[a, b], [b], [c], [b, a], [c, d], [d, a]]).
%% mrule([[a], [b], [c], [b, a], [c, d], [d, b]]).
%% mrule([[a, b], [b], [c], [b, a], [c, d], [d, b]]).
%% mrule([[a], [b], [c], [b, a], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c], [b, a], [d, a], [d, c]]).
%% mrule([[a], [b], [c], [b, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c], [b, a], [d, b], [d, c]]).
%% mrule([[a], [b], [c], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [b], [c], [b, c], [b, c], [c, a]]).
%% mrule([[a], [b], [c], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [b], [c], [b, c], [b, d], [d, a]]).
%% mrule([[a], [b], [c], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [b], [c], [b, c], [c, a], [c, a]]).
%% mrule([[a], [b], [c], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [b], [c], [b, c], [c, a], [c, b]]).
%% mrule([[a], [b], [c], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [b], [c], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [b], [c], [b, c], [d, a], [d, a]]).
%% mrule([[a], [b], [c], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [b], [c], [b, c], [d, a], [d, b]]).
%% mrule([[a], [b], [c], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c], [b, d], [b, d], [c, a]]).
%% mrule([[a], [b], [c], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [b], [c], [b, d], [c, a], [c, d]]).
%% mrule([[a], [b], [c], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [b], [c], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [b], [c], [b, d], [c, a], [d, b]]).
%% mrule([[a], [b], [c], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [b], [c], [b, d], [c, a], [d, c]]).
%% mrule([[a], [b], [c], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [b], [c], [b, d], [c, b], [d, a]]).
%% mrule([[a], [b], [c], [b, d], [c, d], [d, a]]).
%% mrule([[a, b], [b], [c], [b, d], [c, d], [d, a]]).
%% mrule([[a], [b], [c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c], [c, a], [c, a], [c, a]]).
%% mrule([[a], [b], [c], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [b], [c], [c, a], [c, a], [c, b]]).
%% mrule([[a], [b], [c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b], [c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b], [c], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [b], [c], [c, a], [c, d], [d, a]]).
%% mrule([[a], [b], [c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b], [c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b], [c], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [b], [c], [c, a], [d, a], [d, a]]).
%% mrule([[a], [b], [c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [b], [c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [b], [c], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c], [c, a], [d, b], [d, b]]).
%% mrule([[a], [b], [c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c], [c, a], [d, c], [d, c]]).
%% mrule([[a], [b], [c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b], [c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b], [c], [c, b], [d, a], [d, a]]).
%% mrule([[a], [b], [c], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [b], [c], [c, b], [d, a], [d, b]]).
%% mrule([[a], [b], [c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [b], [c], [c, d], [d, a], [d, a]]).
%% mrule([[a], [b], [c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b], [c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b], [c], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [b], [c], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [b], [c], [d, a], [d, a], [d, c]]).
%% mrule([[a], [b], [c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [b], [c], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [b], [c], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [b], [c], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [b], [c], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [b], [c], [d, a], [d, e], [c, e]]).
%% mrule([[a, b], [b], [c], [d, a], [c, e], [e, d]]).
%% mrule([[a, b], [b], [a, b], [a, b], [a, b], [a, b]]).
%% mrule([[a, b], [b], [a, b], [a, b], [a, b], [b, a]]).
%% mrule([[a, b], [b], [a, b], [a, b], [a, c], [a, c]]).
%% mrule([[a, b], [b], [a, b], [a, b], [a, c], [b, c]]).
%% mrule([[a, b], [b], [a, b], [a, b], [a, c], [c, a]]).
%% mrule([[a, b], [b], [a, b], [a, b], [a, c], [c, b]]).
%% mrule([[a, b], [b], [a, b], [a, b], [b, a], [b, a]]).
%% mrule([[a, b], [b], [a, b], [a, b], [b, c], [b, c]]).
%% mrule([[a, b], [b], [a, b], [a, b], [b, c], [c, a]]).
%% mrule([[a, b], [b], [a, b], [a, b], [b, c], [c, b]]).
%% mrule([[a, b], [b], [a, b], [a, b], [c, a], [c, a]]).
%% mrule([[a, b], [b], [a, b], [a, b], [c, a], [c, b]]).
%% mrule([[a, b], [b], [a, b], [a, b], [c, b], [c, b]]).
%% mrule([[a, b], [b], [a, b], [a, c], [a, c], [a, c]]).
%% mrule([[a, b], [b], [a, b], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [b], [a, b], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [b], [a, b], [a, c], [a, c], [c, a]]).
%% mrule([[a, b], [b], [a, b], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [b], [a, b], [a, c], [a, d], [c, d]]).
%% mrule([[a, b], [b], [a, b], [a, c], [a, d], [d, c]]).
%% mrule([[a, b], [b], [a, b], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [b], [a, b], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [b], [a, b], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [b], [a, b], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [b], [a, b], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [b], [a, b], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [b], [a, b], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [b], [a, b], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [b], [a, b], [a, c], [c, a], [c, a]]).
%% mrule([[a, b], [b], [a, b], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [b], [a, b], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [b], [a, b], [a, c], [c, d], [c, d]]).
%% mrule([[a, b], [b], [a, b], [a, c], [c, d], [d, a]]).
%% mrule([[a, b], [b], [a, b], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [b], [a, b], [a, c], [c, d], [d, c]]).
%% mrule([[a, b], [b], [a, b], [a, c], [d, a], [d, c]]).
%% mrule([[a, b], [b], [a, b], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, b], [a, c], [d, c], [d, c]]).
%% mrule([[a, b], [b], [a, b], [b, a], [b, a], [b, a]]).
%% mrule([[a, b], [b], [a, b], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [b], [a, b], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [b], [a, b], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [b], [a, b], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [b], [a, b], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [b], [a, b], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [b], [a, b], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [b], [a, b], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [b], [a, b], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [b], [a, b], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [b], [a, b], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [b], [a, b], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [b], [a, b], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [b], [a, b], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [b], [a, b], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [b], [a, b], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [b], [a, b], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [b], [a, b], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [b], [a, b], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [b], [a, b], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, b], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [b], [a, b], [c, a], [c, a], [c, a]]).
%% mrule([[a, b], [b], [a, b], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [b], [a, b], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b], [a, b], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [b], [a, b], [c, a], [c, d], [d, a]]).
%% mrule([[a, b], [b], [a, b], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b], [a, b], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [b], [a, b], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [b], [a, b], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, b], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [b], [a, b], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [b], [a, b], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [b], [a, b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b], [a, b], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [b], [a, b], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [b], [a, b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b], [a, b], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, b], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [b], [a, b], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [b], [a, b], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [b], [a, b], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [b], [a, b], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [a, c], [a, c], [a, c]]).
%% mrule([[a], [b], [a, c], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [b], [a, c], [a, c], [a, c], [b, a]]).
%% mrule([[a], [b], [a, c], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [b], [a, c], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [b], [a, c], [a, c], [a, c], [c, a]]).
%% mrule([[a], [b], [a, c], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [b], [a, c], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [b], [a, c], [a, c], [a, d], [a, d]]).
%% mrule([[a], [b], [a, c], [a, c], [a, d], [b, d]]).
%% mrule([[a, b], [b], [a, c], [a, c], [a, d], [b, d]]).
%% mrule([[a, b], [b], [a, c], [a, c], [a, d], [c, d]]).
%% mrule([[a, b], [b], [a, c], [a, c], [a, d], [d, a]]).
%% mrule([[a], [b], [a, c], [a, c], [a, d], [d, b]]).
%% mrule([[a, b], [b], [a, c], [a, c], [a, d], [d, b]]).
%% mrule([[a, b], [b], [a, c], [a, c], [a, d], [d, c]]).
%% mrule([[a], [b], [a, c], [a, c], [b, a], [b, a]]).
%% mrule([[a, b], [b], [a, c], [a, c], [b, a], [b, a]]).
%% mrule([[a], [b], [a, c], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [b], [a, c], [a, c], [b, a], [b, c]]).
%% mrule([[a], [b], [a, c], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [b], [a, c], [a, c], [b, a], [c, a]]).
%% mrule([[a], [b], [a, c], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [b], [a, c], [a, c], [b, a], [c, b]]).
%% mrule([[a], [b], [a, c], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [b], [a, c], [a, c], [b, c], [b, c]]).
%% mrule([[a], [b], [a, c], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [b], [a, c], [a, c], [b, c], [c, a]]).
%% mrule([[a], [b], [a, c], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [b], [a, c], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [b], [a, c], [a, c], [b, d], [b, d]]).
%% mrule([[a], [b], [a, c], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [b], [a, c], [a, c], [b, d], [c, d]]).
%% mrule([[a], [b], [a, c], [a, c], [b, d], [d, a]]).
%% mrule([[a, b], [b], [a, c], [a, c], [b, d], [d, a]]).
%% mrule([[a, b], [b], [a, c], [a, c], [b, d], [d, b]]).
%% mrule([[a], [b], [a, c], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [b], [a, c], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [b], [a, c], [a, c], [c, a], [c, a]]).
%% mrule([[a], [b], [a, c], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [b], [a, c], [a, c], [c, a], [c, b]]).
%% mrule([[a], [b], [a, c], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [b], [a, c], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [b], [a, c], [a, c], [c, d], [c, d]]).
%% mrule([[a, b], [b], [a, c], [a, c], [c, d], [d, a]]).
%% mrule([[a], [b], [a, c], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [b], [a, c], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [b], [a, c], [a, c], [c, d], [d, c]]).
%% mrule([[a, b], [b], [a, c], [a, c], [d, a], [d, a]]).
%% mrule([[a], [b], [a, c], [a, c], [d, a], [d, b]]).
%% mrule([[a, b], [b], [a, c], [a, c], [d, a], [d, b]]).
%% mrule([[a, b], [b], [a, c], [a, c], [d, a], [d, c]]).
%% mrule([[a, b], [b], [a, c], [a, c], [d, b], [d, b]]).
%% mrule([[a], [b], [a, c], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [a, c], [d, c], [d, c]]).
%% mrule([[a], [b], [a, c], [a, d], [a, d], [b, c]]).
%% mrule([[a, b], [b], [a, c], [a, d], [a, d], [b, c]]).
%% mrule([[a, b], [b], [a, c], [a, d], [a, d], [c, a]]).
%% mrule([[a], [b], [a, c], [a, d], [a, d], [c, b]]).
%% mrule([[a, b], [b], [a, c], [a, d], [a, d], [c, b]]).
%% mrule([[a, b], [b], [a, c], [a, d], [a, d], [c, d]]).
%% mrule([[a, b], [b], [a, c], [a, d], [a, d], [d, c]]).
%% mrule([[a], [b], [a, c], [a, d], [b, a], [c, d]]).
%% mrule([[a, b], [b], [a, c], [a, d], [b, a], [c, d]]).
%% mrule([[a], [b], [a, c], [a, d], [b, a], [d, c]]).
%% mrule([[a, b], [b], [a, c], [a, d], [b, a], [d, c]]).
%% mrule([[a], [b], [a, c], [a, d], [b, c], [b, d]]).
%% mrule([[a, b], [b], [a, c], [a, d], [b, c], [b, d]]).
%% mrule([[a], [b], [a, c], [a, d], [b, c], [c, d]]).
%% mrule([[a, b], [b], [a, c], [a, d], [b, c], [c, d]]).
%% mrule([[a], [b], [a, c], [a, d], [b, c], [d, a]]).
%% mrule([[a, b], [b], [a, c], [a, d], [b, c], [d, a]]).
%% mrule([[a], [b], [a, c], [a, d], [b, c], [d, b]]).
%% mrule([[a, b], [b], [a, c], [a, d], [b, c], [d, b]]).
%% mrule([[a], [b], [a, c], [a, d], [b, c], [d, c]]).
%% mrule([[a, b], [b], [a, c], [a, d], [b, c], [d, c]]).
%% mrule([[a], [b], [a, c], [a, d], [b, d], [c, a]]).
%% mrule([[a, b], [b], [a, c], [a, d], [b, d], [c, a]]).
%% mrule([[a], [b], [a, c], [a, d], [b, d], [c, b]]).
%% mrule([[a, b], [b], [a, c], [a, d], [b, d], [c, b]]).
%% mrule([[a], [b], [a, c], [a, d], [b, d], [c, d]]).
%% mrule([[a, b], [b], [a, c], [a, d], [b, d], [c, d]]).
%% mrule([[a], [b], [a, c], [a, d], [b, d], [d, c]]).
%% mrule([[a, b], [b], [a, c], [a, d], [b, d], [d, c]]).
%% mrule([[a, b], [b], [a, c], [a, d], [c, a], [c, d]]).
%% mrule([[a, b], [b], [a, c], [a, d], [c, a], [d, a]]).
%% mrule([[a], [b], [a, c], [a, d], [c, a], [d, b]]).
%% mrule([[a, b], [b], [a, c], [a, d], [c, a], [d, b]]).
%% mrule([[a, b], [b], [a, c], [a, d], [c, a], [d, c]]).
%% mrule([[a], [b], [a, c], [a, d], [c, b], [c, d]]).
%% mrule([[a, b], [b], [a, c], [a, d], [c, b], [c, d]]).
%% mrule([[a], [b], [a, c], [a, d], [c, b], [d, a]]).
%% mrule([[a, b], [b], [a, c], [a, d], [c, b], [d, a]]).
%% mrule([[a], [b], [a, c], [a, d], [c, b], [d, b]]).
%% mrule([[a, b], [b], [a, c], [a, d], [c, b], [d, b]]).
%% mrule([[a], [b], [a, c], [a, d], [c, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [a, d], [c, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [a, d], [c, d], [c, d]]).
%% mrule([[a, b], [b], [a, c], [a, d], [c, d], [d, a]]).
%% mrule([[a], [b], [a, c], [a, d], [c, d], [d, b]]).
%% mrule([[a, b], [b], [a, c], [a, d], [c, d], [d, b]]).
%% mrule([[a, b], [b], [a, c], [a, d], [c, d], [d, c]]).
%% mrule([[a, b], [b], [a, c], [a, d], [c, e], [d, e]]).
%% mrule([[a, b], [b], [a, c], [a, d], [c, e], [e, d]]).
%% mrule([[a, b], [b], [a, c], [a, d], [d, a], [d, c]]).
%% mrule([[a], [b], [a, c], [a, d], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [a, d], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [a, d], [d, c], [d, c]]).
%% mrule([[a, b], [b], [a, c], [a, d], [d, e], [e, c]]).
%% mrule([[a, b], [b], [a, c], [a, d], [e, c], [e, d]]).
%% mrule([[a], [b], [a, c], [b, a], [b, a], [b, c]]).
%% mrule([[a, b], [b], [a, c], [b, a], [b, a], [b, c]]).
%% mrule([[a], [b], [a, c], [b, a], [b, a], [c, a]]).
%% mrule([[a, b], [b], [a, c], [b, a], [b, a], [c, a]]).
%% mrule([[a], [b], [a, c], [b, a], [b, a], [c, b]]).
%% mrule([[a, b], [b], [a, c], [b, a], [b, a], [c, b]]).
%% mrule([[a], [b], [a, c], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [b], [a, c], [b, a], [b, c], [b, c]]).
%% mrule([[a], [b], [a, c], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [b], [a, c], [b, a], [b, c], [c, a]]).
%% mrule([[a], [b], [a, c], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [b], [a, c], [b, a], [b, c], [c, b]]).
%% mrule([[a], [b], [a, c], [b, a], [b, d], [c, d]]).
%% mrule([[a, b], [b], [a, c], [b, a], [b, d], [c, d]]).
%% mrule([[a], [b], [a, c], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [b], [a, c], [b, a], [b, d], [d, c]]).
%% mrule([[a], [b], [a, c], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [b], [a, c], [b, a], [c, a], [c, a]]).
%% mrule([[a], [b], [a, c], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [b], [a, c], [b, a], [c, a], [c, b]]).
%% mrule([[a], [b], [a, c], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [b], [a, c], [b, a], [c, b], [c, b]]).
%% mrule([[a], [b], [a, c], [b, a], [c, d], [c, d]]).
%% mrule([[a, b], [b], [a, c], [b, a], [c, d], [c, d]]).
%% mrule([[a], [b], [a, c], [b, a], [c, d], [d, a]]).
%% mrule([[a, b], [b], [a, c], [b, a], [c, d], [d, a]]).
%% mrule([[a], [b], [a, c], [b, a], [c, d], [d, b]]).
%% mrule([[a, b], [b], [a, c], [b, a], [c, d], [d, b]]).
%% mrule([[a], [b], [a, c], [b, a], [c, d], [d, c]]).
%% mrule([[a, b], [b], [a, c], [b, a], [c, d], [d, c]]).
%% mrule([[a], [b], [a, c], [b, a], [d, a], [d, c]]).
%% mrule([[a, b], [b], [a, c], [b, a], [d, a], [d, c]]).
%% mrule([[a], [b], [a, c], [b, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [b, a], [d, b], [d, c]]).
%% mrule([[a], [b], [a, c], [b, a], [d, c], [d, c]]).
%% mrule([[a, b], [b], [a, c], [b, a], [d, c], [d, c]]).
%% mrule([[a], [b], [a, c], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [b], [a, c], [b, c], [b, c], [b, c]]).
%% mrule([[a], [b], [a, c], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [b], [a, c], [b, c], [b, c], [c, a]]).
%% mrule([[a], [b], [a, c], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [b], [a, c], [b, c], [b, c], [c, b]]).
%% mrule([[a], [b], [a, c], [b, c], [b, d], [b, d]]).
%% mrule([[a, b], [b], [a, c], [b, c], [b, d], [b, d]]).
%% mrule([[a], [b], [a, c], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [b], [a, c], [b, c], [b, d], [c, d]]).
%% mrule([[a], [b], [a, c], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [b], [a, c], [b, c], [b, d], [d, a]]).
%% mrule([[a], [b], [a, c], [b, c], [b, d], [d, b]]).
%% mrule([[a, b], [b], [a, c], [b, c], [b, d], [d, b]]).
%% mrule([[a], [b], [a, c], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [b], [a, c], [b, c], [b, d], [d, c]]).
%% mrule([[a], [b], [a, c], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [b], [a, c], [b, c], [c, a], [c, a]]).
%% mrule([[a], [b], [a, c], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [b], [a, c], [b, c], [c, a], [c, b]]).
%% mrule([[a], [b], [a, c], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [b], [a, c], [b, c], [c, b], [c, b]]).
%% mrule([[a], [b], [a, c], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [b], [a, c], [b, c], [c, d], [c, d]]).
%% mrule([[a], [b], [a, c], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [b], [a, c], [b, c], [c, d], [d, a]]).
%% mrule([[a], [b], [a, c], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [b], [a, c], [b, c], [c, d], [d, b]]).
%% mrule([[a], [b], [a, c], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [b], [a, c], [b, c], [c, d], [d, c]]).
%% mrule([[a], [b], [a, c], [b, c], [d, a], [d, a]]).
%% mrule([[a, b], [b], [a, c], [b, c], [d, a], [d, a]]).
%% mrule([[a], [b], [a, c], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [b], [a, c], [b, c], [d, a], [d, b]]).
%% mrule([[a], [b], [a, c], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [b], [a, c], [b, c], [d, a], [d, c]]).
%% mrule([[a], [b], [a, c], [b, c], [d, b], [d, b]]).
%% mrule([[a, b], [b], [a, c], [b, c], [d, b], [d, b]]).
%% mrule([[a], [b], [a, c], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [b, c], [d, b], [d, c]]).
%% mrule([[a], [b], [a, c], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [b], [a, c], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [b], [a, c], [b, d], [b, d], [c, a]]).
%% mrule([[a], [b], [a, c], [b, d], [b, d], [c, b]]).
%% mrule([[a, b], [b], [a, c], [b, d], [b, d], [c, b]]).
%% mrule([[a], [b], [a, c], [b, d], [b, d], [c, d]]).
%% mrule([[a, b], [b], [a, c], [b, d], [b, d], [c, d]]).
%% mrule([[a], [b], [a, c], [b, d], [b, d], [d, c]]).
%% mrule([[a, b], [b], [a, c], [b, d], [b, d], [d, c]]).
%% mrule([[a], [b], [a, c], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [b], [a, c], [b, d], [c, a], [c, d]]).
%% mrule([[a], [b], [a, c], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [b], [a, c], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [b], [a, c], [b, d], [c, a], [d, b]]).
%% mrule([[a], [b], [a, c], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [b], [a, c], [b, d], [c, a], [d, c]]).
%% mrule([[a], [b], [a, c], [b, d], [c, b], [c, d]]).
%% mrule([[a, b], [b], [a, c], [b, d], [c, b], [c, d]]).
%% mrule([[a], [b], [a, c], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [b], [a, c], [b, d], [c, b], [d, a]]).
%% mrule([[a], [b], [a, c], [b, d], [c, b], [d, b]]).
%% mrule([[a, b], [b], [a, c], [b, d], [c, b], [d, b]]).
%% mrule([[a], [b], [a, c], [b, d], [c, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [b, d], [c, b], [d, c]]).
%% mrule([[a], [b], [a, c], [b, d], [c, d], [c, d]]).
%% mrule([[a, b], [b], [a, c], [b, d], [c, d], [c, d]]).
%% mrule([[a], [b], [a, c], [b, d], [c, d], [d, a]]).
%% mrule([[a, b], [b], [a, c], [b, d], [c, d], [d, a]]).
%% mrule([[a], [b], [a, c], [b, d], [c, d], [d, b]]).
%% mrule([[a, b], [b], [a, c], [b, d], [c, d], [d, b]]).
%% mrule([[a], [b], [a, c], [b, d], [c, d], [d, c]]).
%% mrule([[a, b], [b], [a, c], [b, d], [c, d], [d, c]]).
%% mrule([[a], [b], [a, c], [b, d], [c, e], [d, e]]).
%% mrule([[a, b], [b], [a, c], [b, d], [c, e], [d, e]]).
%% mrule([[a], [b], [a, c], [b, d], [c, e], [e, d]]).
%% mrule([[a, b], [b], [a, c], [b, d], [c, e], [e, d]]).
%% mrule([[a], [b], [a, c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [b], [a, c], [b, d], [d, a], [d, c]]).
%% mrule([[a], [b], [a, c], [b, d], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [b, d], [d, b], [d, c]]).
%% mrule([[a], [b], [a, c], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [b], [a, c], [b, d], [d, c], [d, c]]).
%% mrule([[a], [b], [a, c], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [b], [a, c], [b, d], [d, e], [e, c]]).
%% mrule([[a], [b], [a, c], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [b], [a, c], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [b], [a, c], [c, a], [c, a], [c, a]]).
%% mrule([[a], [b], [a, c], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [b], [a, c], [c, a], [c, a], [c, b]]).
%% mrule([[a], [b], [a, c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b], [a, c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b], [a, c], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [b], [a, c], [c, a], [c, d], [d, a]]).
%% mrule([[a], [b], [a, c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b], [a, c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b], [a, c], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [b], [a, c], [c, a], [d, a], [d, a]]).
%% mrule([[a], [b], [a, c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [b], [a, c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [b], [a, c], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [b], [a, c], [c, a], [d, b], [d, b]]).
%% mrule([[a], [b], [a, c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [c, a], [d, c], [d, c]]).
%% mrule([[a], [b], [a, c], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [b], [a, c], [c, b], [c, b], [c, b]]).
%% mrule([[a], [b], [a, c], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [b], [a, c], [c, b], [c, d], [c, d]]).
%% mrule([[a], [b], [a, c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b], [a, c], [c, b], [c, d], [d, a]]).
%% mrule([[a], [b], [a, c], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [b], [a, c], [c, b], [c, d], [d, b]]).
%% mrule([[a], [b], [a, c], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [b], [a, c], [c, b], [c, d], [d, c]]).
%% mrule([[a], [b], [a, c], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [b], [a, c], [c, b], [d, a], [d, a]]).
%% mrule([[a], [b], [a, c], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [b], [a, c], [c, b], [d, a], [d, b]]).
%% mrule([[a], [b], [a, c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b], [a, c], [c, b], [d, a], [d, c]]).
%% mrule([[a], [b], [a, c], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [b], [a, c], [c, b], [d, b], [d, b]]).
%% mrule([[a], [b], [a, c], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [c, b], [d, b], [d, c]]).
%% mrule([[a], [b], [a, c], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [b], [a, c], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [b], [a, c], [c, d], [c, d], [c, d]]).
%% mrule([[a, b], [b], [a, c], [c, d], [c, d], [d, a]]).
%% mrule([[a], [b], [a, c], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [b], [a, c], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [b], [a, c], [c, d], [c, d], [d, c]]).
%% mrule([[a, b], [b], [a, c], [c, d], [c, e], [d, e]]).
%% mrule([[a, b], [b], [a, c], [c, d], [c, e], [e, d]]).
%% mrule([[a, b], [b], [a, c], [c, d], [d, a], [d, a]]).
%% mrule([[a], [b], [a, c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b], [a, c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b], [a, c], [c, d], [d, a], [d, c]]).
%% mrule([[a], [b], [a, c], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [b], [a, c], [c, d], [d, b], [d, b]]).
%% mrule([[a], [b], [a, c], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [c, d], [d, c], [d, c]]).
%% mrule([[a, b], [b], [a, c], [c, d], [d, e], [d, e]]).
%% mrule([[a, b], [b], [a, c], [c, d], [d, e], [e, a]]).
%% mrule([[a], [b], [a, c], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [b], [a, c], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [b], [a, c], [c, d], [d, e], [e, c]]).
%% mrule([[a, b], [b], [a, c], [c, d], [d, e], [e, d]]).
%% mrule([[a, b], [b], [a, c], [c, d], [e, a], [e, d]]).
%% mrule([[a], [b], [a, c], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [b], [a, c], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [b], [a, c], [c, d], [e, c], [e, d]]).
%% mrule([[a, b], [b], [a, c], [c, d], [e, d], [e, d]]).
%% mrule([[a, b], [b], [a, c], [d, a], [d, a], [d, c]]).
%% mrule([[a], [b], [a, c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [b], [a, c], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [b], [a, c], [d, a], [e, c], [e, d]]).
%% mrule([[a], [b], [a, c], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [b], [a, c], [d, b], [d, b], [d, c]]).
%% mrule([[a], [b], [a, c], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [b], [a, c], [d, b], [d, c], [d, c]]).
%% mrule([[a], [b], [a, c], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [b], [a, c], [d, b], [d, e], [e, c]]).
%% mrule([[a], [b], [a, c], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [b], [a, c], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [b], [a, c], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [b], [a, c], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [b], [a, c], [d, c], [d, e], [e, a]]).
%% mrule([[a], [b], [a, c], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [b], [a, c], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [b], [a, c], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [b], [a, c], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [b], [a, c], [d, c], [e, a], [e, d]]).
%% mrule([[a], [b], [a, c], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [b], [a, c], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [b], [a, c], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [b], [a, c], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [b], [a, c], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [b], [a, c], [d, e], [e, c], [e, d]]).
%% mrule([[a, b], [b], [a, c], [d, a], [d, e], [c, e]]).
%% mrule([[a, b], [b], [a, c], [d, a], [c, e], [e, d]]).
%% mrule([[a], [b], [a, c], [d, b], [d, e], [c, e]]).
%% mrule([[a, b], [b], [a, c], [d, b], [d, e], [c, e]]).
%% mrule([[a], [b], [a, c], [d, b], [c, e], [e, d]]).
%% mrule([[a, b], [b], [a, c], [d, b], [c, e], [e, d]]).
%% mrule([[a, b], [b], [a, c], [d, c], [d, e], [c, e]]).
%% mrule([[a, b], [b], [a, c], [d, c], [c, e], [e, d]]).
%% mrule([[a, b], [b], [a, c], [d, e], [d, e], [c, e]]).
%% mrule([[a, b], [b], [a, c], [d, e], [c, e], [e, d]]).
%% mrule([[a], [b], [b, a], [b, a], [b, a], [b, a]]).
%% mrule([[a, b], [b], [b, a], [b, a], [b, a], [b, a]]).
%% mrule([[a], [b], [b, a], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [b], [b, a], [b, a], [b, c], [b, c]]).
%% mrule([[a], [b], [b, a], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [b], [b, a], [b, a], [b, c], [c, a]]).
%% mrule([[a], [b], [b, a], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [b], [b, a], [b, a], [b, c], [c, b]]).
%% mrule([[a], [b], [b, a], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [b], [b, a], [b, a], [c, a], [c, a]]).
%% mrule([[a], [b], [b, a], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [b], [b, a], [b, a], [c, a], [c, b]]).
%% mrule([[a], [b], [b, a], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [b], [b, a], [b, a], [c, b], [c, b]]).
%% mrule([[a], [b], [b, a], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [b], [b, a], [b, c], [b, c], [b, c]]).
%% mrule([[a], [b], [b, a], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [b], [b, a], [b, c], [b, c], [c, a]]).
%% mrule([[a], [b], [b, a], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [b], [b, a], [b, c], [b, c], [c, b]]).
%% mrule([[a], [b], [b, a], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [b], [b, a], [b, c], [b, d], [c, d]]).
%% mrule([[a], [b], [b, a], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [b], [b, a], [b, c], [b, d], [d, c]]).
%% mrule([[a], [b], [b, a], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [b], [b, a], [b, c], [c, a], [c, a]]).
%% mrule([[a], [b], [b, a], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [b], [b, a], [b, c], [c, a], [c, b]]).
%% mrule([[a], [b], [b, a], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [b], [b, a], [b, c], [c, b], [c, b]]).
%% mrule([[a], [b], [b, a], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [b], [b, a], [b, c], [c, d], [c, d]]).
%% mrule([[a], [b], [b, a], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [b], [b, a], [b, c], [c, d], [d, a]]).
%% mrule([[a], [b], [b, a], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [b], [b, a], [b, c], [c, d], [d, b]]).
%% mrule([[a], [b], [b, a], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [b], [b, a], [b, c], [c, d], [d, c]]).
%% mrule([[a], [b], [b, a], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [b], [b, a], [b, c], [d, a], [d, c]]).
%% mrule([[a], [b], [b, a], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [b], [b, a], [b, c], [d, b], [d, c]]).
%% mrule([[a], [b], [b, a], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [b], [b, a], [b, c], [d, c], [d, c]]).
%% mrule([[a], [b], [b, a], [c, a], [c, a], [c, a]]).
%% mrule([[a, b], [b], [b, a], [c, a], [c, a], [c, a]]).
%% mrule([[a], [b], [b, a], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [b], [b, a], [c, a], [c, a], [c, b]]).
%% mrule([[a], [b], [b, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b], [b, a], [c, a], [c, b], [c, b]]).
%% mrule([[a], [b], [b, a], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [b], [b, a], [c, a], [c, d], [c, d]]).
%% mrule([[a], [b], [b, a], [c, a], [c, d], [d, a]]).
%% mrule([[a, b], [b], [b, a], [c, a], [c, d], [d, a]]).
%% mrule([[a], [b], [b, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b], [b, a], [c, a], [c, d], [d, b]]).
%% mrule([[a], [b], [b, a], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [b], [b, a], [c, a], [c, d], [d, c]]).
%% mrule([[a], [b], [b, a], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [b], [b, a], [c, a], [d, a], [d, c]]).
%% mrule([[a], [b], [b, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [b, a], [c, a], [d, b], [d, c]]).
%% mrule([[a], [b], [b, a], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [b], [b, a], [c, a], [d, c], [d, c]]).
%% mrule([[a], [b], [b, a], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [b], [b, a], [c, b], [c, b], [c, b]]).
%% mrule([[a], [b], [b, a], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [b], [b, a], [c, b], [c, d], [c, d]]).
%% mrule([[a], [b], [b, a], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b], [b, a], [c, b], [c, d], [d, a]]).
%% mrule([[a], [b], [b, a], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [b], [b, a], [c, b], [c, d], [d, b]]).
%% mrule([[a], [b], [b, a], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [b], [b, a], [c, b], [c, d], [d, c]]).
%% mrule([[a], [b], [b, a], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b], [b, a], [c, b], [d, a], [d, c]]).
%% mrule([[a], [b], [b, a], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [b], [b, a], [c, b], [d, b], [d, c]]).
%% mrule([[a], [b], [b, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [b], [b, a], [c, b], [d, c], [d, c]]).
%% mrule([[a], [b], [b, a], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [b], [b, a], [c, d], [c, d], [d, a]]).
%% mrule([[a], [b], [b, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [b], [b, a], [c, d], [c, d], [d, b]]).
%% mrule([[a], [b], [b, a], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [b], [b, a], [c, d], [d, a], [d, c]]).
%% mrule([[a], [b], [b, a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [b], [b, a], [c, d], [d, b], [d, c]]).
%% mrule([[a], [b], [b, c], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [b], [b, c], [b, c], [b, c], [c, a]]).
%% mrule([[a], [b], [b, c], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [b], [b, c], [b, c], [b, d], [d, a]]).
%% mrule([[a], [b], [b, c], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [b], [b, c], [b, c], [c, a], [c, a]]).
%% mrule([[a], [b], [b, c], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [b], [b, c], [b, c], [c, a], [c, b]]).
%% mrule([[a], [b], [b, c], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [b], [b, c], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [b], [b, c], [b, c], [d, a], [d, a]]).
%% mrule([[a], [b], [b, c], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [b], [b, c], [b, c], [d, a], [d, b]]).
%% mrule([[a], [b], [b, c], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [b], [b, c], [b, c], [d, a], [d, c]]).
%% mrule([[a], [b], [b, c], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [b], [b, c], [b, d], [b, d], [c, a]]).
%% mrule([[a], [b], [b, c], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [b], [b, c], [b, d], [c, a], [c, d]]).
%% mrule([[a], [b], [b, c], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [b], [b, c], [b, d], [c, a], [d, a]]).
%% mrule([[a], [b], [b, c], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [b], [b, c], [b, d], [c, a], [d, b]]).
%% mrule([[a], [b], [b, c], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [b], [b, c], [b, d], [c, a], [d, c]]).
%% mrule([[a], [b], [b, c], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [b], [b, c], [b, d], [c, b], [d, a]]).
%% mrule([[a], [b], [b, c], [b, d], [c, d], [d, a]]).
%% mrule([[a, b], [b], [b, c], [b, d], [c, d], [d, a]]).
%% mrule([[a], [b], [b, c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [b], [b, c], [b, d], [d, a], [d, c]]).
%% mrule([[a], [b], [b, c], [c, a], [c, a], [c, a]]).
%% mrule([[a, b], [b], [b, c], [c, a], [c, a], [c, a]]).
%% mrule([[a], [b], [b, c], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [b], [b, c], [c, a], [c, a], [c, b]]).
%% mrule([[a], [b], [b, c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b], [b, c], [c, a], [c, b], [c, b]]).
%% mrule([[a], [b], [b, c], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [b], [b, c], [c, a], [c, d], [c, d]]).
%% mrule([[a], [b], [b, c], [c, a], [c, d], [d, a]]).
%% mrule([[a, b], [b], [b, c], [c, a], [c, d], [d, a]]).
%% mrule([[a], [b], [b, c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b], [b, c], [c, a], [c, d], [d, b]]).
%% mrule([[a], [b], [b, c], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [b], [b, c], [c, a], [c, d], [d, c]]).
%% mrule([[a], [b], [b, c], [c, a], [d, a], [d, a]]).
%% mrule([[a, b], [b], [b, c], [c, a], [d, a], [d, a]]).
%% mrule([[a], [b], [b, c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [b], [b, c], [c, a], [d, a], [d, b]]).
%% mrule([[a], [b], [b, c], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [b], [b, c], [c, a], [d, a], [d, c]]).
%% mrule([[a], [b], [b, c], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [b], [b, c], [c, a], [d, b], [d, b]]).
%% mrule([[a], [b], [b, c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [b, c], [c, a], [d, b], [d, c]]).
%% mrule([[a], [b], [b, c], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [b], [b, c], [c, a], [d, c], [d, c]]).
%% mrule([[a], [b], [b, c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b], [b, c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b], [b, c], [c, b], [d, a], [d, a]]).
%% mrule([[a], [b], [b, c], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [b], [b, c], [c, b], [d, a], [d, b]]).
%% mrule([[a], [b], [b, c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b], [b, c], [c, b], [d, a], [d, c]]).
%% mrule([[a], [b], [b, c], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [b], [b, c], [c, d], [c, d], [d, a]]).
%% mrule([[a], [b], [b, c], [c, d], [d, a], [d, a]]).
%% mrule([[a, b], [b], [b, c], [c, d], [d, a], [d, a]]).
%% mrule([[a], [b], [b, c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b], [b, c], [c, d], [d, a], [d, b]]).
%% mrule([[a], [b], [b, c], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [b], [b, c], [c, d], [d, a], [d, c]]).
%% mrule([[a], [b], [b, c], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [b], [b, c], [c, d], [d, e], [e, a]]).
%% mrule([[a], [b], [b, c], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [b], [b, c], [c, d], [e, a], [e, d]]).
%% mrule([[a], [b], [b, c], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [b], [b, c], [d, a], [d, a], [d, c]]).
%% mrule([[a], [b], [b, c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [b, c], [d, a], [d, b], [d, c]]).
%% mrule([[a], [b], [b, c], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [b], [b, c], [d, a], [d, c], [d, c]]).
%% mrule([[a], [b], [b, c], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [b], [b, c], [d, a], [d, e], [e, c]]).
%% mrule([[a], [b], [b, c], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [b], [b, c], [d, a], [e, c], [e, d]]).
%% mrule([[a], [b], [b, c], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [b], [b, c], [d, c], [d, e], [e, a]]).
%% mrule([[a], [b], [b, c], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [b], [b, c], [d, c], [e, a], [e, d]]).
%% mrule([[a], [b], [b, c], [d, a], [d, e], [c, e]]).
%% mrule([[a, b], [b], [b, c], [d, a], [d, e], [c, e]]).
%% mrule([[a], [b], [b, c], [d, a], [c, e], [e, d]]).
%% mrule([[a, b], [b], [b, c], [d, a], [c, e], [e, d]]).
%% mrule([[a, b], [b], [c, a], [c, a], [c, a], [c, a]]).
%% mrule([[a], [b], [c, a], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [b], [c, a], [c, a], [c, a], [c, b]]).
%% mrule([[a], [b], [c, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b], [c, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b], [c, a], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [b], [c, a], [c, a], [c, d], [d, a]]).
%% mrule([[a], [b], [c, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b], [c, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b], [c, a], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [b], [c, a], [c, a], [d, a], [d, a]]).
%% mrule([[a], [b], [c, a], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [b], [c, a], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [b], [c, a], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c, a], [c, a], [d, b], [d, b]]).
%% mrule([[a], [b], [c, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c, a], [c, a], [d, c], [d, c]]).
%% mrule([[a], [b], [c, a], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [b], [c, a], [c, b], [c, b], [c, b]]).
%% mrule([[a], [b], [c, a], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [b], [c, a], [c, b], [c, d], [c, d]]).
%% mrule([[a], [b], [c, a], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b], [c, a], [c, b], [c, d], [d, a]]).
%% mrule([[a], [b], [c, a], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [b], [c, a], [c, b], [c, d], [d, b]]).
%% mrule([[a], [b], [c, a], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [b], [c, a], [c, b], [c, d], [d, c]]).
%% mrule([[a], [b], [c, a], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [b], [c, a], [c, b], [d, a], [d, a]]).
%% mrule([[a], [b], [c, a], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [b], [c, a], [c, b], [d, a], [d, b]]).
%% mrule([[a], [b], [c, a], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c, a], [c, b], [d, a], [d, c]]).
%% mrule([[a], [b], [c, a], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [b], [c, a], [c, b], [d, b], [d, b]]).
%% mrule([[a], [b], [c, a], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c, a], [c, b], [d, b], [d, c]]).
%% mrule([[a], [b], [c, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [b], [c, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [b], [c, a], [c, d], [c, d], [c, d]]).
%% mrule([[a, b], [b], [c, a], [c, d], [c, d], [d, a]]).
%% mrule([[a], [b], [c, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [b], [c, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [b], [c, a], [c, d], [c, d], [d, c]]).
%% mrule([[a, b], [b], [c, a], [c, d], [c, e], [d, e]]).
%% mrule([[a, b], [b], [c, a], [c, d], [c, e], [e, d]]).
%% mrule([[a, b], [b], [c, a], [c, d], [d, a], [d, a]]).
%% mrule([[a], [b], [c, a], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b], [c, a], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b], [c, a], [c, d], [d, a], [d, c]]).
%% mrule([[a], [b], [c, a], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [b], [c, a], [c, d], [d, b], [d, b]]).
%% mrule([[a], [b], [c, a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c, a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c, a], [c, d], [d, c], [d, c]]).
%% mrule([[a, b], [b], [c, a], [c, d], [d, e], [d, e]]).
%% mrule([[a, b], [b], [c, a], [c, d], [d, e], [e, a]]).
%% mrule([[a], [b], [c, a], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [b], [c, a], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [b], [c, a], [c, d], [d, e], [e, c]]).
%% mrule([[a, b], [b], [c, a], [c, d], [d, e], [e, d]]).
%% mrule([[a, b], [b], [c, a], [c, d], [e, a], [e, d]]).
%% mrule([[a], [b], [c, a], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [b], [c, a], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [b], [c, a], [c, d], [e, c], [e, d]]).
%% mrule([[a, b], [b], [c, a], [c, d], [e, d], [e, d]]).
%% mrule([[a, b], [b], [c, a], [d, a], [d, a], [d, c]]).
%% mrule([[a], [b], [c, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c, a], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [b], [c, a], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [b], [c, a], [d, a], [e, c], [e, d]]).
%% mrule([[a], [b], [c, a], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c, a], [d, b], [d, b], [d, c]]).
%% mrule([[a], [b], [c, a], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [b], [c, a], [d, b], [d, c], [d, c]]).
%% mrule([[a], [b], [c, a], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [b], [c, a], [d, b], [d, e], [e, c]]).
%% mrule([[a], [b], [c, a], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [b], [c, a], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [b], [c, a], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [b], [c, a], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [b], [c, a], [d, c], [d, e], [e, a]]).
%% mrule([[a], [b], [c, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [b], [c, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [b], [c, a], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [b], [c, a], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [b], [c, a], [d, c], [e, a], [e, d]]).
%% mrule([[a], [b], [c, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [b], [c, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [b], [c, a], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [b], [c, a], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [b], [c, a], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [b], [c, a], [d, e], [e, c], [e, d]]).
%% mrule([[a], [b], [c, b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b], [c, b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b], [c, b], [c, b], [d, a], [d, a]]).
%% mrule([[a], [b], [c, b], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [b], [c, b], [c, b], [d, a], [d, b]]).
%% mrule([[a], [b], [c, b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c, b], [c, b], [d, a], [d, c]]).
%% mrule([[a], [b], [c, b], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [b], [c, b], [c, d], [c, d], [d, a]]).
%% mrule([[a], [b], [c, b], [c, d], [d, a], [d, a]]).
%% mrule([[a, b], [b], [c, b], [c, d], [d, a], [d, a]]).
%% mrule([[a], [b], [c, b], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b], [c, b], [c, d], [d, a], [d, b]]).
%% mrule([[a], [b], [c, b], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c, b], [c, d], [d, a], [d, c]]).
%% mrule([[a], [b], [c, b], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [b], [c, b], [c, d], [d, e], [e, a]]).
%% mrule([[a], [b], [c, b], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [b], [c, b], [c, d], [e, a], [e, d]]).
%% mrule([[a], [b], [c, b], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c, b], [d, a], [d, a], [d, c]]).
%% mrule([[a], [b], [c, b], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c, b], [d, a], [d, b], [d, c]]).
%% mrule([[a], [b], [c, b], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [b], [c, b], [d, a], [d, c], [d, c]]).
%% mrule([[a], [b], [c, b], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [b], [c, b], [d, a], [d, e], [e, c]]).
%% mrule([[a], [b], [c, b], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [b], [c, b], [d, a], [e, c], [e, d]]).
%% mrule([[a], [b], [c, b], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [b], [c, b], [d, c], [d, e], [e, a]]).
%% mrule([[a], [b], [c, b], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [b], [c, b], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [b], [c, d], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [b], [c, d], [c, d], [c, e], [e, a]]).
%% mrule([[a, b], [b], [c, d], [c, d], [d, a], [d, a]]).
%% mrule([[a], [b], [c, d], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b], [c, d], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b], [c, d], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [b], [c, d], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [b], [c, d], [c, d], [e, a], [e, c]]).
%% mrule([[a, b], [b], [c, d], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [b], [c, d], [c, e], [c, e], [d, a]]).
%% mrule([[a, b], [b], [c, d], [c, e], [d, a], [d, e]]).
%% mrule([[a, b], [b], [c, d], [c, e], [d, a], [e, a]]).
%% mrule([[a], [b], [c, d], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [b], [c, d], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [b], [c, d], [c, e], [d, a], [e, c]]).
%% mrule([[a, b], [b], [c, d], [c, e], [d, a], [e, d]]).
%% mrule([[a], [b], [c, d], [c, e], [d, b], [e, a]]).
%% mrule([[a, b], [b], [c, d], [c, e], [d, b], [e, a]]).
%% mrule([[a, b], [b], [c, d], [c, e], [d, c], [e, a]]).
%% mrule([[a, b], [b], [c, d], [c, e], [d, e], [e, a]]).
%% mrule([[a, b], [b], [c, d], [c, e], [e, a], [e, d]]).
%% mrule([[a, b], [b], [c, d], [d, a], [d, a], [d, c]]).
%% mrule([[a], [b], [c, d], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c, d], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b], [c, d], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [b], [c, d], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [b], [c, d], [d, a], [e, a], [e, c]]).
%% mrule([[a], [b], [c, d], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [b], [c, d], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [b], [c, d], [d, a], [e, c], [e, c]]).
%% mrule([[a, b], [b], [c, d], [d, a], [e, c], [e, d]]).
%% mrule([[a], [b], [c, d], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [b], [c, d], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [b], [c, d], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [b], [c, d], [d, c], [e, a], [e, c]]).
%% mrule([[a, b], [b], [c, d], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [b], [c, d], [d, e], [e, a], [e, c]]).
%% mrule([[a, b], [b], [c, d], [e, a], [e, c], [e, d]]).
%% mrule([[a, b], [c], [c], [c], [a, b], [a, c]]).
%% mrule([[a, b], [c], [c], [c], [a, b], [b, c]]).
%% mrule([[a, b], [c], [c], [c], [a, b], [c, a]]).
%% mrule([[a, b], [c], [c], [c], [a, b], [c, b]]).
%% mrule([[a, b], [c], [c], [c], [a, c], [b, a]]).
%% mrule([[a, b], [c], [c], [c], [a, c], [b, c]]).
%% mrule([[a, b], [c], [c], [c], [a, c], [c, b]]).
%% mrule([[a, b], [c], [c], [c], [b, a], [b, c]]).
%% mrule([[a, b], [c], [c], [c], [b, a], [c, a]]).
%% mrule([[a, b], [c], [c], [c], [b, a], [c, b]]).
%% mrule([[a, b], [c], [c], [c], [b, c], [c, a]]).
%% mrule([[a, b], [c], [c], [c], [c, a], [c, b]]).
%% mrule([[a, b], [c], [c], [d], [a, c], [b, d]]).
%% mrule([[a, b], [c], [c], [d], [a, c], [d, b]]).
%% mrule([[a, b], [c], [c], [d], [a, d], [b, c]]).
%% mrule([[a, b], [c], [c], [d], [a, d], [c, b]]).
%% mrule([[a, b], [c], [c], [d], [b, c], [d, a]]).
%% mrule([[a, b], [c], [c], [d], [b, d], [c, a]]).
%% mrule([[a, b], [c], [c], [d], [c, a], [d, b]]).
%% mrule([[a, b], [c], [c], [d], [c, b], [d, a]]).
%% mrule([[a, b], [c], [c], [a, b], [a, b], [a, c]]).
%% mrule([[a, b], [c], [c], [a, b], [a, b], [b, c]]).
%% mrule([[a, b], [c], [c], [a, b], [a, b], [c, a]]).
%% mrule([[a, b], [c], [c], [a, b], [a, b], [c, b]]).
%% mrule([[a, b], [c], [c], [a, b], [a, c], [a, c]]).
%% mrule([[a, b], [c], [c], [a, b], [a, c], [b, a]]).
%% mrule([[a, b], [c], [c], [a, b], [a, c], [b, c]]).
%% mrule([[a, b], [c], [c], [a, b], [a, c], [c, a]]).
%% mrule([[a, b], [c], [c], [a, b], [a, c], [c, b]]).
%% mrule([[a, b], [c], [c], [a, b], [a, d], [c, d]]).
%% mrule([[a, b], [c], [c], [a, b], [a, d], [d, c]]).
%% mrule([[a, b], [c], [c], [a, b], [b, a], [b, c]]).
%% mrule([[a, b], [c], [c], [a, b], [b, a], [c, a]]).
%% mrule([[a, b], [c], [c], [a, b], [b, a], [c, b]]).
%% mrule([[a, b], [c], [c], [a, b], [b, c], [b, c]]).
%% mrule([[a, b], [c], [c], [a, b], [b, c], [c, a]]).
%% mrule([[a, b], [c], [c], [a, b], [b, c], [c, b]]).
%% mrule([[a, b], [c], [c], [a, b], [b, d], [c, d]]).
%% mrule([[a, b], [c], [c], [a, b], [b, d], [d, c]]).
%% mrule([[a, b], [c], [c], [a, b], [c, a], [c, a]]).
%% mrule([[a, b], [c], [c], [a, b], [c, a], [c, b]]).
%% mrule([[a, b], [c], [c], [a, b], [c, b], [c, b]]).
%% mrule([[a, b], [c], [c], [a, b], [c, d], [d, a]]).
%% mrule([[a, b], [c], [c], [a, b], [c, d], [d, b]]).
%% mrule([[a, b], [c], [c], [a, b], [d, a], [d, c]]).
%% mrule([[a, b], [c], [c], [a, b], [d, b], [d, c]]).
%% mrule([[a, b], [c], [c], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [c], [c], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [c], [c], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [c], [c], [a, c], [a, d], [b, d]]).
%% mrule([[a, b], [c], [c], [a, c], [a, d], [d, b]]).
%% mrule([[a, b], [c], [c], [a, c], [b, a], [b, a]]).
%% mrule([[a, b], [c], [c], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [c], [c], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [c], [c], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [c], [c], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [c], [c], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [c], [c], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [c], [c], [a, c], [b, d], [b, d]]).
%% mrule([[a, b], [c], [c], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [c], [c], [a, c], [b, d], [d, a]]).
%% mrule([[a, b], [c], [c], [a, c], [b, d], [d, b]]).
%% mrule([[a, b], [c], [c], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [c], [c], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [c], [c], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [c], [c], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [c], [c], [a, c], [d, a], [d, b]]).
%% mrule([[a, b], [c], [c], [a, c], [d, b], [d, b]]).
%% mrule([[a, b], [c], [c], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [c], [c], [a, d], [a, d], [b, c]]).
%% mrule([[a, b], [c], [c], [a, d], [a, d], [c, b]]).
%% mrule([[a, b], [c], [c], [a, d], [b, a], [c, d]]).
%% mrule([[a, b], [c], [c], [a, d], [b, a], [d, c]]).
%% mrule([[a, b], [c], [c], [a, d], [b, c], [b, d]]).
%% mrule([[a, b], [c], [c], [a, d], [b, c], [c, d]]).
%% mrule([[a, b], [c], [c], [a, d], [b, c], [d, a]]).
%% mrule([[a, b], [c], [c], [a, d], [b, c], [d, b]]).
%% mrule([[a, b], [c], [c], [a, d], [b, c], [d, c]]).
%% mrule([[a, b], [c], [c], [a, d], [b, d], [c, a]]).
%% mrule([[a, b], [c], [c], [a, d], [b, d], [c, b]]).
%% mrule([[a, b], [c], [c], [a, d], [b, d], [c, d]]).
%% mrule([[a, b], [c], [c], [a, d], [b, d], [d, c]]).
%% mrule([[a, b], [c], [c], [a, d], [c, a], [d, b]]).
%% mrule([[a, b], [c], [c], [a, d], [c, b], [c, d]]).
%% mrule([[a, b], [c], [c], [a, d], [c, b], [d, a]]).
%% mrule([[a, b], [c], [c], [a, d], [c, b], [d, b]]).
%% mrule([[a, b], [c], [c], [a, d], [c, b], [d, c]]).
%% mrule([[a, b], [c], [c], [a, d], [c, d], [d, b]]).
%% mrule([[a, b], [c], [c], [a, d], [d, b], [d, c]]).
%% mrule([[a, b], [c], [c], [b, a], [b, a], [b, c]]).
%% mrule([[a, b], [c], [c], [b, a], [b, a], [c, a]]).
%% mrule([[a, b], [c], [c], [b, a], [b, a], [c, b]]).
%% mrule([[a, b], [c], [c], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [c], [c], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [c], [c], [b, a], [b, c], [c, b]]).
%% mrule([[a], [b], [b], [c, a], [c, d], [b, d]]).
%% mrule([[a, b], [c], [c], [b, a], [b, d], [c, d]]).
%% mrule([[a, b], [c], [c], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [c], [c], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [c], [c], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [c], [c], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [c], [c], [b, a], [c, d], [d, a]]).
%% mrule([[a], [b], [b], [c, a], [b, d], [d, c]]).
%% mrule([[a, b], [c], [c], [b, a], [c, d], [d, b]]).
%% mrule([[a, b], [c], [c], [b, a], [d, a], [d, c]]).
%% mrule([[a, b], [c], [c], [b, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [c], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [c], [c], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [c], [c], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [c], [c], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [c], [c], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [c], [c], [b, c], [d, a], [d, a]]).
%% mrule([[a, b], [c], [c], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [c], [c], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [c], [c], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [c], [c], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [c], [c], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [c], [c], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [c], [c], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [c], [c], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [c], [c], [b, d], [c, d], [d, a]]).
%% mrule([[a, b], [c], [c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [c], [c], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [c], [c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [c], [c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [c], [c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [c], [c], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [c], [c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [c], [c], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [c], [c], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [c], [c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [c], [c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c], [c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [d], [d], [a, c], [b, d]]).
%% mrule([[a, b], [c], [d], [d], [a, c], [d, b]]).
%% mrule([[a, b], [c], [d], [d], [a, d], [b, c]]).
%% mrule([[a, b], [c], [d], [d], [a, d], [c, b]]).
%% mrule([[a, b], [c], [d], [d], [b, c], [d, a]]).
%% mrule([[a, b], [c], [d], [d], [b, d], [c, a]]).
%% mrule([[a, b], [c], [d], [d], [c, a], [d, b]]).
%% mrule([[a, b], [c], [d], [d], [c, b], [d, a]]).
%% mrule([[a, b], [c], [d], [a, b], [a, c], [a, d]]).
%% mrule([[a, b], [c], [d], [a, b], [a, c], [b, d]]).
%% mrule([[a, b], [c], [d], [a, b], [a, c], [c, d]]).
%% mrule([[a, b], [c], [d], [a, b], [a, c], [d, a]]).
%% mrule([[a, b], [c], [d], [a, b], [a, c], [d, b]]).
%% mrule([[a, b], [c], [d], [a, b], [a, c], [d, c]]).
%% mrule([[a, b], [c], [d], [a, b], [a, d], [b, c]]).
%% mrule([[a, b], [c], [d], [a, b], [a, d], [c, a]]).
%% mrule([[a, b], [c], [d], [a, b], [a, d], [c, b]]).
%% mrule([[a, b], [c], [d], [a, b], [a, d], [c, d]]).
%% mrule([[a, b], [c], [d], [a, b], [a, d], [d, c]]).
%% mrule([[a, b], [c], [d], [a, b], [b, c], [b, d]]).
%% mrule([[a, b], [c], [d], [a, b], [b, c], [c, d]]).
%% mrule([[a, b], [c], [d], [a, b], [b, c], [d, a]]).
%% mrule([[a, b], [c], [d], [a, b], [b, c], [d, b]]).
%% mrule([[a, b], [c], [d], [a, b], [b, c], [d, c]]).
%% mrule([[a, b], [c], [d], [a, b], [b, d], [c, a]]).
%% mrule([[a, b], [c], [d], [a, b], [b, d], [c, b]]).
%% mrule([[a, b], [c], [d], [a, b], [b, d], [c, d]]).
%% mrule([[a, b], [c], [d], [a, b], [b, d], [d, c]]).
%% mrule([[a, b], [c], [d], [a, b], [c, a], [c, d]]).
%% mrule([[a, b], [c], [d], [a, b], [c, a], [d, a]]).
%% mrule([[a, b], [c], [d], [a, b], [c, a], [d, b]]).
%% mrule([[a, b], [c], [d], [a, b], [c, a], [d, c]]).
%% mrule([[a, b], [c], [d], [a, b], [c, b], [c, d]]).
%% mrule([[a, b], [c], [d], [a, b], [c, b], [d, a]]).
%% mrule([[a, b], [c], [d], [a, b], [c, b], [d, b]]).
%% mrule([[a, b], [c], [d], [a, b], [c, b], [d, c]]).
%% mrule([[a, b], [c], [d], [a, b], [c, d], [d, a]]).
%% mrule([[a, b], [c], [d], [a, b], [c, d], [d, b]]).
%% mrule([[a, b], [c], [d], [a, b], [d, a], [d, c]]).
%% mrule([[a, b], [c], [d], [a, b], [d, b], [d, c]]).
%% mrule([[a, b], [c], [d], [a, c], [a, c], [b, d]]).
%% mrule([[a, b], [c], [d], [a, c], [a, c], [d, b]]).
%% mrule([[a, b], [c], [d], [a, c], [a, d], [b, a]]).
%% mrule([[a, b], [c], [d], [a, c], [a, d], [b, c]]).
%% mrule([[a, b], [c], [d], [a, c], [a, d], [b, d]]).
%% mrule([[a, b], [c], [d], [a, c], [a, d], [c, b]]).
%% mrule([[a, b], [c], [d], [a, c], [a, d], [d, b]]).
%% mrule([[a, b], [c], [d], [a, c], [b, a], [b, d]]).
%% mrule([[a, b], [c], [d], [a, c], [b, a], [c, d]]).
%% mrule([[a, b], [c], [d], [a, c], [b, a], [d, a]]).
%% mrule([[a, b], [c], [d], [a, c], [b, a], [d, b]]).
%% mrule([[a, b], [c], [d], [a, c], [b, a], [d, c]]).
%% mrule([[a, b], [c], [d], [a, c], [b, c], [b, d]]).
%% mrule([[a, b], [c], [d], [a, c], [b, c], [c, d]]).
%% mrule([[a, b], [c], [d], [a, c], [b, c], [d, a]]).
%% mrule([[a, b], [c], [d], [a, c], [b, c], [d, b]]).
%% mrule([[a, b], [c], [d], [a, c], [b, c], [d, c]]).
%% mrule([[a, b], [c], [d], [a, c], [b, d], [b, d]]).
%% mrule([[a, b], [c], [d], [a, c], [b, d], [c, a]]).
%% mrule([[a, b], [c], [d], [a, c], [b, d], [c, b]]).
%% mrule([[a, b], [c], [d], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [c], [d], [a, c], [b, d], [d, a]]).
%% mrule([[a, b], [c], [d], [a, c], [b, d], [d, b]]).
%% mrule([[a, b], [c], [d], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [c], [d], [a, c], [b, e], [d, e]]).
%% mrule([[a, b], [c], [d], [a, c], [b, e], [e, d]]).
%% mrule([[a, b], [c], [d], [a, c], [c, a], [d, b]]).
%% mrule([[a, b], [c], [d], [a, c], [c, b], [c, d]]).
%% mrule([[a, b], [c], [d], [a, c], [c, b], [d, a]]).
%% mrule([[a, b], [c], [d], [a, c], [c, b], [d, b]]).
%% mrule([[a, b], [c], [d], [a, c], [c, b], [d, c]]).
%% mrule([[a, b], [c], [d], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [c], [d], [a, c], [d, a], [d, b]]).
%% mrule([[a, b], [c], [d], [a, c], [d, b], [d, b]]).
%% mrule([[a, b], [c], [d], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [c], [d], [a, c], [d, e], [e, b]]).
%% mrule([[a, b], [c], [d], [a, c], [e, b], [e, d]]).
%% mrule([[a, b], [c], [d], [a, d], [a, d], [b, c]]).
%% mrule([[a, b], [c], [d], [a, d], [a, d], [c, b]]).
%% mrule([[a, b], [c], [d], [a, d], [b, a], [b, c]]).
%% mrule([[a, b], [c], [d], [a, d], [b, a], [c, a]]).
%% mrule([[a, b], [c], [d], [a, d], [b, a], [c, b]]).
%% mrule([[a, b], [c], [d], [a, d], [b, a], [c, d]]).
%% mrule([[a, b], [c], [d], [a, d], [b, a], [d, c]]).
%% mrule([[a, b], [c], [d], [a, d], [b, c], [b, c]]).
%% mrule([[a, b], [c], [d], [a, d], [b, c], [b, d]]).
%% mrule([[a, b], [c], [d], [a, d], [b, c], [c, a]]).
%% mrule([[a, b], [c], [d], [a, d], [b, c], [c, b]]).
%% mrule([[a, b], [c], [d], [a, d], [b, c], [c, d]]).
%% mrule([[a, b], [c], [d], [a, d], [b, c], [d, a]]).
%% mrule([[a, b], [c], [d], [a, d], [b, c], [d, b]]).
%% mrule([[a, b], [c], [d], [a, d], [b, c], [d, c]]).
%% mrule([[a, b], [c], [d], [a, d], [b, d], [c, a]]).
%% mrule([[a, b], [c], [d], [a, d], [b, d], [c, b]]).
%% mrule([[a, b], [c], [d], [a, d], [b, d], [c, d]]).
%% mrule([[a, b], [c], [d], [a, d], [b, d], [d, c]]).
%% mrule([[a, b], [c], [d], [a, d], [b, e], [c, e]]).
%% mrule([[a, b], [c], [d], [a, d], [b, e], [e, c]]).
%% mrule([[a, b], [c], [d], [a, d], [c, a], [c, b]]).
%% mrule([[a, b], [c], [d], [a, d], [c, a], [d, b]]).
%% mrule([[a, b], [c], [d], [a, d], [c, b], [c, b]]).
%% mrule([[a, b], [c], [d], [a, d], [c, b], [c, d]]).
%% mrule([[a, b], [c], [d], [a, d], [c, b], [d, a]]).
%% mrule([[a, b], [c], [d], [a, d], [c, b], [d, b]]).
%% mrule([[a, b], [c], [d], [a, d], [c, b], [d, c]]).
%% mrule([[a, b], [c], [d], [a, d], [c, d], [d, b]]).
%% mrule([[a, b], [c], [d], [a, d], [c, e], [e, b]]).
%% mrule([[a, b], [c], [d], [a, d], [d, b], [d, c]]).
%% mrule([[a, b], [c], [d], [a, d], [e, b], [e, c]]).
%% mrule([[a, b], [c], [d], [a, e], [b, c], [d, e]]).
%% mrule([[a, b], [c], [d], [a, e], [b, c], [e, d]]).
%% mrule([[a, b], [c], [d], [a, e], [b, d], [c, e]]).
%% mrule([[a, b], [c], [d], [a, e], [b, d], [e, c]]).
%% mrule([[a, b], [c], [d], [a, e], [c, b], [d, e]]).
%% mrule([[a, b], [c], [d], [a, e], [c, b], [e, d]]).
%% mrule([[a, b], [c], [d], [a, e], [c, e], [d, b]]).
%% mrule([[a, b], [c], [d], [a, e], [d, b], [e, c]]).
%% mrule([[a, b], [c], [d], [b, a], [b, c], [b, d]]).
%% mrule([[a, b], [c], [d], [b, a], [b, c], [c, d]]).
%% mrule([[a, b], [c], [d], [b, a], [b, c], [d, a]]).
%% mrule([[a, b], [c], [d], [b, a], [b, c], [d, b]]).
%% mrule([[a, b], [c], [d], [b, a], [b, c], [d, c]]).
%% mrule([[a, b], [c], [d], [b, a], [b, d], [c, a]]).
%% mrule([[a, b], [c], [d], [b, a], [b, d], [c, b]]).
%% mrule([[a, b], [c], [d], [b, a], [b, d], [c, d]]).
%% mrule([[a, b], [c], [d], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [c], [d], [b, a], [c, a], [c, d]]).
%% mrule([[a, b], [c], [d], [b, a], [c, a], [d, a]]).
%% mrule([[a, b], [c], [d], [b, a], [c, a], [d, b]]).
%% mrule([[a, b], [c], [d], [b, a], [c, a], [d, c]]).
%% mrule([[a, b], [c], [d], [b, a], [c, b], [c, d]]).
%% mrule([[a, b], [c], [d], [b, a], [c, b], [d, a]]).
%% mrule([[a, b], [c], [d], [b, a], [c, b], [d, b]]).
%% mrule([[a, b], [c], [d], [b, a], [c, b], [d, c]]).
%% mrule([[a, b], [c], [d], [b, a], [c, d], [d, a]]).
%% mrule([[a, b], [c], [d], [b, a], [c, d], [d, b]]).
%% mrule([[a, b], [c], [d], [b, a], [d, a], [d, c]]).
%% mrule([[a, b], [c], [d], [b, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [d], [b, c], [b, c], [d, a]]).
%% mrule([[a, b], [c], [d], [b, c], [b, d], [c, a]]).
%% mrule([[a, b], [c], [d], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [c], [d], [b, c], [c, a], [c, d]]).
%% mrule([[a, b], [c], [d], [b, c], [c, a], [d, a]]).
%% mrule([[a, b], [c], [d], [b, c], [c, a], [d, b]]).
%% mrule([[a, b], [c], [d], [b, c], [c, a], [d, c]]).
%% mrule([[a, b], [c], [d], [b, c], [c, b], [d, a]]).
%% mrule([[a, b], [c], [d], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [c], [d], [b, c], [d, a], [d, a]]).
%% mrule([[a, b], [c], [d], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [c], [d], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [c], [d], [b, c], [d, e], [e, a]]).
%% mrule([[a, b], [c], [d], [b, c], [e, a], [e, d]]).
%% mrule([[a, b], [c], [d], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [c], [d], [b, d], [c, a], [c, a]]).
%% mrule([[a, b], [c], [d], [b, d], [c, a], [c, b]]).
%% mrule([[a, b], [c], [d], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [c], [d], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [c], [d], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [c], [d], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [c], [d], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [c], [d], [b, d], [c, d], [d, a]]).
%% mrule([[a, b], [c], [d], [b, d], [c, e], [e, a]]).
%% mrule([[a, b], [c], [d], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [c], [d], [b, d], [e, a], [e, c]]).
%% mrule([[a, b], [c], [d], [b, e], [c, a], [d, e]]).
%% mrule([[a, b], [c], [d], [b, e], [c, a], [e, d]]).
%% mrule([[a, b], [c], [d], [b, e], [c, e], [d, a]]).
%% mrule([[a, b], [c], [d], [b, e], [d, a], [e, c]]).
%% mrule([[a, b], [c], [d], [c, a], [c, a], [d, b]]).
%% mrule([[a, b], [c], [d], [c, a], [c, b], [c, d]]).
%% mrule([[a, b], [c], [d], [c, a], [c, b], [d, a]]).
%% mrule([[a, b], [c], [d], [c, a], [c, b], [d, b]]).
%% mrule([[a, b], [c], [d], [c, a], [c, b], [d, c]]).
%% mrule([[a, b], [c], [d], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [c], [d], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [c], [d], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [c], [d], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [d], [c, a], [d, e], [e, b]]).
%% mrule([[a, b], [c], [d], [c, a], [e, b], [e, d]]).
%% mrule([[a, b], [c], [d], [c, b], [c, b], [d, a]]).
%% mrule([[a, b], [c], [d], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [c], [d], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [c], [d], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [c], [d], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [c], [d], [c, b], [d, e], [e, a]]).
%% mrule([[a, b], [c], [d], [c, b], [e, a], [e, d]]).
%% mrule([[a, b], [c], [d], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c], [d], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [c], [d], [c, e], [d, b], [e, a]]).
%% mrule([[a, b], [c], [d], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [d], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [c], [d], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [c], [a, b], [a, b], [a, b], [a, c]]).
%% mrule([[a, b], [c], [a, b], [a, b], [a, b], [b, c]]).
%% mrule([[a, b], [c], [a, b], [a, b], [a, b], [c, a]]).
%% mrule([[a, b], [c], [a, b], [a, b], [a, b], [c, b]]).
%% mrule([[a, b], [c], [a, b], [a, b], [a, c], [a, c]]).
%% mrule([[a, b], [c], [a, b], [a, b], [a, c], [b, a]]).
%% mrule([[a, b], [c], [a, b], [a, b], [a, c], [b, c]]).
%% mrule([[a, b], [c], [a, b], [a, b], [a, c], [c, a]]).
%% mrule([[a, b], [c], [a, b], [a, b], [a, c], [c, b]]).
%% mrule([[a, b], [c], [a, b], [a, b], [a, d], [c, d]]).
%% mrule([[a, b], [c], [a, b], [a, b], [a, d], [d, c]]).
%% mrule([[a, b], [c], [a, b], [a, b], [b, a], [b, c]]).
%% mrule([[a, b], [c], [a, b], [a, b], [b, a], [c, a]]).
%% mrule([[a, b], [c], [a, b], [a, b], [b, a], [c, b]]).
%% mrule([[a, b], [c], [a, b], [a, b], [b, c], [b, c]]).
%% mrule([[a, b], [c], [a, b], [a, b], [b, c], [c, a]]).
%% mrule([[a, b], [c], [a, b], [a, b], [b, c], [c, b]]).
%% mrule([[a, b], [c], [a, b], [a, b], [b, d], [c, d]]).
%% mrule([[a, b], [c], [a, b], [a, b], [b, d], [d, c]]).
%% mrule([[a, b], [c], [a, b], [a, b], [c, a], [c, a]]).
%% mrule([[a, b], [c], [a, b], [a, b], [c, a], [c, b]]).
%% mrule([[a, b], [c], [a, b], [a, b], [c, b], [c, b]]).
%% mrule([[a, b], [c], [a, b], [a, b], [c, d], [d, a]]).
%% mrule([[a, b], [c], [a, b], [a, b], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, b], [a, b], [d, a], [d, c]]).
%% mrule([[a, b], [c], [a, b], [a, b], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, b], [a, c], [a, c], [a, c]]).
%% mrule([[a, b], [c], [a, b], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [c], [a, b], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [c], [a, b], [a, c], [a, c], [c, a]]).
%% mrule([[a, b], [c], [a, b], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [c], [a, b], [a, c], [a, d], [a, d]]).
%% mrule([[a, b], [c], [a, b], [a, c], [a, d], [b, d]]).
%% mrule([[a, b], [c], [a, b], [a, c], [a, d], [c, d]]).
%% mrule([[a, b], [c], [a, b], [a, c], [a, d], [d, a]]).
%% mrule([[a, b], [c], [a, b], [a, c], [a, d], [d, b]]).
%% mrule([[a, b], [c], [a, b], [a, c], [a, d], [d, c]]).
%% mrule([[a, b], [c], [a, b], [a, c], [b, a], [b, a]]).
%% mrule([[a, b], [c], [a, b], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [c], [a, b], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [c], [a, b], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [c], [a, b], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [c], [a, b], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [c], [a, b], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [c], [a, b], [a, c], [b, d], [b, d]]).
%% mrule([[a, b], [c], [a, b], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [c], [a, b], [a, c], [b, d], [d, a]]).
%% mrule([[a, b], [c], [a, b], [a, c], [b, d], [d, b]]).
%% mrule([[a, b], [c], [a, b], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [c], [a, b], [a, c], [c, a], [c, a]]).
%% mrule([[a, b], [c], [a, b], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [c], [a, b], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [c], [a, b], [a, c], [c, d], [c, d]]).
%% mrule([[a, b], [c], [a, b], [a, c], [c, d], [d, a]]).
%% mrule([[a, b], [c], [a, b], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, b], [a, c], [c, d], [d, c]]).
%% mrule([[a, b], [c], [a, b], [a, c], [d, a], [d, a]]).
%% mrule([[a, b], [c], [a, b], [a, c], [d, a], [d, b]]).
%% mrule([[a, b], [c], [a, b], [a, c], [d, a], [d, c]]).
%% mrule([[a, b], [c], [a, b], [a, c], [d, b], [d, b]]).
%% mrule([[a, b], [c], [a, b], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, b], [a, c], [d, c], [d, c]]).
%% mrule([[a, b], [c], [a, b], [a, d], [a, d], [b, c]]).
%% mrule([[a, b], [c], [a, b], [a, d], [a, d], [c, a]]).
%% mrule([[a, b], [c], [a, b], [a, d], [a, d], [c, b]]).
%% mrule([[a, b], [c], [a, b], [a, d], [a, d], [c, d]]).
%% mrule([[a, b], [c], [a, b], [a, d], [a, d], [d, c]]).
%% mrule([[a, b], [c], [a, b], [a, d], [b, a], [c, d]]).
%% mrule([[a, b], [c], [a, b], [a, d], [b, a], [d, c]]).
%% mrule([[a, b], [c], [a, b], [a, d], [b, c], [b, d]]).
%% mrule([[a, b], [c], [a, b], [a, d], [b, c], [c, d]]).
%% mrule([[a, b], [c], [a, b], [a, d], [b, c], [d, a]]).
%% mrule([[a, b], [c], [a, b], [a, d], [b, c], [d, b]]).
%% mrule([[a, b], [c], [a, b], [a, d], [b, c], [d, c]]).
%% mrule([[a, b], [c], [a, b], [a, d], [b, d], [c, a]]).
%% mrule([[a, b], [c], [a, b], [a, d], [b, d], [c, b]]).
%% mrule([[a, b], [c], [a, b], [a, d], [b, d], [c, d]]).
%% mrule([[a, b], [c], [a, b], [a, d], [b, d], [d, c]]).
%% mrule([[a, b], [c], [a, b], [a, d], [c, a], [c, d]]).
%% mrule([[a, b], [c], [a, b], [a, d], [c, a], [d, a]]).
%% mrule([[a, b], [c], [a, b], [a, d], [c, a], [d, b]]).
%% mrule([[a, b], [c], [a, b], [a, d], [c, a], [d, c]]).
%% mrule([[a, b], [c], [a, b], [a, d], [c, b], [c, d]]).
%% mrule([[a, b], [c], [a, b], [a, d], [c, b], [d, a]]).
%% mrule([[a, b], [c], [a, b], [a, d], [c, b], [d, b]]).
%% mrule([[a, b], [c], [a, b], [a, d], [c, b], [d, c]]).
%% mrule([[a, b], [c], [a, b], [a, d], [c, d], [c, d]]).
%% mrule([[a, b], [c], [a, b], [a, d], [c, d], [d, a]]).
%% mrule([[a, b], [c], [a, b], [a, d], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, b], [a, d], [c, d], [d, c]]).
%% mrule([[a, b], [c], [a, b], [a, d], [c, e], [d, e]]).
%% mrule([[a, b], [c], [a, b], [a, d], [c, e], [e, d]]).
%% mrule([[a, b], [c], [a, b], [a, d], [d, a], [d, c]]).
%% mrule([[a, b], [c], [a, b], [a, d], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, b], [a, d], [d, c], [d, c]]).
%% mrule([[a, b], [c], [a, b], [a, d], [d, e], [e, c]]).
%% mrule([[a, b], [c], [a, b], [a, d], [e, c], [e, d]]).
%% mrule([[a, b], [c], [a, b], [b, a], [b, a], [b, c]]).
%% mrule([[a, b], [c], [a, b], [b, a], [b, a], [c, a]]).
%% mrule([[a, b], [c], [a, b], [b, a], [b, a], [c, b]]).
%% mrule([[a, b], [c], [a, b], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [c], [a, b], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [c], [a, b], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [c], [a, b], [b, a], [b, d], [c, d]]).
%% mrule([[a, b], [c], [a, b], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [c], [a, b], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [c], [a, b], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [c], [a, b], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [c], [a, b], [b, a], [c, d], [d, a]]).
%% mrule([[a, b], [c], [a, b], [b, a], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, b], [b, a], [d, a], [d, c]]).
%% mrule([[a, b], [c], [a, b], [b, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, b], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [c], [a, b], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [c], [a, b], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [c], [a, b], [b, c], [b, d], [b, d]]).
%% mrule([[a, b], [c], [a, b], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [c], [a, b], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [c], [a, b], [b, c], [b, d], [d, b]]).
%% mrule([[a, b], [c], [a, b], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [c], [a, b], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [c], [a, b], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [c], [a, b], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [c], [a, b], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [c], [a, b], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [c], [a, b], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, b], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [c], [a, b], [b, c], [d, a], [d, a]]).
%% mrule([[a, b], [c], [a, b], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [c], [a, b], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [c], [a, b], [b, c], [d, b], [d, b]]).
%% mrule([[a, b], [c], [a, b], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, b], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [c], [a, b], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [c], [a, b], [b, d], [b, d], [c, b]]).
%% mrule([[a, b], [c], [a, b], [b, d], [b, d], [c, d]]).
%% mrule([[a, b], [c], [a, b], [b, d], [b, d], [d, c]]).
%% mrule([[a, b], [c], [a, b], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [c], [a, b], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [c], [a, b], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [c], [a, b], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [c], [a, b], [b, d], [c, b], [c, d]]).
%% mrule([[a, b], [c], [a, b], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [c], [a, b], [b, d], [c, b], [d, b]]).
%% mrule([[a, b], [c], [a, b], [b, d], [c, b], [d, c]]).
%% mrule([[a, b], [c], [a, b], [b, d], [c, d], [c, d]]).
%% mrule([[a, b], [c], [a, b], [b, d], [c, d], [d, a]]).
%% mrule([[a, b], [c], [a, b], [b, d], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, b], [b, d], [c, d], [d, c]]).
%% mrule([[a], [b], [a, c], [c, d], [b, e], [d, e]]).
%% mrule([[a, b], [c], [a, b], [b, d], [c, e], [d, e]]).
%% mrule([[a], [b], [a, c], [c, d], [b, e], [e, d]]).
%% mrule([[a, b], [c], [a, b], [b, d], [c, e], [e, d]]).
%% mrule([[a, b], [c], [a, b], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [c], [a, b], [b, d], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, b], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [c], [a, b], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [c], [a, b], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [c], [a, b], [c, a], [c, a], [c, a]]).
%% mrule([[a, b], [c], [a, b], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [c], [a, b], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [c], [a, b], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [c], [a, b], [c, a], [c, d], [d, a]]).
%% mrule([[a, b], [c], [a, b], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, b], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [c], [a, b], [c, a], [d, a], [d, a]]).
%% mrule([[a, b], [c], [a, b], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [c], [a, b], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [c], [a, b], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [c], [a, b], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, b], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [c], [a, b], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [c], [a, b], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [c], [a, b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [c], [a, b], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, b], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [c], [a, b], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [c], [a, b], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [c], [a, b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [c], [a, b], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [c], [a, b], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, b], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [c], [a, b], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [c], [a, b], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, b], [c, d], [d, a], [d, a]]).
%% mrule([[a, b], [c], [a, b], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c], [a, b], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [c], [a, b], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [c], [a, b], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, b], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [c], [a, b], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [c], [a, b], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [c], [a, b], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [c], [a, b], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [c], [a, b], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, b], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [c], [a, b], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [c], [a, b], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [c], [a, b], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, b], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [c], [a, b], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [c], [a, b], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [c], [a, b], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [c], [a, b], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [c], [a, b], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [c], [a, b], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [c], [a, c], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [c], [a, c], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [c], [a, c], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [c], [a, c], [a, c], [a, d], [b, d]]).
%% mrule([[a, b], [c], [a, c], [a, c], [a, d], [d, b]]).
%% mrule([[a, b], [c], [a, c], [a, c], [b, a], [b, a]]).
%% mrule([[a, b], [c], [a, c], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [c], [a, c], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [c], [a, c], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [c], [a, c], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [c], [a, c], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [c], [a, c], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [c], [a, c], [a, c], [b, d], [b, d]]).
%% mrule([[a, b], [c], [a, c], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [c], [a, c], [a, c], [b, d], [d, a]]).
%% mrule([[a, b], [c], [a, c], [a, c], [b, d], [d, b]]).
%% mrule([[a, b], [c], [a, c], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [c], [a, c], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [c], [a, c], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [c], [a, c], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, c], [a, c], [d, a], [d, b]]).
%% mrule([[a, b], [c], [a, c], [a, c], [d, b], [d, b]]).
%% mrule([[a, b], [c], [a, c], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, c], [a, d], [a, d], [b, a]]).
%% mrule([[a, b], [c], [a, c], [a, d], [a, d], [b, c]]).
%% mrule([[a, b], [c], [a, c], [a, d], [a, d], [b, d]]).
%% mrule([[a, b], [c], [a, c], [a, d], [a, d], [c, b]]).
%% mrule([[a, b], [c], [a, c], [a, d], [a, d], [d, b]]).
%% mrule([[a, b], [c], [a, c], [a, d], [b, a], [b, d]]).
%% mrule([[a, b], [c], [a, c], [a, d], [b, a], [c, d]]).
%% mrule([[a, b], [c], [a, c], [a, d], [b, a], [d, a]]).
%% mrule([[a, b], [c], [a, c], [a, d], [b, a], [d, b]]).
%% mrule([[a, b], [c], [a, c], [a, d], [b, a], [d, c]]).
%% mrule([[a, b], [c], [a, c], [a, d], [b, c], [b, d]]).
%% mrule([[a, b], [c], [a, c], [a, d], [b, c], [c, d]]).
%% mrule([[a, b], [c], [a, c], [a, d], [b, c], [d, a]]).
%% mrule([[a, b], [c], [a, c], [a, d], [b, c], [d, b]]).
%% mrule([[a, b], [c], [a, c], [a, d], [b, c], [d, c]]).
%% mrule([[a, b], [c], [a, c], [a, d], [b, d], [b, d]]).
%% mrule([[a, b], [c], [a, c], [a, d], [b, d], [c, a]]).
%% mrule([[a, b], [c], [a, c], [a, d], [b, d], [c, b]]).
%% mrule([[a, b], [c], [a, c], [a, d], [b, d], [c, d]]).
%% mrule([[a, b], [c], [a, c], [a, d], [b, d], [d, a]]).
%% mrule([[a, b], [c], [a, c], [a, d], [b, d], [d, b]]).
%% mrule([[a, b], [c], [a, c], [a, d], [b, d], [d, c]]).
%% mrule([[a, b], [c], [a, c], [a, d], [b, e], [d, e]]).
%% mrule([[a, b], [c], [a, c], [a, d], [b, e], [e, d]]).
%% mrule([[a, b], [c], [a, c], [a, d], [c, a], [d, b]]).
%% mrule([[a, b], [c], [a, c], [a, d], [c, b], [c, d]]).
%% mrule([[a, b], [c], [a, c], [a, d], [c, b], [d, a]]).
%% mrule([[a, b], [c], [a, c], [a, d], [c, b], [d, b]]).
%% mrule([[a, b], [c], [a, c], [a, d], [c, b], [d, c]]).
%% mrule([[a, b], [c], [a, c], [a, d], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, c], [a, d], [d, a], [d, b]]).
%% mrule([[a, b], [c], [a, c], [a, d], [d, b], [d, b]]).
%% mrule([[a, b], [c], [a, c], [a, d], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, c], [a, d], [d, e], [e, b]]).
%% mrule([[a, b], [c], [a, c], [a, d], [e, b], [e, d]]).
%% mrule([[a, b], [c], [a, c], [b, a], [b, a], [b, a]]).
%% mrule([[a, b], [c], [a, c], [b, a], [b, a], [b, c]]).
%% mrule([[a, b], [c], [a, c], [b, a], [b, a], [c, a]]).
%% mrule([[a, b], [c], [a, c], [b, a], [b, a], [c, b]]).
%% mrule([[a, b], [c], [a, c], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [c], [a, c], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [c], [a, c], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [c], [a, c], [b, a], [b, d], [b, d]]).
%% mrule([[a], [b], [a, b], [c, a], [c, d], [b, d]]).
%% mrule([[a, b], [c], [a, c], [b, a], [b, d], [c, d]]).
%% mrule([[a, b], [c], [a, c], [b, a], [b, d], [d, a]]).
%% mrule([[a, b], [c], [a, c], [b, a], [b, d], [d, b]]).
%% mrule([[a, b], [c], [a, c], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [c], [a, c], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [c], [a, c], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [c], [a, c], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [c], [a, c], [b, a], [c, d], [c, d]]).
%% mrule([[a, b], [c], [a, c], [b, a], [c, d], [d, a]]).
%% mrule([[a], [b], [a, b], [c, a], [b, d], [d, c]]).
%% mrule([[a, b], [c], [a, c], [b, a], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, c], [b, a], [c, d], [d, c]]).
%% mrule([[a, b], [c], [a, c], [b, a], [d, a], [d, a]]).
%% mrule([[a, b], [c], [a, c], [b, a], [d, a], [d, b]]).
%% mrule([[a, b], [c], [a, c], [b, a], [d, a], [d, c]]).
%% mrule([[a, b], [c], [a, c], [b, a], [d, b], [d, b]]).
%% mrule([[a, b], [c], [a, c], [b, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, c], [b, a], [d, c], [d, c]]).
%% mrule([[a, b], [c], [a, c], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [c], [a, c], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [c], [a, c], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [c], [a, c], [b, c], [b, d], [b, d]]).
%% mrule([[a], [b], [a, b], [c, b], [c, d], [b, d]]).
%% mrule([[a, b], [c], [a, c], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [c], [a, c], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [c], [a, c], [b, c], [b, d], [d, b]]).
%% mrule([[a, b], [c], [a, c], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [c], [a, c], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [c], [a, c], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [c], [a, c], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [c], [a, c], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [c], [a, c], [b, c], [c, d], [d, a]]).
%% mrule([[a], [b], [a, b], [c, b], [b, d], [d, c]]).
%% mrule([[a, b], [c], [a, c], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, c], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [c], [a, c], [b, c], [d, a], [d, a]]).
%% mrule([[a, b], [c], [a, c], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [c], [a, c], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [c], [a, c], [b, c], [d, b], [d, b]]).
%% mrule([[a, b], [c], [a, c], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, c], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [c], [a, c], [b, d], [b, d], [b, d]]).
%% mrule([[a, b], [c], [a, c], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [c], [a, c], [b, d], [b, d], [c, b]]).
%% mrule([[a], [b], [a, b], [c, d], [c, d], [b, d]]).
%% mrule([[a, b], [c], [a, c], [b, d], [b, d], [c, d]]).
%% mrule([[a, b], [c], [a, c], [b, d], [b, d], [d, a]]).
%% mrule([[a, b], [c], [a, c], [b, d], [b, d], [d, b]]).
%% mrule([[a, b], [c], [a, c], [b, d], [b, d], [d, c]]).
%% mrule([[a, b], [c], [a, c], [b, d], [b, e], [d, e]]).
%% mrule([[a, b], [c], [a, c], [b, d], [b, e], [e, d]]).
%% mrule([[a, b], [c], [a, c], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [c], [a, c], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [c], [a, c], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [c], [a, c], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [c], [a, c], [b, d], [c, b], [c, d]]).
%% mrule([[a, b], [c], [a, c], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [c], [a, c], [b, d], [c, b], [d, b]]).
%% mrule([[a, b], [c], [a, c], [b, d], [c, b], [d, c]]).
%% mrule([[a, b], [c], [a, c], [b, d], [c, d], [c, d]]).
%% mrule([[a, b], [c], [a, c], [b, d], [c, d], [d, a]]).
%% mrule([[a], [b], [a, b], [c, d], [b, d], [d, c]]).
%% mrule([[a, b], [c], [a, c], [b, d], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, c], [b, d], [c, d], [d, c]]).
%% mrule([[a, b], [c], [a, c], [b, d], [c, e], [d, e]]).
%% mrule([[a, b], [c], [a, c], [b, d], [c, e], [e, d]]).
%% mrule([[a, b], [c], [a, c], [b, d], [d, a], [d, a]]).
%% mrule([[a, b], [c], [a, c], [b, d], [d, a], [d, b]]).
%% mrule([[a, b], [c], [a, c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [c], [a, c], [b, d], [d, b], [d, b]]).
%% mrule([[a, b], [c], [a, c], [b, d], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, c], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [c], [a, c], [b, d], [d, e], [d, e]]).
%% mrule([[a, b], [c], [a, c], [b, d], [d, e], [e, a]]).
%% mrule([[a, b], [c], [a, c], [b, d], [d, e], [e, b]]).
%% mrule([[a, b], [c], [a, c], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [c], [a, c], [b, d], [d, e], [e, d]]).
%% mrule([[a, b], [c], [a, c], [b, d], [e, a], [e, d]]).
%% mrule([[a, b], [c], [a, c], [b, d], [e, b], [e, d]]).
%% mrule([[a, b], [c], [a, c], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [c], [a, c], [b, d], [e, d], [e, d]]).
%% mrule([[a, b], [c], [a, c], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [c], [a, c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [c], [a, c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [c], [a, c], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [c], [a, c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, c], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [c], [a, c], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [c], [a, c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [c], [a, c], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, c], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [c], [a, c], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [c], [a, c], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [c], [a, c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [c], [a, c], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [c], [a, c], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, c], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [c], [a, c], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c], [a, c], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [c], [a, c], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, c], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [c], [a, c], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [c], [a, c], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [c], [a, c], [d, a], [d, b], [d, b]]).
%% mrule([[a, b], [c], [a, c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, c], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [c], [a, c], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [c], [a, c], [d, b], [d, b], [d, b]]).
%% mrule([[a, b], [c], [a, c], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, c], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [c], [a, c], [d, b], [d, e], [d, e]]).
%% mrule([[a, b], [c], [a, c], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [c], [a, c], [d, b], [d, e], [e, b]]).
%% mrule([[a, b], [c], [a, c], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [c], [a, c], [d, b], [d, e], [e, d]]).
%% mrule([[a, b], [c], [a, c], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [c], [a, c], [d, b], [e, b], [e, d]]).
%% mrule([[a, b], [c], [a, c], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [c], [a, c], [d, b], [e, d], [e, d]]).
%% mrule([[a, b], [c], [a, c], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [c], [a, c], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [c], [a, c], [d, e], [d, e], [e, b]]).
%% mrule([[a, b], [c], [a, c], [d, e], [e, b], [e, d]]).
%% mrule([[a, b], [c], [a, d], [a, d], [a, d], [b, c]]).
%% mrule([[a, b], [c], [a, d], [a, d], [a, d], [c, b]]).
%% mrule([[a, b], [c], [a, d], [a, d], [b, a], [b, c]]).
%% mrule([[a, b], [c], [a, d], [a, d], [b, a], [c, a]]).
%% mrule([[a, b], [c], [a, d], [a, d], [b, a], [c, b]]).
%% mrule([[a, b], [c], [a, d], [a, d], [b, a], [c, d]]).
%% mrule([[a, b], [c], [a, d], [a, d], [b, a], [d, c]]).
%% mrule([[a, b], [c], [a, d], [a, d], [b, c], [b, c]]).
%% mrule([[a, b], [c], [a, d], [a, d], [b, c], [b, d]]).
%% mrule([[a, b], [c], [a, d], [a, d], [b, c], [c, a]]).
%% mrule([[a, b], [c], [a, d], [a, d], [b, c], [c, b]]).
%% mrule([[a, b], [c], [a, d], [a, d], [b, c], [c, d]]).
%% mrule([[a, b], [c], [a, d], [a, d], [b, c], [d, a]]).
%% mrule([[a, b], [c], [a, d], [a, d], [b, c], [d, b]]).
%% mrule([[a, b], [c], [a, d], [a, d], [b, c], [d, c]]).
%% mrule([[a, b], [c], [a, d], [a, d], [b, d], [c, a]]).
%% mrule([[a, b], [c], [a, d], [a, d], [b, d], [c, b]]).
%% mrule([[a, b], [c], [a, d], [a, d], [b, d], [c, d]]).
%% mrule([[a, b], [c], [a, d], [a, d], [b, d], [d, c]]).
%% mrule([[a, b], [c], [a, d], [a, d], [b, e], [c, e]]).
%% mrule([[a, b], [c], [a, d], [a, d], [b, e], [e, c]]).
%% mrule([[a, b], [c], [a, d], [a, d], [c, a], [c, b]]).
%% mrule([[a, b], [c], [a, d], [a, d], [c, a], [d, b]]).
%% mrule([[a, b], [c], [a, d], [a, d], [c, b], [c, b]]).
%% mrule([[a, b], [c], [a, d], [a, d], [c, b], [c, d]]).
%% mrule([[a, b], [c], [a, d], [a, d], [c, b], [d, a]]).
%% mrule([[a, b], [c], [a, d], [a, d], [c, b], [d, b]]).
%% mrule([[a, b], [c], [a, d], [a, d], [c, b], [d, c]]).
%% mrule([[a, b], [c], [a, d], [a, d], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, d], [a, d], [c, e], [e, b]]).
%% mrule([[a, b], [c], [a, d], [a, d], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, d], [a, d], [e, b], [e, c]]).
%% mrule([[a, b], [c], [a, d], [a, e], [b, c], [d, e]]).
%% mrule([[a, b], [c], [a, d], [a, e], [b, c], [e, d]]).
%% mrule([[a, b], [c], [a, d], [a, e], [b, d], [c, e]]).
%% mrule([[a, b], [c], [a, d], [a, e], [b, d], [e, c]]).
%% mrule([[a, b], [c], [a, d], [a, e], [b, e], [c, d]]).
%% mrule([[a, b], [c], [a, d], [a, e], [b, e], [d, c]]).
%% mrule([[a, b], [c], [a, d], [a, e], [c, b], [d, e]]).
%% mrule([[a, b], [c], [a, d], [a, e], [c, b], [e, d]]).
%% mrule([[a, b], [c], [a, d], [a, e], [c, d], [e, b]]).
%% mrule([[a, b], [c], [a, d], [a, e], [c, e], [d, b]]).
%% mrule([[a, b], [c], [a, d], [a, e], [d, b], [e, c]]).
%% mrule([[a, b], [c], [a, d], [a, e], [d, c], [e, b]]).
%% mrule([[a, b], [c], [a, d], [b, a], [b, a], [c, d]]).
%% mrule([[a, b], [c], [a, d], [b, a], [b, a], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, a], [b, c], [b, d]]).
%% mrule([[a, b], [c], [a, d], [b, a], [b, c], [c, d]]).
%% mrule([[a, b], [c], [a, d], [b, a], [b, c], [d, a]]).
%% mrule([[a, b], [c], [a, d], [b, a], [b, c], [d, b]]).
%% mrule([[a, b], [c], [a, d], [b, a], [b, c], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, a], [b, d], [c, a]]).
%% mrule([[a, b], [c], [a, d], [b, a], [b, d], [c, b]]).
%% mrule([[a, b], [c], [a, d], [b, a], [b, d], [c, d]]).
%% mrule([[a, b], [c], [a, d], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, a], [c, a], [c, d]]).
%% mrule([[a, b], [c], [a, d], [b, a], [c, a], [d, a]]).
%% mrule([[a, b], [c], [a, d], [b, a], [c, a], [d, b]]).
%% mrule([[a, b], [c], [a, d], [b, a], [c, a], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, a], [c, b], [c, d]]).
%% mrule([[a, b], [c], [a, d], [b, a], [c, b], [d, a]]).
%% mrule([[a, b], [c], [a, d], [b, a], [c, b], [d, b]]).
%% mrule([[a, b], [c], [a, d], [b, a], [c, b], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, a], [c, d], [c, d]]).
%% mrule([[a, b], [c], [a, d], [b, a], [c, d], [d, a]]).
%% mrule([[a, b], [c], [a, d], [b, a], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, d], [b, a], [c, d], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, a], [c, e], [d, e]]).
%% mrule([[a, b], [c], [a, d], [b, a], [c, e], [e, d]]).
%% mrule([[a, b], [c], [a, d], [b, a], [d, a], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, a], [d, c], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, a], [d, e], [e, c]]).
%% mrule([[a, b], [c], [a, d], [b, a], [e, c], [e, d]]).
%% mrule([[a, b], [c], [a, d], [b, c], [b, c], [b, d]]).
%% mrule([[a, b], [c], [a, d], [b, c], [b, c], [c, d]]).
%% mrule([[a, b], [c], [a, d], [b, c], [b, c], [d, a]]).
%% mrule([[a, b], [c], [a, d], [b, c], [b, c], [d, b]]).
%% mrule([[a, b], [c], [a, d], [b, c], [b, c], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, c], [b, d], [b, d]]).
%% mrule([[a, b], [c], [a, d], [b, c], [b, d], [c, a]]).
%% mrule([[a, b], [c], [a, d], [b, c], [b, d], [c, b]]).
%% mrule([[a, b], [c], [a, d], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [c], [a, d], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [c], [a, d], [b, c], [b, d], [d, b]]).
%% mrule([[a, b], [c], [a, d], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, c], [b, e], [d, e]]).
%% mrule([[a, b], [c], [a, d], [b, c], [b, e], [e, d]]).
%% mrule([[a, b], [c], [a, d], [b, c], [c, a], [c, d]]).
%% mrule([[a, b], [c], [a, d], [b, c], [c, a], [d, a]]).
%% mrule([[a, b], [c], [a, d], [b, c], [c, a], [d, b]]).
%% mrule([[a, b], [c], [a, d], [b, c], [c, a], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, c], [c, b], [c, d]]).
%% mrule([[a, b], [c], [a, d], [b, c], [c, b], [d, a]]).
%% mrule([[a, b], [c], [a, d], [b, c], [c, b], [d, b]]).
%% mrule([[a, b], [c], [a, d], [b, c], [c, b], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [c], [a, d], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [c], [a, d], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, d], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, c], [c, e], [d, e]]).
%% mrule([[a, b], [c], [a, d], [b, c], [c, e], [e, d]]).
%% mrule([[a, b], [c], [a, d], [b, c], [d, a], [d, a]]).
%% mrule([[a, b], [c], [a, d], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [c], [a, d], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, c], [d, b], [d, b]]).
%% mrule([[a, b], [c], [a, d], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, c], [d, e], [d, e]]).
%% mrule([[a, b], [c], [a, d], [b, c], [d, e], [e, a]]).
%% mrule([[a, b], [c], [a, d], [b, c], [d, e], [e, b]]).
%% mrule([[a, b], [c], [a, d], [b, c], [d, e], [e, c]]).
%% mrule([[a, b], [c], [a, d], [b, c], [d, e], [e, d]]).
%% mrule([[a, b], [c], [a, d], [b, c], [e, a], [e, d]]).
%% mrule([[a, b], [c], [a, d], [b, c], [e, b], [e, d]]).
%% mrule([[a, b], [c], [a, d], [b, c], [e, c], [e, d]]).
%% mrule([[a, b], [c], [a, d], [b, c], [e, d], [e, d]]).
%% mrule([[a, b], [c], [a, d], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [c], [a, d], [b, d], [b, d], [c, b]]).
%% mrule([[a, b], [c], [a, d], [b, d], [b, d], [c, d]]).
%% mrule([[a, b], [c], [a, d], [b, d], [b, d], [d, c]]).
%% mrule([[a], [b], [a, c], [d, c], [d, e], [b, e]]).
%% mrule([[a, b], [c], [a, d], [b, d], [b, e], [c, e]]).
%% mrule([[a, b], [c], [a, d], [b, d], [b, e], [e, c]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, a], [c, a]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, a], [c, b]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, b], [c, b]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, b], [c, d]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, b], [d, b]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, b], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, d], [c, d]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, d], [d, a]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, d], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, e], [d, e]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, e], [e, a]]).
%% mrule([[a], [b], [a, c], [d, c], [b, e], [e, d]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, e], [e, b]]).
%% mrule([[a, b], [c], [a, d], [b, d], [c, e], [e, d]]).
%% mrule([[a, b], [c], [a, d], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, d], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [c], [a, d], [b, d], [e, a], [e, c]]).
%% mrule([[a, b], [c], [a, d], [b, d], [e, b], [e, c]]).
%% mrule([[a, b], [c], [a, d], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [c], [a, d], [b, e], [b, e], [c, d]]).
%% mrule([[a, b], [c], [a, d], [b, e], [b, e], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, e], [c, a], [d, e]]).
%% mrule([[a, b], [c], [a, d], [b, e], [c, a], [e, d]]).
%% mrule([[a, b], [c], [a, d], [b, e], [c, b], [d, e]]).
%% mrule([[a, b], [c], [a, d], [b, e], [c, b], [e, d]]).
%% mrule([[a, b], [c], [a, d], [b, e], [c, d], [c, e]]).
%% mrule([[a, b], [c], [a, d], [b, e], [c, d], [d, e]]).
%% mrule([[a, b], [c], [a, d], [b, e], [c, d], [e, a]]).
%% mrule([[a, b], [c], [a, d], [b, e], [c, d], [e, b]]).
%% mrule([[a, b], [c], [a, d], [b, e], [c, d], [e, c]]).
%% mrule([[a, b], [c], [a, d], [b, e], [c, d], [e, d]]).
%% mrule([[a, b], [c], [a, d], [b, e], [c, e], [d, a]]).
%% mrule([[a, b], [c], [a, d], [b, e], [c, e], [d, b]]).
%% mrule([[a, b], [c], [a, d], [b, e], [c, e], [d, c]]).
%% mrule([[a, b], [c], [a, d], [b, e], [c, e], [d, e]]).
%% mrule([[a, b], [c], [a, d], [b, e], [c, e], [e, d]]).
%% mrule([[a, b], [c], [a, d], [b, e], [d, a], [e, c]]).
%% mrule([[a, b], [c], [a, d], [b, e], [d, b], [e, c]]).
%% mrule([[a, b], [c], [a, d], [b, e], [d, c], [d, e]]).
%% mrule([[a, b], [c], [a, d], [b, e], [d, c], [e, a]]).
%% mrule([[a, b], [c], [a, d], [b, e], [d, c], [e, b]]).
%% mrule([[a, b], [c], [a, d], [b, e], [d, c], [e, c]]).
%% mrule([[a, b], [c], [a, d], [b, e], [d, c], [e, d]]).
%% mrule([[a, b], [c], [a, d], [b, e], [d, e], [e, c]]).
%% mrule([[a, b], [c], [a, d], [b, e], [e, c], [e, d]]).
%% mrule([[a, b], [c], [a, d], [c, a], [c, a], [d, b]]).
%% mrule([[a, b], [c], [a, d], [c, a], [c, b], [c, d]]).
%% mrule([[a, b], [c], [a, d], [c, a], [c, b], [d, a]]).
%% mrule([[a, b], [c], [a, d], [c, a], [c, b], [d, b]]).
%% mrule([[a, b], [c], [a, d], [c, a], [c, b], [d, c]]).
%% mrule([[a, b], [c], [a, d], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, d], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [c], [a, d], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [c], [a, d], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, d], [c, a], [d, e], [e, b]]).
%% mrule([[a, b], [c], [a, d], [c, a], [e, b], [e, d]]).
%% mrule([[a, b], [c], [a, d], [c, b], [c, b], [c, d]]).
%% mrule([[a, b], [c], [a, d], [c, b], [c, b], [d, a]]).
%% mrule([[a, b], [c], [a, d], [c, b], [c, b], [d, b]]).
%% mrule([[a, b], [c], [a, d], [c, b], [c, b], [d, c]]).
%% mrule([[a, b], [c], [a, d], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [c], [a, d], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [c], [a, d], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, d], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [c], [a, d], [c, b], [c, e], [d, e]]).
%% mrule([[a, b], [c], [a, d], [c, b], [c, e], [e, d]]).
%% mrule([[a, b], [c], [a, d], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [c], [a, d], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [c], [a, d], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [c], [a, d], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [c], [a, d], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, d], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [c], [a, d], [c, b], [d, e], [d, e]]).
%% mrule([[a, b], [c], [a, d], [c, b], [d, e], [e, a]]).
%% mrule([[a, b], [c], [a, d], [c, b], [d, e], [e, b]]).
%% mrule([[a, b], [c], [a, d], [c, b], [d, e], [e, c]]).
%% mrule([[a, b], [c], [a, d], [c, b], [d, e], [e, d]]).
%% mrule([[a, b], [c], [a, d], [c, b], [e, a], [e, d]]).
%% mrule([[a, b], [c], [a, d], [c, b], [e, b], [e, d]]).
%% mrule([[a, b], [c], [a, d], [c, b], [e, c], [e, d]]).
%% mrule([[a, b], [c], [a, d], [c, b], [e, d], [e, d]]).
%% mrule([[a, b], [c], [a, d], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [c], [a, d], [c, d], [c, e], [e, b]]).
%% mrule([[a, b], [c], [a, d], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c], [a, d], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [c], [a, d], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, d], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [c], [a, d], [c, d], [e, a], [e, b]]).
%% mrule([[a, b], [c], [a, d], [c, d], [e, b], [e, b]]).
%% mrule([[a, b], [c], [a, d], [c, d], [e, b], [e, c]]).
%% mrule([[a, b], [c], [a, d], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [c], [a, d], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [c], [a, d], [c, e], [d, b], [d, e]]).
%% mrule([[a, b], [c], [a, d], [c, e], [d, b], [e, a]]).
%% mrule([[a, b], [c], [a, d], [c, e], [d, b], [e, b]]).
%% mrule([[a, b], [c], [a, d], [c, e], [d, b], [e, d]]).
%% mrule([[a, b], [c], [a, d], [c, e], [d, c], [e, b]]).
%% mrule([[a, b], [c], [a, d], [c, e], [d, e], [e, b]]).
%% mrule([[a, b], [c], [a, d], [c, e], [e, b], [e, d]]).
%% mrule([[a, b], [c], [a, d], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, d], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [c], [a, d], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [c], [a, d], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [c], [a, d], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [c], [a, d], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [c], [a, d], [d, b], [e, b], [e, c]]).
%% mrule([[a, b], [c], [a, d], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [c], [a, d], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [c], [a, d], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [c], [a, d], [d, c], [e, b], [e, b]]).
%% mrule([[a, b], [c], [a, d], [d, c], [e, b], [e, c]]).
%% mrule([[a, b], [c], [a, d], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [c], [a, d], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [c], [a, d], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [c], [b, a], [b, a], [b, a], [b, c]]).
%% mrule([[a, b], [c], [b, a], [b, a], [b, a], [c, a]]).
%% mrule([[a, b], [c], [b, a], [b, a], [b, a], [c, b]]).
%% mrule([[a, b], [c], [b, a], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [c], [b, a], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [c], [b, a], [b, a], [b, c], [c, b]]).
%% mrule([[a], [b], [c, a], [c, a], [c, d], [b, d]]).
%% mrule([[a, b], [c], [b, a], [b, a], [b, d], [c, d]]).
%% mrule([[a, b], [c], [b, a], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [c], [b, a], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [c], [b, a], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [c], [b, a], [b, a], [c, b], [c, b]]).
%% mrule([[a], [b], [c, a], [c, a], [b, d], [d, a]]).
%% mrule([[a, b], [c], [b, a], [b, a], [c, d], [d, a]]).
%% mrule([[a], [b], [c, a], [c, a], [b, d], [d, c]]).
%% mrule([[a, b], [c], [b, a], [b, a], [c, d], [d, b]]).
%% mrule([[a, b], [c], [b, a], [b, a], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, a], [b, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [b, a], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [c], [b, a], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [c], [b, a], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [c], [b, a], [b, c], [b, d], [b, d]]).
%% mrule([[a], [b], [c, a], [c, b], [c, d], [b, d]]).
%% mrule([[a, b], [c], [b, a], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [c], [b, a], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [c], [b, a], [b, c], [b, d], [d, b]]).
%% mrule([[a, b], [c], [b, a], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [c], [b, a], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [c], [b, a], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [c], [b, a], [b, c], [c, b], [c, b]]).
%% mrule([[a], [b], [c, a], [c, b], [b, d], [b, d]]).
%% mrule([[a, b], [c], [b, a], [b, c], [c, d], [c, d]]).
%% mrule([[a], [b], [c, a], [c, b], [b, d], [d, a]]).
%% mrule([[a, b], [c], [b, a], [b, c], [c, d], [d, a]]).
%% mrule([[a], [b], [c, a], [c, b], [b, d], [d, c]]).
%% mrule([[a, b], [c], [b, a], [b, c], [c, d], [d, b]]).
%% mrule([[a], [b], [c, a], [c, b], [b, d], [d, b]]).
%% mrule([[a, b], [c], [b, a], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [c], [b, a], [b, c], [d, a], [d, a]]).
%% mrule([[a, b], [c], [b, a], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [c], [b, a], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, a], [b, c], [d, b], [d, b]]).
%% mrule([[a, b], [c], [b, a], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [c], [b, a], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [c], [b, a], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [c], [b, a], [b, d], [b, d], [c, b]]).
%% mrule([[a], [b], [c, a], [c, d], [c, d], [b, d]]).
%% mrule([[a, b], [c], [b, a], [b, d], [b, d], [c, d]]).
%% mrule([[a, b], [c], [b, a], [b, d], [b, d], [d, c]]).
%% mrule([[a], [b], [c, a], [c, d], [b, a], [b, d]]).
%% mrule([[a, b], [c], [b, a], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [c], [b, a], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [c], [b, a], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [c], [b, a], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [c], [b, a], [b, d], [c, b], [c, d]]).
%% mrule([[a, b], [c], [b, a], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [c], [b, a], [b, d], [c, b], [d, b]]).
%% mrule([[a, b], [c], [b, a], [b, d], [c, b], [d, c]]).
%% mrule([[a], [b], [c, a], [c, d], [b, d], [b, d]]).
%% mrule([[a, b], [c], [b, a], [b, d], [c, d], [c, d]]).
%% mrule([[a], [b], [c, a], [c, d], [b, d], [d, a]]).
%% mrule([[a, b], [c], [b, a], [b, d], [c, d], [d, a]]).
%% mrule([[a], [b], [c, a], [c, d], [b, d], [d, c]]).
%% mrule([[a, b], [c], [b, a], [b, d], [c, d], [d, b]]).
%% mrule([[a], [b], [c, a], [c, d], [b, d], [d, b]]).
%% mrule([[a, b], [c], [b, a], [b, d], [c, d], [d, c]]).
%% mrule([[a], [b], [c, a], [c, d], [b, e], [d, e]]).
%% mrule([[a, b], [c], [b, a], [b, d], [c, e], [d, e]]).
%% mrule([[a], [b], [c, a], [c, d], [b, e], [e, d]]).
%% mrule([[a, b], [c], [b, a], [b, d], [c, e], [e, d]]).
%% mrule([[a, b], [c], [b, a], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, a], [b, d], [d, b], [d, c]]).
%% mrule([[a, b], [c], [b, a], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [c], [b, a], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [c], [b, a], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [c], [b, a], [c, a], [c, a], [c, a]]).
%% mrule([[a, b], [c], [b, a], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [c], [b, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [c], [b, a], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [c], [b, a], [c, a], [c, d], [d, a]]).
%% mrule([[a], [b], [c, a], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [c], [b, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [c], [b, a], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [c], [b, a], [c, a], [d, a], [d, a]]).
%% mrule([[a, b], [c], [b, a], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [c], [b, a], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, a], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [c], [b, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [b, a], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [c], [b, a], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [c], [b, a], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [c], [b, a], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [c], [b, a], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [c], [b, a], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [c], [b, a], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [c], [b, a], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [c], [b, a], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, a], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [c], [b, a], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [c], [b, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [c], [b, a], [c, d], [c, d], [d, a]]).
%% mrule([[a], [b], [c, a], [b, d], [b, d], [d, c]]).
%% mrule([[a, b], [c], [b, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [c], [b, a], [c, d], [d, a], [d, a]]).
%% mrule([[a], [b], [c, a], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, a], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c], [b, a], [c, d], [d, a], [d, c]]).
%% mrule([[a], [b], [c, a], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [c], [b, a], [c, d], [d, b], [d, b]]).
%% mrule([[a], [b], [c, a], [b, d], [d, c], [d, b]]).
%% mrule([[a, b], [c], [b, a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [c], [b, a], [c, d], [d, e], [e, a]]).
%% mrule([[a], [b], [c, a], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [c], [b, a], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [c], [b, a], [c, d], [e, a], [e, d]]).
%% mrule([[a], [b], [c, a], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [c], [b, a], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [c], [b, a], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [b, a], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [c], [b, a], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [c], [b, a], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [c], [b, a], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [c], [b, a], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [c], [b, a], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [c], [b, a], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [c], [b, a], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [c], [b, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [c], [b, a], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [c], [b, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [c], [b, c], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [c], [b, c], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [c], [b, c], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [c], [b, c], [b, c], [c, a], [c, b]]).
%% mrule([[a], [b], [c, b], [c, b], [b, d], [d, a]]).
%% mrule([[a, b], [c], [b, c], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [c], [b, c], [b, c], [d, a], [d, a]]).
%% mrule([[a, b], [c], [b, c], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [c], [b, c], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, c], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [c], [b, c], [b, d], [b, d], [d, a]]).
%% mrule([[a], [b], [c, b], [c, d], [b, a], [b, d]]).
%% mrule([[a, b], [c], [b, c], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [c], [b, c], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [c], [b, c], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [c], [b, c], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [c], [b, c], [b, d], [c, b], [d, a]]).
%% mrule([[a], [b], [c, b], [c, d], [b, d], [d, a]]).
%% mrule([[a, b], [c], [b, c], [b, d], [c, d], [d, a]]).
%% mrule([[a, b], [c], [b, c], [b, d], [d, a], [d, a]]).
%% mrule([[a, b], [c], [b, c], [b, d], [d, a], [d, b]]).
%% mrule([[a, b], [c], [b, c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, c], [b, d], [d, e], [e, a]]).
%% mrule([[a, b], [c], [b, c], [b, d], [e, a], [e, d]]).
%% mrule([[a, b], [c], [b, c], [c, a], [c, a], [c, a]]).
%% mrule([[a, b], [c], [b, c], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [c], [b, c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [c], [b, c], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [c], [b, c], [c, a], [c, d], [d, a]]).
%% mrule([[a], [b], [c, b], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [c], [b, c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [c], [b, c], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [c], [b, c], [c, a], [d, a], [d, a]]).
%% mrule([[a, b], [c], [b, c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [c], [b, c], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, c], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [c], [b, c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [b, c], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [c], [b, c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [c], [b, c], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [c], [b, c], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [c], [b, c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, c], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [c], [b, c], [c, d], [d, a], [d, a]]).
%% mrule([[a], [b], [c, b], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c], [b, c], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, c], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [c], [b, c], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [c], [b, c], [d, a], [d, a], [d, a]]).
%% mrule([[a, b], [c], [b, c], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [c], [b, c], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, c], [d, a], [d, b], [d, b]]).
%% mrule([[a, b], [c], [b, c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [b, c], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [c], [b, c], [d, a], [d, e], [d, e]]).
%% mrule([[a, b], [c], [b, c], [d, a], [d, e], [e, a]]).
%% mrule([[a, b], [c], [b, c], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [c], [b, c], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [c], [b, c], [d, a], [d, e], [e, d]]).
%% mrule([[a, b], [c], [b, c], [d, a], [e, a], [e, d]]).
%% mrule([[a, b], [c], [b, c], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [c], [b, c], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [c], [b, c], [d, a], [e, d], [e, d]]).
%% mrule([[a, b], [c], [b, c], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [c], [b, c], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [c], [b, c], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [c], [b, c], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [c], [b, c], [d, e], [d, e], [e, a]]).
%% mrule([[a, b], [c], [b, c], [d, e], [e, a], [e, d]]).
%% mrule([[a, b], [c], [b, d], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [c], [b, d], [b, d], [c, a], [c, a]]).
%% mrule([[a, b], [c], [b, d], [b, d], [c, a], [c, b]]).
%% mrule([[a], [b], [c, d], [c, d], [b, a], [b, d]]).
%% mrule([[a, b], [c], [b, d], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [c], [b, d], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [c], [b, d], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [c], [b, d], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [c], [b, d], [b, d], [c, b], [d, a]]).
%% mrule([[a], [b], [c, d], [c, d], [b, d], [d, a]]).
%% mrule([[a, b], [c], [b, d], [b, d], [c, d], [d, a]]).
%% mrule([[a, b], [c], [b, d], [b, d], [c, e], [e, a]]).
%% mrule([[a, b], [c], [b, d], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, d], [b, d], [e, a], [e, c]]).
%% mrule([[a, b], [c], [b, d], [b, e], [c, a], [d, e]]).
%% mrule([[a, b], [c], [b, d], [b, e], [c, a], [e, d]]).
%% mrule([[a], [b], [c, d], [c, e], [b, d], [e, a]]).
%% mrule([[a, b], [c], [b, d], [b, e], [c, d], [e, a]]).
%% mrule([[a], [b], [c, d], [c, e], [b, e], [d, a]]).
%% mrule([[a, b], [c], [b, d], [b, e], [c, e], [d, a]]).
%% mrule([[a, b], [c], [b, d], [b, e], [d, a], [e, c]]).
%% mrule([[a, b], [c], [b, d], [b, e], [d, c], [e, a]]).
%% mrule([[a, b], [c], [b, d], [c, a], [c, a], [c, d]]).
%% mrule([[a, b], [c], [b, d], [c, a], [c, a], [d, a]]).
%% mrule([[a, b], [c], [b, d], [c, a], [c, a], [d, b]]).
%% mrule([[a, b], [c], [b, d], [c, a], [c, a], [d, c]]).
%% mrule([[a, b], [c], [b, d], [c, a], [c, b], [c, d]]).
%% mrule([[a, b], [c], [b, d], [c, a], [c, b], [d, a]]).
%% mrule([[a, b], [c], [b, d], [c, a], [c, b], [d, b]]).
%% mrule([[a, b], [c], [b, d], [c, a], [c, b], [d, c]]).
%% mrule([[a, b], [c], [b, d], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [c], [b, d], [c, a], [c, d], [d, a]]).
%% mrule([[a], [b], [c, d], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [c], [b, d], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [c], [b, d], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [c], [b, d], [c, a], [c, e], [d, e]]).
%% mrule([[a, b], [c], [b, d], [c, a], [c, e], [e, d]]).
%% mrule([[a, b], [c], [b, d], [c, a], [d, a], [d, a]]).
%% mrule([[a, b], [c], [b, d], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [c], [b, d], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, d], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [c], [b, d], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [b, d], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [c], [b, d], [c, a], [d, e], [d, e]]).
%% mrule([[a, b], [c], [b, d], [c, a], [d, e], [e, a]]).
%% mrule([[a, b], [c], [b, d], [c, a], [d, e], [e, b]]).
%% mrule([[a, b], [c], [b, d], [c, a], [d, e], [e, c]]).
%% mrule([[a, b], [c], [b, d], [c, a], [d, e], [e, d]]).
%% mrule([[a, b], [c], [b, d], [c, a], [e, a], [e, d]]).
%% mrule([[a, b], [c], [b, d], [c, a], [e, b], [e, d]]).
%% mrule([[a, b], [c], [b, d], [c, a], [e, c], [e, d]]).
%% mrule([[a, b], [c], [b, d], [c, a], [e, d], [e, d]]).
%% mrule([[a, b], [c], [b, d], [c, b], [c, b], [d, a]]).
%% mrule([[a, b], [c], [b, d], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [c], [b, d], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [c], [b, d], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [c], [b, d], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, d], [c, b], [d, e], [e, a]]).
%% mrule([[a, b], [c], [b, d], [c, b], [e, a], [e, d]]).
%% mrule([[a, b], [c], [b, d], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [c], [b, d], [c, d], [c, e], [e, a]]).
%% mrule([[a, b], [c], [b, d], [c, d], [d, a], [d, a]]).
%% mrule([[a], [b], [c, d], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, d], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c], [b, d], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, d], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [c], [b, d], [c, d], [e, a], [e, a]]).
%% mrule([[a], [b], [c, d], [b, d], [e, a], [e, c]]).
%% mrule([[a, b], [c], [b, d], [c, d], [e, a], [e, b]]).
%% mrule([[a, b], [c], [b, d], [c, d], [e, a], [e, c]]).
%% mrule([[a, b], [c], [b, d], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [c], [b, d], [c, e], [d, a], [d, e]]).
%% mrule([[a, b], [c], [b, d], [c, e], [d, a], [e, a]]).
%% mrule([[a], [b], [c, d], [b, e], [d, a], [e, c]]).
%% mrule([[a, b], [c], [b, d], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [c], [b, d], [c, e], [d, a], [e, d]]).
%% mrule([[a, b], [c], [b, d], [c, e], [d, b], [e, a]]).
%% mrule([[a, b], [c], [b, d], [c, e], [d, c], [e, a]]).
%% mrule([[a, b], [c], [b, d], [c, e], [d, e], [e, a]]).
%% mrule([[a, b], [c], [b, d], [c, e], [e, a], [e, d]]).
%% mrule([[a, b], [c], [b, d], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [c], [b, d], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [b, d], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [c], [b, d], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [c], [b, d], [d, a], [e, a], [e, c]]).
%% mrule([[a, b], [c], [b, d], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [c], [b, d], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [c], [b, d], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [c], [b, d], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [c], [b, d], [d, c], [e, a], [e, a]]).
%% mrule([[a, b], [c], [b, d], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [c], [b, d], [d, c], [e, a], [e, c]]).
%% mrule([[a, b], [c], [b, d], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [c], [b, d], [d, e], [e, a], [e, c]]).
%% mrule([[a, b], [c], [b, d], [e, a], [e, c], [e, d]]).
%% mrule([[a, b], [c], [c, a], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [c], [c, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [c], [c, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [c], [c, a], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [c], [c, a], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [c], [c, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [c, a], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [c], [c, a], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [c], [c, a], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [c], [c, a], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [c], [c, a], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [c], [c, a], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [c], [c, a], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [c], [c, a], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [c], [c, a], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [c], [c, a], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [c], [c, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [c], [c, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [c], [c, a], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c], [c, a], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [c], [c, a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [c], [c, a], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [c], [c, a], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [c], [c, a], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [c], [c, a], [d, a], [d, b], [d, b]]).
%% mrule([[a, b], [c], [c, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [c, a], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [c], [c, a], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [c], [c, a], [d, b], [d, b], [d, b]]).
%% mrule([[a, b], [c], [c, a], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [c], [c, a], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [c], [c, a], [d, b], [d, e], [d, e]]).
%% mrule([[a, b], [c], [c, a], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [c], [c, a], [d, b], [d, e], [e, b]]).
%% mrule([[a, b], [c], [c, a], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [c], [c, a], [d, b], [d, e], [e, d]]).
%% mrule([[a, b], [c], [c, a], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [c], [c, a], [d, b], [e, b], [e, d]]).
%% mrule([[a, b], [c], [c, a], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [c], [c, a], [d, b], [e, d], [e, d]]).
%% mrule([[a, b], [c], [c, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [c], [c, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [c], [c, a], [d, e], [d, e], [e, b]]).
%% mrule([[a, b], [c], [c, a], [d, e], [e, b], [e, d]]).
%% mrule([[a, b], [c], [c, b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [c], [c, b], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [c], [c, b], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [c], [c, b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [c], [c, b], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [c], [c, b], [c, d], [d, a], [d, a]]).
%% mrule([[a, b], [c], [c, b], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c], [c, b], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [c], [c, b], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [c], [c, b], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [c], [c, b], [d, a], [d, a], [d, a]]).
%% mrule([[a, b], [c], [c, b], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [c], [c, b], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [c], [c, b], [d, a], [d, b], [d, b]]).
%% mrule([[a, b], [c], [c, b], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [c, b], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [c], [c, b], [d, a], [d, e], [d, e]]).
%% mrule([[a, b], [c], [c, b], [d, a], [d, e], [e, a]]).
%% mrule([[a, b], [c], [c, b], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [c], [c, b], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [c], [c, b], [d, a], [d, e], [e, d]]).
%% mrule([[a, b], [c], [c, b], [d, a], [e, a], [e, d]]).
%% mrule([[a, b], [c], [c, b], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [c], [c, b], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [c], [c, b], [d, a], [e, d], [e, d]]).
%% mrule([[a, b], [c], [c, b], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [c], [c, b], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [c], [c, b], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [c], [c, b], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [c], [c, b], [d, e], [d, e], [e, a]]).
%% mrule([[a, b], [c], [c, b], [d, e], [e, a], [e, d]]).
%% mrule([[a, b], [c], [c, d], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c], [c, d], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [c], [c, d], [c, e], [d, b], [e, a]]).
%% mrule([[a, b], [c], [c, d], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [c], [c, d], [d, a], [d, b], [d, b]]).
%% mrule([[a, b], [c], [c, d], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [c, d], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [c], [c, d], [d, a], [e, a], [e, b]]).
%% mrule([[a, b], [c], [c, d], [d, a], [e, b], [e, b]]).
%% mrule([[a, b], [c], [c, d], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [c], [c, d], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [c], [c, d], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [c], [c, d], [d, b], [e, a], [e, a]]).
%% mrule([[a, b], [c], [c, d], [d, b], [e, a], [e, b]]).
%% mrule([[a, b], [c], [c, d], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [c], [c, d], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [c], [c, d], [d, e], [e, a], [e, b]]).
%% mrule([[a, b], [c], [c, d], [e, a], [e, b], [e, d]]).
%% mrule([[a, b], [c], [d, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c], [d, a], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [c], [d, a], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [c], [d, a], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [c], [d, a], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [c], [d, a], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [c], [d, a], [d, b], [e, b], [e, c]]).
%% mrule([[a, b], [c], [d, a], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [c], [d, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [c], [d, a], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [c], [d, a], [d, c], [e, b], [e, b]]).
%% mrule([[a, b], [c], [d, a], [d, c], [e, b], [e, c]]).
%% mrule([[a, b], [c], [d, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [c], [d, a], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [c], [d, a], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [c], [d, b], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [c], [d, b], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [c], [d, b], [d, c], [e, a], [e, a]]).
%% mrule([[a, b], [c], [d, b], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [c], [d, b], [d, c], [e, a], [e, c]]).
%% mrule([[a, b], [c], [d, b], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [c], [d, b], [d, e], [e, a], [e, c]]).
%% mrule([[a, b], [c], [d, b], [e, a], [e, c], [e, d]]).
%% mrule([[a, b], [c], [d, c], [d, e], [e, a], [e, b]]).
%% mrule([[a, b], [c], [d, c], [e, a], [e, b], [e, d]]).
%% mrule([[a, b], [c], [a, b], [d, a], [d, e], [c, e]]).
%% mrule([[a, b], [c], [a, b], [d, a], [c, e], [e, d]]).
%% mrule([[a, b], [c], [a, b], [d, b], [d, e], [c, e]]).
%% mrule([[a, b], [c], [a, b], [d, b], [c, e], [e, d]]).
%% mrule([[a, b], [c], [a, c], [d, b], [d, e], [c, e]]).
%% mrule([[a, b], [c], [a, c], [d, b], [c, e], [e, d]]).
%% mrule([[a, b], [c], [b, a], [d, a], [d, e], [c, e]]).
%% mrule([[a, b], [c], [b, a], [d, a], [c, e], [e, d]]).
%% mrule([[a], [b], [c, a], [d, c], [d, e], [b, e]]).
%% mrule([[a, b], [c], [b, a], [d, b], [d, e], [c, e]]).
%% mrule([[a], [b], [c, a], [d, c], [b, e], [e, d]]).
%% mrule([[a, b], [c], [b, a], [d, b], [c, e], [e, d]]).
%% mrule([[a, b], [c], [b, c], [d, a], [d, e], [c, e]]).
%% mrule([[a, b], [c], [b, c], [d, a], [c, e], [e, d]]).
%% mrule([[a, b], [c], [d, a], [d, a], [c, e], [e, b]]).
%% mrule([[a, b], [c], [d, a], [d, b], [d, e], [c, e]]).
%% mrule([[a, b], [c], [d, a], [d, b], [c, e], [e, a]]).
%% mrule([[a, b], [c], [d, a], [d, b], [c, e], [e, b]]).
%% mrule([[a, b], [c], [d, a], [d, b], [c, e], [e, d]]).
%% mrule([[a, b], [c], [d, a], [d, c], [c, e], [e, b]]).
%% mrule([[a, b], [c], [d, a], [d, e], [c, b], [c, e]]).
%% mrule([[a, b], [c], [d, a], [d, e], [c, e], [e, b]]).
%% mrule([[a, b], [c], [d, a], [c, b], [c, e], [e, d]]).
%% mrule([[a, b], [c], [d, a], [c, e], [e, b], [e, d]]).
%% mrule([[a, b], [c], [d, b], [d, b], [c, e], [e, a]]).
%% mrule([[a, b], [c], [d, b], [d, c], [c, e], [e, a]]).
%% mrule([[a, b], [c], [d, b], [d, e], [c, a], [c, e]]).
%% mrule([[a, b], [c], [d, b], [d, e], [c, e], [e, a]]).
%% mrule([[a, b], [c], [d, b], [c, a], [c, e], [e, d]]).
%% mrule([[a, b], [c], [d, b], [c, e], [e, a], [e, d]]).
%% mrule([[a, b], [a, b], [a, b], [a, b], [a, b], [a, b]]).
%% mrule([[a, b], [a, b], [a, b], [a, b], [a, b], [b, a]]).
%% mrule([[a, b], [a, b], [a, b], [a, b], [a, c], [a, c]]).
%% mrule([[a, b], [a, b], [a, b], [a, b], [a, c], [b, c]]).
%% mrule([[a, b], [a, b], [a, b], [a, b], [a, c], [c, a]]).
%% mrule([[a, b], [a, b], [a, b], [a, b], [a, c], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [a, b], [b, a], [b, a]]).
%% mrule([[a, b], [a, b], [a, b], [a, b], [b, c], [b, c]]).
%% mrule([[a, b], [a, b], [a, b], [a, b], [b, c], [c, a]]).
%% mrule([[a, b], [a, b], [a, b], [a, b], [b, c], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [a, b], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [a, b], [a, b], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [a, b], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [a, c], [a, c]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [a, c], [c, a]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [a, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [a, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [a, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [b, a], [b, a], [b, a]]).
%% mrule([[a, b], [a, b], [a, b], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [a, b], [a, b], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [a, b], [a, b], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [a, b], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [a, b], [a, b], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a, b], [a, b], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, b], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [a, b], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, b], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [a, b], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [a, b], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [c, a], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [a, b], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, b], [c, a], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [a, b], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [a, b], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [a, b], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [a, b], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [a, b], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [a, b], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [a, b], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [a, b], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [a, c], [a, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [a, c], [c, a]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [a, d], [a, d]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [a, d], [b, d]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [a, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [a, d], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [a, d], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [a, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [b, a], [b, a]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [b, d], [b, d]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [b, d], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [b, d], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [d, a], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [d, a], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [d, b], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [a, d], [b, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [a, d], [c, a]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [a, d], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [a, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [a, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [b, a], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [b, a], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [b, c], [b, d]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [b, c], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [b, c], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [b, c], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [b, c], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [b, d], [c, a]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [b, d], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [b, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [b, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [c, a], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [c, a], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [c, a], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [c, a], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [c, b], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [c, b], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [c, b], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [c, b], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [c, e], [d, e]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [c, e], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [a, c], [a, d], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [b, a], [b, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [b, a], [c, a]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [b, a], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [b, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [b, d], [b, d]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [b, d], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [d, a], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [d, b], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [b, d], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [b, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [b, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [c, b], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [c, b], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [c, b], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [c, e], [d, e]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [c, e], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [a, c], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [c, a], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [a, c], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [c, a], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [c, a], [d, a], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [a, c], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [c, e], [d, e]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [c, e], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [d, a], [d, a]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [d, e], [d, e]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [d, e], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [c, d], [e, d], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [a, c], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [a, c], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [a, c], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [a, b], [a, c], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [a, b], [a, c], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a, b], [a, c], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [a, c], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [a, c], [d, e], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [d, a], [d, e], [c, e]]).
%% mrule([[a, b], [a, b], [a, c], [d, a], [c, e], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [d, b], [d, e], [c, e]]).
%% mrule([[a, b], [a, b], [a, c], [d, b], [c, e], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [d, c], [d, e], [c, e]]).
%% mrule([[a, b], [a, b], [a, c], [d, c], [c, e], [e, d]]).
%% mrule([[a, b], [a, b], [a, c], [d, e], [d, e], [c, e]]).
%% mrule([[a, b], [a, b], [a, c], [d, e], [c, e], [e, d]]).
%% mrule([[a, b], [a, b], [b, a], [b, a], [b, a], [b, a]]).
%% mrule([[a, b], [a, b], [b, a], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [a, b], [b, a], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [a, b], [b, a], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [a, b], [b, a], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [b, a], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [b, a], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [b, a], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [a, b], [b, a], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a, b], [b, a], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [a, b], [b, a], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [a, b], [b, a], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [a, b], [b, a], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [b, a], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [b, a], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [b, a], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [b, a], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [b, a], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [b, a], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [b, a], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [b, a], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [b, a], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [b, a], [c, a], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [b, a], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [b, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [b, a], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [b, a], [c, a], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [b, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [b, a], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [b, a], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [b, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [b, a], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [b, a], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [b, a], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [b, a], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [b, a], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [b, a], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [b, a], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [b, a], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [b, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [b, a], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [b, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [b, a], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [b, a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [b, d], [b, d]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [b, d], [d, b]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [d, a], [d, a]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [d, b], [d, b]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [b, d], [c, b]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [b, d], [c, d]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [b, d], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [c, b], [c, d]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [c, b], [d, b]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [c, b], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [c, e], [d, e]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [c, e], [e, d]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [b, c], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [b, c], [c, a], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [b, c], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [b, c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [b, c], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [b, c], [c, a], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [b, c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [b, c], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [c, a], [d, a], [d, a]]).
%% mrule([[a, b], [a, b], [b, c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [a, b], [b, c], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [a, b], [b, c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [b, c], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [b, c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [b, c], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [b, c], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [a, b], [b, c], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [a, b], [b, c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [a, b], [b, c], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [c, e], [d, e]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [c, e], [e, d]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [d, a], [d, a]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [d, e], [d, e]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [d, e], [e, d]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [b, c], [c, d], [e, d], [e, d]]).
%% mrule([[a, b], [a, b], [b, c], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [b, c], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [b, c], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [b, c], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [b, c], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [b, c], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [a, b], [b, c], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [a, b], [b, c], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a, b], [b, c], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [b, c], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [a, b], [b, c], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [a, b], [b, c], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a, b], [b, c], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [b, c], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [a, b], [b, c], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [b, c], [d, e], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [b, c], [d, a], [d, e], [c, e]]).
%% mrule([[a, b], [a, b], [b, c], [d, a], [c, e], [e, d]]).
%% mrule([[a, b], [a, b], [b, c], [d, b], [d, e], [c, e]]).
%% mrule([[a, b], [a, b], [b, c], [d, b], [c, e], [e, d]]).
%% mrule([[a, b], [a, b], [b, c], [d, c], [d, e], [c, e]]).
%% mrule([[a, b], [a, b], [b, c], [d, c], [c, e], [e, d]]).
%% mrule([[a, b], [a, b], [b, c], [d, e], [d, e], [c, e]]).
%% mrule([[a, b], [a, b], [b, c], [d, e], [c, e], [e, d]]).
%% mrule([[a, b], [a, b], [c, a], [c, a], [c, a], [c, a]]).
%% mrule([[a, b], [a, b], [c, a], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [a, b], [c, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [c, a], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [c, a], [c, a], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [c, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [c, a], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [c, a], [d, a], [d, a]]).
%% mrule([[a, b], [a, b], [c, a], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [a, b], [c, a], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [a, b], [c, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [c, a], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [c, a], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [c, a], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [c, a], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [a, b], [c, a], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [a, b], [c, a], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [a, b], [c, a], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [c, e], [d, e]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [c, e], [e, d]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [d, a], [d, a]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [d, e], [d, e]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [d, e], [e, d]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [c, a], [c, d], [e, d], [e, d]]).
%% mrule([[a, b], [a, b], [c, a], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [c, a], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [c, a], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [c, a], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [c, a], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [c, a], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [a, b], [c, a], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [a, b], [c, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a, b], [c, a], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [c, a], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [a, b], [c, a], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [a, b], [c, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a, b], [c, a], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [c, a], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [a, b], [c, a], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [c, a], [d, e], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [c, b], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a, b], [c, b], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [c, b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [c, b], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [c, b], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [c, b], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [a, b], [c, b], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [a, b], [c, b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [c, b], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [a, b], [c, b], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [c, b], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [c, d], [c, d]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [c, d], [d, c]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [c, e], [d, e]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [c, e], [e, d]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [d, a], [d, a]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [d, e], [d, e]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [d, e], [e, d]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [c, b], [c, d], [e, d], [e, d]]).
%% mrule([[a, b], [a, b], [c, b], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [c, b], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [c, b], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [c, b], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [c, b], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [c, b], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [c, b], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [c, b], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [c, b], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [c, b], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [c, b], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [a, b], [c, b], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [a, b], [c, b], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a, b], [c, b], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [c, b], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [a, b], [c, b], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [a, b], [c, b], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a, b], [c, b], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [c, b], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [a, b], [c, b], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [c, b], [d, e], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [c, d], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [a, b], [c, d], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, b], [c, d], [c, d], [c, e], [e, a]]).
%% mrule([[a, b], [a, b], [c, d], [c, d], [c, e], [e, b]]).
%% mrule([[a, b], [a, b], [c, d], [c, d], [d, a], [d, a]]).
%% mrule([[a, b], [a, b], [c, d], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a, b], [c, d], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [c, d], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [a, b], [c, d], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [c, d], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [a, b], [c, d], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [a, b], [c, d], [c, d], [e, a], [e, c]]).
%% mrule([[a, b], [a, b], [c, d], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [a, b], [c, d], [c, d], [e, b], [e, c]]).
%% mrule([[a, b], [a, b], [c, d], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [a, b], [c, d], [c, e], [c, e], [d, a]]).
%% mrule([[a, b], [a, b], [c, d], [c, e], [c, e], [d, b]]).
%% mrule([[a, b], [a, b], [c, d], [c, e], [d, a], [d, e]]).
%% mrule([[a, b], [a, b], [c, d], [c, e], [d, a], [e, a]]).
%% mrule([[a, b], [a, b], [c, d], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [a, b], [c, d], [c, e], [d, a], [e, c]]).
%% mrule([[a, b], [a, b], [c, d], [c, e], [d, a], [e, d]]).
%% mrule([[a, b], [a, b], [c, d], [c, e], [d, b], [d, e]]).
%% mrule([[a, b], [a, b], [c, d], [c, e], [d, b], [e, a]]).
%% mrule([[a, b], [a, b], [c, d], [c, e], [d, b], [e, b]]).
%% mrule([[a, b], [a, b], [c, d], [c, e], [d, b], [e, c]]).
%% mrule([[a, b], [a, b], [c, d], [c, e], [d, b], [e, d]]).
%% mrule([[a, b], [a, b], [c, d], [c, e], [d, c], [e, a]]).
%% mrule([[a, b], [a, b], [c, d], [c, e], [d, c], [e, b]]).
%% mrule([[a, b], [a, b], [c, d], [c, e], [d, e], [e, a]]).
%% mrule([[a, b], [a, b], [c, d], [c, e], [d, e], [e, b]]).
%% mrule([[a, b], [a, b], [c, d], [c, e], [e, a], [e, d]]).
%% mrule([[a, b], [a, b], [c, d], [c, e], [e, b], [e, d]]).
%% mrule([[a, b], [a, b], [c, d], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, b], [c, d], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [c, d], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [c, d], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [c, d], [d, a], [e, a], [e, c]]).
%% mrule([[a, b], [a, b], [c, d], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [a, b], [c, d], [d, a], [e, c], [e, c]]).
%% mrule([[a, b], [a, b], [c, d], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [c, d], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, b], [c, d], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, b], [c, d], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a, b], [c, d], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [a, b], [c, d], [d, b], [e, b], [e, c]]).
%% mrule([[a, b], [a, b], [c, d], [d, b], [e, c], [e, c]]).
%% mrule([[a, b], [a, b], [c, d], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [c, d], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [a, b], [c, d], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a, b], [c, d], [d, c], [e, a], [e, c]]).
%% mrule([[a, b], [a, b], [c, d], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [a, b], [c, d], [d, c], [e, b], [e, c]]).
%% mrule([[a, b], [a, b], [c, d], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a, b], [c, d], [d, e], [e, a], [e, c]]).
%% mrule([[a, b], [a, b], [c, d], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [a, b], [c, d], [e, a], [e, c], [e, d]]).
%% mrule([[a, b], [a, b], [c, d], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [a, c], [b, a]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [a, c], [b, c]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [a, c], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [a, d], [b, d]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [a, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [b, a], [b, a]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [b, a], [b, c]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [b, a], [c, a]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [b, a], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [b, c], [b, c]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [b, c], [c, a]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [b, c], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [b, d], [b, d]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [b, d], [d, a]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [b, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [c, a], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, c], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [a, d], [b, a]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [a, d], [b, c]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [a, d], [b, d]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [a, d], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [a, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [b, a], [b, d]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [b, a], [c, d]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [b, a], [d, a]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [b, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [b, a], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [b, c], [b, d]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [b, c], [c, d]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [b, c], [d, a]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [b, c], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [b, c], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [b, d], [b, d]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [b, d], [c, a]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [b, d], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [b, d], [d, a]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [b, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [b, e], [d, e]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [b, e], [e, d]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [c, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [c, b], [c, d]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [c, b], [d, a]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [c, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [c, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [a, c], [a, d], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [b, a], [b, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [b, a], [b, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [b, a], [c, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [b, a], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [b, d], [b, d]]).
%% mrule([[a], [a, b], [a, b], [c, a], [c, d], [b, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [b, d], [d, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [b, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [c, d], [d, a]]).
%% mrule([[a], [a, b], [a, b], [c, a], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [b, d], [b, d]]).
%% mrule([[a], [a, b], [a, b], [c, b], [c, d], [b, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [b, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [c, d], [d, a]]).
%% mrule([[a], [a, b], [a, b], [c, b], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [b, d], [b, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [b, d], [c, b]]).
%% mrule([[a], [a, b], [a, b], [c, d], [c, d], [b, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [b, d], [d, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [b, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [b, e], [d, e]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [b, e], [e, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [c, b], [c, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [c, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [c, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [c, d], [d, a]]).
%% mrule([[a], [a, b], [a, b], [c, d], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [c, e], [d, e]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [a, c], [b, d], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [a, c], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [a, c], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [a, c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a, c], [a, c], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [a, c], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [a, c], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [a, c], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [d, a], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [a, c], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [a, c], [d, b], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, c], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [a, c], [d, b], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [a, c], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [a, c], [d, b], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [a, c], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [a, c], [d, b], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [a, c], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [a, c], [d, b], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [a, c], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [a, c], [d, b], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [a, c], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [a, c], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [a, c], [d, e], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [a, c], [d, e], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [a, d], [b, c]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [a, d], [c, b]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [b, a], [b, c]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [b, a], [c, a]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [b, a], [c, b]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [b, a], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [b, a], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [b, c], [b, c]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [b, c], [b, d]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [b, c], [c, a]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [b, c], [c, b]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [b, c], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [b, c], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [b, c], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [b, c], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [b, d], [c, a]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [b, d], [c, b]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [b, e], [c, e]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [b, e], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [c, a], [c, b]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [c, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [c, b], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [c, b], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [c, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [c, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [c, e], [e, b]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [a, d], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [a, e], [b, c], [d, e]]).
%% mrule([[a, b], [a, c], [a, d], [a, e], [b, c], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [a, e], [b, d], [c, e]]).
%% mrule([[a, b], [a, c], [a, d], [a, e], [b, d], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [a, e], [b, e], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [a, e], [b, e], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [a, e], [c, b], [d, e]]).
%% mrule([[a, b], [a, c], [a, d], [a, e], [c, b], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [a, e], [c, d], [e, b]]).
%% mrule([[a, b], [a, c], [a, d], [a, e], [c, e], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [a, e], [d, b], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [a, e], [d, c], [e, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [b, a], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [b, a], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [b, c], [b, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [b, c], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [b, c], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [b, c], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [b, c], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [b, d], [c, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [b, d], [c, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [c, a], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [c, a], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [c, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [c, a], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [c, b], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [c, b], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [c, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [c, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [c, d], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [c, e], [d, e]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, a], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [b, c], [b, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [b, c], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [b, c], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [b, c], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [b, c], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [b, d], [b, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [b, d], [c, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [b, d], [c, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [b, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [b, e], [d, e]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [b, e], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [c, a], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [c, a], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [c, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [c, a], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [c, b], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [c, b], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [c, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [c, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [c, e], [d, e]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, c], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [b, d], [c, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [b, d], [d, c]]).
%% mrule([[a], [a, b], [a, c], [d, c], [d, e], [b, e]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [b, e], [c, e]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [b, e], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, a], [c, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, a], [c, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, b], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, d], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, e], [c, e]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, e], [d, e]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, e], [e, a]]).
%% mrule([[a], [a, b], [a, c], [d, c], [b, e], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, e], [e, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, e], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [e, a], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [e, c], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [b, e], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [b, e], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [c, a], [d, e]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [c, a], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [c, b], [d, e]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [c, b], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [c, d], [c, e]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [c, d], [d, e]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [c, d], [e, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [c, d], [e, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [c, d], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [c, d], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [c, e], [d, a]]).
%% mrule([[a], [a, b], [a, c], [d, e], [b, e], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [c, e], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [c, e], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [c, e], [d, e]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [d, a], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [d, b], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [d, c], [d, e]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [d, c], [e, a]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [d, c], [e, b]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [d, c], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [d, c], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [b, e], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [c, a], [c, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, a], [c, b], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [c, a], [c, b], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [c, a], [c, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, a], [c, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [c, a], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, a], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [c, b], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [c, b], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [c, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [c, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [c, e], [d, e]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [c, b], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, d], [c, e], [e, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, d], [e, a], [e, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, d], [e, b], [e, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, d], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [c, e], [c, e], [d, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, e], [d, b], [d, e]]).
%% mrule([[a, b], [a, c], [a, d], [c, e], [d, b], [e, a]]).
%% mrule([[a, b], [a, c], [a, d], [c, e], [d, b], [e, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, e], [d, b], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [c, e], [d, b], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [c, e], [d, c], [e, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, e], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [a, d], [c, e], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [a, d], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [d, b], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [d, b], [e, c], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [a, d], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [a, c], [a, d], [d, c], [e, b], [e, b]]).
%% mrule([[a, b], [a, c], [a, d], [d, c], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [a, d], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [a, d], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [b, a], [b, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [b, a], [c, a]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [b, a], [c, b]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [b, c], [c, b]]).
%% mrule([[a], [a, b], [c, a], [c, a], [c, d], [b, d]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [c, b], [c, b]]).
%% mrule([[a], [a, b], [c, a], [c, a], [b, d], [b, d]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [c, d], [c, d]]).
%% mrule([[a], [a, b], [c, a], [c, a], [b, d], [d, a]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [c, d], [d, a]]).
%% mrule([[a], [a, b], [c, a], [c, a], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [c, d], [d, b]]).
%% mrule([[a], [a, b], [c, a], [c, a], [b, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [b, d], [b, d]]).
%% mrule([[a], [a, b], [c, a], [c, b], [c, d], [b, d]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [b, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [c, b], [c, b]]).
%% mrule([[a], [a, b], [c, a], [c, b], [b, d], [b, d]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [c, d], [c, d]]).
%% mrule([[a], [a, b], [c, a], [c, b], [b, d], [d, a]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [c, d], [d, a]]).
%% mrule([[a], [a, b], [c, a], [c, b], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [c, d], [d, b]]).
%% mrule([[a], [a, b], [c, a], [c, b], [b, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [b, d], [c, b]]).
%% mrule([[a], [a, b], [c, a], [c, d], [c, d], [b, d]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [b, d], [d, c]]).
%% mrule([[a], [a, b], [c, a], [c, d], [b, a], [b, d]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [c, b], [c, d]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [c, b], [d, b]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [c, b], [d, c]]).
%% mrule([[a], [a, b], [c, a], [c, d], [b, d], [b, d]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [c, d], [c, d]]).
%% mrule([[a], [a, b], [c, a], [c, d], [b, d], [d, a]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [c, d], [d, a]]).
%% mrule([[a], [a, b], [c, a], [c, d], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [c, d], [d, b]]).
%% mrule([[a], [a, b], [c, a], [c, d], [b, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [c, d], [d, c]]).
%% mrule([[a], [a, b], [c, a], [c, d], [b, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [c, e], [d, e]]).
%% mrule([[a], [a, b], [c, a], [c, d], [b, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, a], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [c, a], [c, a], [c, a]]).
%% mrule([[a, b], [a, c], [b, a], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [a, c], [b, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [b, a], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, a], [c, a], [c, d], [d, a]]).
%% mrule([[a], [a, b], [c, a], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, a], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [c, a], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [b, a], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [b, a], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [b, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [b, a], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, a], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a, c], [b, a], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, a], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [b, a], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [b, a], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [b, a], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [c, d], [d, a]]).
%% mrule([[a], [a, b], [c, a], [b, d], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [c, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [d, a], [d, a]]).
%% mrule([[a], [a, b], [c, a], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [d, a], [d, c]]).
%% mrule([[a], [a, b], [c, a], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [d, b], [d, b]]).
%% mrule([[a], [a, b], [c, a], [b, d], [d, c], [d, b]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [d, e], [e, a]]).
%% mrule([[a], [a, b], [c, a], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [e, a], [e, d]]).
%% mrule([[a], [a, b], [c, a], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [c, d], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, a], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, a], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, a], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, a], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [b, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [b, a], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, a], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, a], [d, e], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [b, d], [b, d]]).
%% mrule([[a], [a, b], [c, b], [c, b], [c, d], [b, d]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [b, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [c, b], [c, b]]).
%% mrule([[a], [a, b], [c, b], [c, b], [b, d], [b, d]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [c, d], [c, d]]).
%% mrule([[a], [a, b], [c, b], [c, b], [b, d], [d, a]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [c, d], [d, a]]).
%% mrule([[a], [a, b], [c, b], [c, b], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [c, d], [d, b]]).
%% mrule([[a], [a, b], [c, b], [c, b], [b, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [b, d], [b, d]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [b, d], [c, b]]).
%% mrule([[a], [a, b], [c, b], [c, d], [c, d], [b, d]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [b, d], [d, a]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [b, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [b, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [b, e], [e, d]]).
%% mrule([[a], [a, b], [c, b], [c, d], [b, a], [b, d]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [c, b], [c, d]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [c, b], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [c, b], [d, c]]).
%% mrule([[a], [a, b], [c, b], [c, d], [b, d], [b, d]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [c, d], [c, d]]).
%% mrule([[a], [a, b], [c, b], [c, d], [b, d], [d, a]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [c, d], [d, a]]).
%% mrule([[a], [a, b], [c, b], [c, d], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [c, d], [d, b]]).
%% mrule([[a], [a, b], [c, b], [c, d], [b, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [c, d], [d, c]]).
%% mrule([[a], [a, b], [c, b], [c, d], [b, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [c, e], [d, e]]).
%% mrule([[a], [a, b], [c, b], [c, d], [b, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [b, d], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [c, a], [c, a], [c, a]]).
%% mrule([[a, b], [a, c], [b, c], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [a, c], [b, c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [b, c], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, c], [c, a], [c, d], [d, a]]).
%% mrule([[a], [a, b], [c, b], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [c, a], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [b, c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [b, c], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a, c], [b, c], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [b, c], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [c, d], [d, a]]).
%% mrule([[a], [a, b], [c, b], [b, d], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [c, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [d, a], [d, a]]).
%% mrule([[a], [a, b], [c, b], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [d, a], [d, c]]).
%% mrule([[a], [a, b], [c, b], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [d, b], [d, b]]).
%% mrule([[a], [a, b], [c, b], [b, d], [d, c], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [d, e], [e, a]]).
%% mrule([[a], [a, b], [c, b], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [e, a], [e, d]]).
%% mrule([[a], [a, b], [c, b], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [c, d], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, a], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [b, c], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [d, a], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [d, a], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, c], [d, a], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [b, c], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [b, c], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, c], [d, a], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, a], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, a], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, b], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [b, c], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [d, b], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, c], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [b, c], [d, b], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [b, c], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, c], [d, b], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, b], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, b], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, c], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, c], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [b, c], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [b, c], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, c], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, e], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [b, c], [d, e], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [b, c], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, c], [d, e], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, e], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, e], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [b, d], [c, b]]).
%% mrule([[a], [a, b], [c, d], [c, d], [c, d], [b, d]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [b, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [b, d], [d, c]]).
%% mrule([[a], [a, b], [c, d], [c, d], [c, e], [b, e]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [b, e], [c, e]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [b, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, a], [c, a]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, a], [c, b]]).
%% mrule([[a], [a, b], [c, d], [c, d], [b, a], [b, d]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, b], [c, d]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, b], [d, b]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, b], [d, c]]).
%% mrule([[a], [a, b], [c, d], [c, d], [b, d], [b, d]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, d], [c, d]]).
%% mrule([[a], [a, b], [c, d], [c, d], [b, d], [d, a]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, d], [d, a]]).
%% mrule([[a], [a, b], [c, d], [c, d], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, d], [d, b]]).
%% mrule([[a], [a, b], [c, d], [c, d], [b, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, e], [c, e]]).
%% mrule([[a], [a, b], [c, d], [c, d], [b, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, e], [e, a]]).
%% mrule([[a], [a, b], [c, d], [c, d], [b, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, e], [e, b]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, e], [e, c]]).
%% mrule([[a], [a, b], [c, d], [c, d], [b, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [e, a], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [e, c], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [b, d], [e, c], [e, d]]).
%% mrule([[a], [a, b], [c, d], [c, e], [c, e], [b, d]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [b, e], [c, d]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [b, e], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [c, a], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [c, a], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [c, b], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [c, b], [e, d]]).
%% mrule([[a], [a, b], [c, d], [c, e], [b, d], [b, e]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [c, d], [c, e]]).
%% mrule([[a], [a, b], [c, d], [c, e], [b, d], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [c, d], [d, e]]).
%% mrule([[a], [a, b], [c, d], [c, e], [b, d], [e, a]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [c, d], [e, a]]).
%% mrule([[a], [a, b], [c, d], [c, e], [b, d], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [c, d], [e, b]]).
%% mrule([[a], [a, b], [c, d], [c, e], [b, d], [e, b]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [c, d], [e, c]]).
%% mrule([[a], [a, b], [c, d], [c, e], [b, d], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [c, d], [e, d]]).
%% mrule([[a], [a, b], [c, d], [c, e], [b, e], [d, a]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [c, e], [d, a]]).
%% mrule([[a], [a, b], [c, d], [c, e], [b, e], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [c, e], [d, b]]).
%% mrule([[a], [a, b], [c, d], [c, e], [b, e], [d, b]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [c, e], [d, c]]).
%% mrule([[a], [a, b], [c, d], [c, e], [b, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [c, e], [d, e]]).
%% mrule([[a], [a, b], [c, d], [c, e], [b, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [d, a], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [d, b], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [d, c], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [d, c], [e, a]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [d, c], [e, b]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [d, c], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [d, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [b, e], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [c, a], [c, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [c, a], [d, a]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [c, a], [d, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [c, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [c, b], [c, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [c, b], [d, a]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [c, b], [d, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [c, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [c, d], [d, a]]).
%% mrule([[a], [a, b], [c, d], [b, a], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [c, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, a], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [c, b], [c, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [c, b], [d, a]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [c, b], [d, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [c, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [c, d], [d, c]]).
%% mrule([[a], [a, b], [c, d], [b, c], [b, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [c, e], [d, e]]).
%% mrule([[a], [a, b], [c, d], [b, c], [b, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, b], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [c, d], [d, a]]).
%% mrule([[a], [a, b], [c, d], [b, d], [b, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [c, e], [c, e]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [c, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [c, e], [e, a]]).
%% mrule([[a], [a, b], [c, d], [b, d], [b, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [c, e], [e, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [c, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [d, a], [d, a]]).
%% mrule([[a], [a, b], [c, d], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [d, a], [d, c]]).
%% mrule([[a], [a, b], [c, d], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [d, b], [d, b]]).
%% mrule([[a], [a, b], [c, d], [b, d], [d, c], [d, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [d, e], [e, a]]).
%% mrule([[a], [a, b], [c, d], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [e, a], [e, a]]).
%% mrule([[a], [a, b], [c, d], [b, d], [e, a], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [e, a], [e, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [e, a], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [e, a], [e, d]]).
%% mrule([[a], [a, b], [c, d], [b, d], [e, c], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [e, b], [e, b]]).
%% mrule([[a], [a, b], [c, d], [b, d], [e, c], [e, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [e, b], [e, c]]).
%% mrule([[a], [a, b], [c, d], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [e, c], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, d], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [c, e], [d, a]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [c, e], [d, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [c, e], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [c, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, a], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, a], [e, a]]).
%% mrule([[a], [a, b], [c, d], [b, e], [d, a], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, a], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, a], [e, d]]).
%% mrule([[a], [a, b], [c, d], [b, e], [d, c], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, b], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, b], [e, a]]).
%% mrule([[a], [a, b], [c, d], [b, e], [d, c], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, b], [e, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, b], [e, c]]).
%% mrule([[a], [a, b], [c, d], [b, e], [d, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, b], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, c], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, c], [e, a]]).
%% mrule([[a], [a, b], [c, d], [b, e], [d, b], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, c], [e, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, c], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, e], [e, a]]).
%% mrule([[a], [a, b], [c, d], [b, e], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, f], [e, f]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [d, f], [f, e]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [e, a], [e, d]]).
%% mrule([[a], [a, b], [c, d], [b, e], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [e, f], [f, d]]).
%% mrule([[a, b], [a, c], [b, d], [c, e], [f, d], [f, e]]).
%% mrule([[a, b], [a, c], [b, d], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, a], [e, a], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, a], [e, c], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, b], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, b], [e, c], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [b, d], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [b, d], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [b, d], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [d, c], [e, a], [e, a]]).
%% mrule([[a, b], [a, c], [b, d], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [a, c], [b, d], [d, c], [e, a], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [d, c], [e, b], [e, b]]).
%% mrule([[a, b], [a, c], [b, d], [d, c], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [d, c], [e, c], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, e], [e, a], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, e], [e, c], [e, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, e], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [d, e], [e, f], [f, c]]).
%% mrule([[a, b], [a, c], [b, d], [d, e], [f, c], [f, e]]).
%% mrule([[a, b], [a, c], [b, d], [e, a], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [e, c], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [e, c], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [e, c], [e, f], [f, d]]).
%% mrule([[a, b], [a, c], [b, d], [e, c], [f, d], [f, e]]).
%% mrule([[a, b], [a, c], [b, d], [e, d], [e, f], [f, c]]).
%% mrule([[a, b], [a, c], [b, d], [e, d], [f, c], [f, e]]).
%% mrule([[a, b], [a, c], [b, d], [e, c], [e, f], [d, f]]).
%% mrule([[a, b], [a, c], [b, d], [e, c], [d, f], [f, e]]).
%% mrule([[a, b], [a, c], [c, a], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [a, c], [c, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [c, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [c, a], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [c, a], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [c, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [c, a], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [c, a], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [c, a], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a, c], [c, a], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [c, a], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [c, a], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [c, a], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [c, a], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [c, a], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [c, a], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [c, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [c, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [c, a], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [c, a], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [c, a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [c, a], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [c, a], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [c, a], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [c, a], [d, a], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [c, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [c, a], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [c, a], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [c, a], [d, b], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [c, a], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [c, a], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [c, a], [d, b], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [c, a], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [c, a], [d, b], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [c, a], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [c, a], [d, b], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [c, a], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [c, a], [d, b], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [c, a], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [c, a], [d, b], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [c, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [c, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [c, a], [d, e], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [c, a], [d, e], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [a, c], [c, b], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [c, b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [a, c], [c, b], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [c, b], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [c, b], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [c, b], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [c, b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [c, b], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [c, b], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [c, b], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [c, d], [c, d]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [c, d], [d, c]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [c, e], [d, e]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [c, d], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [d, a], [d, a], [d, a]]).
%% mrule([[a, b], [a, c], [c, b], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [c, b], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [a, c], [c, b], [d, a], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [c, b], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [c, b], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [c, b], [d, a], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [c, b], [d, a], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [c, b], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [c, b], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [c, b], [d, a], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [d, a], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [d, a], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [d, b], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [c, b], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [c, b], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [c, b], [d, b], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [c, b], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [c, b], [d, b], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [c, b], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [c, b], [d, b], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [d, b], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [d, b], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [c, b], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [c, b], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [c, b], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [c, b], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [c, b], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [d, e], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [c, b], [d, e], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [c, b], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [c, b], [d, e], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [d, e], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [c, b], [d, e], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [c, d], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [a, c], [c, d], [c, d], [c, e], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [c, d], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [c, d], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [c, d], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [c, d], [e, a], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [c, d], [e, b], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [c, d], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [c, d], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [c, d], [c, e], [c, e], [d, b]]).
%% mrule([[a, b], [a, c], [c, d], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [c, e], [d, b], [d, e]]).
%% mrule([[a, b], [a, c], [c, d], [c, e], [d, b], [e, a]]).
%% mrule([[a, b], [a, c], [c, d], [c, e], [d, b], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [c, e], [d, b], [e, c]]).
%% mrule([[a, b], [a, c], [c, d], [c, e], [d, b], [e, d]]).
%% mrule([[a, b], [a, c], [c, d], [c, e], [d, c], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [c, e], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [c, e], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [c, d], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [a, c], [c, d], [d, a], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [c, d], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [c, d], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [d, a], [e, a], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [d, a], [e, b], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [c, d], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [c, d], [d, b], [d, b], [d, b]]).
%% mrule([[a, b], [a, c], [c, d], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [c, d], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [c, d], [d, b], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [c, d], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [c, d], [d, b], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [c, d], [d, b], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [c, d], [d, b], [e, a], [e, a]]).
%% mrule([[a, b], [a, c], [c, d], [d, b], [e, a], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [a, c], [c, d], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [c, d], [d, b], [e, b], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [d, b], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [c, d], [d, b], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [c, d], [d, b], [e, c], [e, c]]).
%% mrule([[a, b], [a, c], [c, d], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [c, d], [d, b], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [c, d], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [d, c], [e, b], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [d, c], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [c, d], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [c, d], [d, e], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [d, e], [e, a], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [d, e], [e, b], [e, b]]).
%% mrule([[a, b], [a, c], [c, d], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [c, d], [d, e], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [c, d], [d, e], [e, f], [f, b]]).
%% mrule([[a, b], [a, c], [c, d], [d, e], [f, b], [f, e]]).
%% mrule([[a, b], [a, c], [c, d], [e, a], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [c, d], [e, b], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [c, d], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [c, d], [e, b], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [c, d], [e, b], [e, f], [f, d]]).
%% mrule([[a, b], [a, c], [c, d], [e, b], [f, d], [f, e]]).
%% mrule([[a, b], [a, c], [c, d], [e, d], [e, f], [f, b]]).
%% mrule([[a, b], [a, c], [c, d], [e, d], [f, b], [f, e]]).
%% mrule([[a, b], [a, c], [c, d], [e, b], [e, f], [d, f]]).
%% mrule([[a, b], [a, c], [c, d], [e, b], [d, f], [f, e]]).
%% mrule([[a, b], [a, c], [d, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [d, a], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [d, a], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [d, a], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [d, a], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [d, a], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [a, c], [d, a], [d, b], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [d, a], [d, b], [e, c], [e, c]]).
%% mrule([[a, b], [a, c], [d, a], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [d, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [d, a], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [a, c], [d, a], [d, c], [e, b], [e, b]]).
%% mrule([[a, b], [a, c], [d, a], [d, c], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [d, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [d, a], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [d, a], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, b], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, b], [e, c], [e, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [e, a], [e, a]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [e, a], [e, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [e, b], [e, b]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [e, c], [e, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, e], [e, a], [e, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, e], [e, c], [e, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, e], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [d, e], [e, f], [f, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, e], [f, c], [f, e]]).
%% mrule([[a, b], [a, c], [d, b], [e, a], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [e, c], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [e, c], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [e, c], [e, f], [f, d]]).
%% mrule([[a, b], [a, c], [d, b], [e, c], [f, d], [f, e]]).
%% mrule([[a, b], [a, c], [d, b], [e, d], [e, f], [f, c]]).
%% mrule([[a, b], [a, c], [d, b], [e, d], [f, c], [f, e]]).
%% mrule([[a, b], [a, c], [d, c], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [d, c], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [a, c], [d, c], [d, c], [e, b], [e, b]]).
%% mrule([[a, b], [a, c], [d, c], [d, c], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [d, c], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [d, c], [d, e], [d, e], [e, b]]).
%% mrule([[a, b], [a, c], [d, c], [d, e], [e, a], [e, b]]).
%% mrule([[a, b], [a, c], [d, c], [d, e], [e, b], [e, b]]).
%% mrule([[a, b], [a, c], [d, c], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [d, c], [d, e], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [d, c], [d, e], [e, f], [f, b]]).
%% mrule([[a, b], [a, c], [d, c], [d, e], [f, b], [f, e]]).
%% mrule([[a, b], [a, c], [d, c], [e, a], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [d, c], [e, b], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [d, c], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [d, c], [e, b], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [d, c], [e, b], [e, f], [f, d]]).
%% mrule([[a, b], [a, c], [d, c], [e, b], [f, d], [f, e]]).
%% mrule([[a, b], [a, c], [d, c], [e, d], [e, f], [f, b]]).
%% mrule([[a, b], [a, c], [d, c], [e, d], [f, b], [f, e]]).
%% mrule([[a, b], [a, c], [d, e], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [a, c], [d, e], [d, f], [e, b], [f, c]]).
%% mrule([[a, b], [a, c], [d, e], [d, f], [e, c], [f, b]]).
%% mrule([[a, b], [a, c], [d, e], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [a, c], [d, e], [e, b], [f, c], [f, d]]).
%% mrule([[a, b], [a, c], [d, e], [e, c], [f, b], [f, d]]).
%% mrule([[a, b], [a, c], [a, c], [d, b], [d, e], [c, e]]).
%% mrule([[a, b], [a, c], [a, c], [d, b], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [d, a], [d, e], [c, e]]).
%% mrule([[a, b], [a, c], [b, a], [d, a], [c, e], [e, d]]).
%% mrule([[a], [a, b], [c, a], [d, c], [d, e], [b, e]]).
%% mrule([[a, b], [a, c], [b, a], [d, b], [d, e], [c, e]]).
%% mrule([[a], [a, b], [c, a], [d, c], [b, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [d, b], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [d, c], [d, e], [c, e]]).
%% mrule([[a, b], [a, c], [b, a], [d, c], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, a], [d, e], [d, e], [c, e]]).
%% mrule([[a, b], [a, c], [b, a], [d, e], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [d, e], [c, f], [e, f]]).
%% mrule([[a, b], [a, c], [b, d], [d, e], [c, f], [f, e]]).
%% mrule([[a, b], [a, c], [b, c], [d, a], [d, e], [c, e]]).
%% mrule([[a, b], [a, c], [b, c], [d, a], [c, e], [e, d]]).
%% mrule([[a], [a, b], [c, b], [d, c], [d, e], [b, e]]).
%% mrule([[a, b], [a, c], [b, c], [d, b], [d, e], [c, e]]).
%% mrule([[a], [a, b], [c, b], [d, c], [b, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, b], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, c], [d, e], [c, e]]).
%% mrule([[a, b], [a, c], [b, c], [d, c], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, c], [d, e], [d, e], [c, e]]).
%% mrule([[a, b], [a, c], [b, c], [d, e], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [b, d], [e, d], [e, f], [c, f]]).
%% mrule([[a, b], [a, c], [b, d], [e, d], [c, f], [f, e]]).
%% mrule([[a, b], [a, c], [d, a], [d, a], [c, e], [e, b]]).
%% mrule([[a, b], [a, c], [d, a], [d, b], [d, e], [c, e]]).
%% mrule([[a, b], [a, c], [d, a], [d, b], [c, e], [c, e]]).
%% mrule([[a, b], [a, c], [d, a], [d, b], [c, e], [e, a]]).
%% mrule([[a, b], [a, c], [d, a], [d, b], [c, e], [e, b]]).
%% mrule([[a, b], [a, c], [d, a], [d, b], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [d, a], [d, b], [c, e], [e, c]]).
%% mrule([[a, b], [a, c], [d, a], [d, c], [c, e], [e, b]]).
%% mrule([[a, b], [a, c], [d, a], [d, e], [c, b], [c, e]]).
%% mrule([[a, b], [a, c], [d, a], [d, e], [c, e], [e, b]]).
%% mrule([[a, b], [a, c], [d, a], [c, b], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [d, a], [c, e], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [d, b], [d, e], [c, e]]).
%% mrule([[a, b], [a, c], [d, b], [d, b], [c, e], [c, e]]).
%% mrule([[a, b], [a, c], [d, b], [d, b], [c, e], [e, a]]).
%% mrule([[a, b], [a, c], [d, b], [d, b], [c, e], [e, b]]).
%% mrule([[a, b], [a, c], [d, b], [d, b], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [d, b], [c, e], [e, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [d, e], [c, e]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [c, e], [c, e]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [c, e], [e, a]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [c, e], [e, b]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [d, c], [c, e], [e, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, e], [d, e], [c, e]]).
%% mrule([[a, b], [a, c], [d, b], [d, e], [c, a], [c, e]]).
%% mrule([[a, b], [a, c], [d, b], [d, e], [c, b], [c, e]]).
%% mrule([[a, b], [a, c], [d, b], [d, e], [c, e], [c, e]]).
%% mrule([[a, b], [a, c], [d, b], [d, e], [c, e], [e, a]]).
%% mrule([[a, b], [a, c], [d, b], [d, e], [c, e], [e, b]]).
%% mrule([[a, b], [a, c], [d, b], [d, e], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [d, e], [c, e], [e, c]]).
%% mrule([[a, b], [a, c], [d, b], [d, e], [c, f], [e, f]]).
%% mrule([[a, b], [a, c], [d, b], [d, e], [c, f], [f, e]]).
%% mrule([[a, b], [a, c], [d, b], [c, a], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [c, b], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [c, e], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [c, e], [e, a], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [c, e], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [c, e], [e, d], [e, d]]).
%% mrule([[a, b], [a, c], [d, b], [c, e], [e, d], [e, c]]).
%% mrule([[a, b], [a, c], [d, b], [c, e], [e, f], [f, d]]).
%% mrule([[a, b], [a, c], [d, b], [c, e], [f, d], [f, e]]).
%% mrule([[a, b], [a, c], [d, c], [d, c], [c, e], [e, b]]).
%% mrule([[a, b], [a, c], [d, c], [d, e], [c, b], [c, e]]).
%% mrule([[a, b], [a, c], [d, c], [d, e], [c, e], [e, b]]).
%% mrule([[a, b], [a, c], [d, c], [c, b], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [d, c], [c, e], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [d, e], [d, e], [c, b], [c, e]]).
%% mrule([[a, b], [a, c], [d, e], [d, e], [c, e], [e, b]]).
%% mrule([[a, b], [a, c], [d, e], [d, f], [c, e], [f, b]]).
%% mrule([[a, b], [a, c], [d, e], [d, f], [c, f], [e, b]]).
%% mrule([[a, b], [a, c], [d, e], [c, b], [c, e], [e, d]]).
%% mrule([[a, b], [a, c], [d, e], [c, e], [e, b], [e, d]]).
%% mrule([[a, b], [a, c], [d, e], [c, e], [f, b], [f, d]]).
%% mrule([[a, b], [a, c], [d, e], [c, f], [e, b], [f, d]]).
%% mrule([[a, b], [a, c], [d, b], [e, d], [e, f], [c, f]]).
%% mrule([[a, b], [a, c], [d, b], [e, d], [c, f], [f, e]]).
%% mrule([[a], [b, a], [b, a], [b, a], [b, a], [b, a]]).
%% mrule([[a, b], [b, a], [b, a], [b, a], [b, a], [b, a]]).
%% mrule([[a], [b, a], [b, a], [b, a], [b, c], [b, c]]).
%% mrule([[a, b], [b, a], [b, a], [b, a], [b, c], [b, c]]).
%% mrule([[a], [b, a], [b, a], [b, a], [b, c], [c, a]]).
%% mrule([[a, b], [b, a], [b, a], [b, a], [b, c], [c, a]]).
%% mrule([[a], [b, a], [b, a], [b, a], [b, c], [c, b]]).
%% mrule([[a, b], [b, a], [b, a], [b, a], [b, c], [c, b]]).
%% mrule([[a], [b, a], [b, a], [b, a], [c, a], [c, a]]).
%% mrule([[a, b], [b, a], [b, a], [b, a], [c, a], [c, a]]).
%% mrule([[a], [b, a], [b, a], [b, a], [c, a], [c, b]]).
%% mrule([[a, b], [b, a], [b, a], [b, a], [c, a], [c, b]]).
%% mrule([[a], [b, a], [b, a], [b, a], [c, b], [c, b]]).
%% mrule([[a, b], [b, a], [b, a], [b, a], [c, b], [c, b]]).
%% mrule([[a], [b, a], [b, a], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [b, a], [b, a], [b, c], [b, c], [b, c]]).
%% mrule([[a], [b, a], [b, a], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [b, a], [b, a], [b, c], [b, c], [c, a]]).
%% mrule([[a], [b, a], [b, a], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [b, a], [b, a], [b, c], [b, c], [c, b]]).
%% mrule([[a], [b, a], [b, a], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [b, a], [b, a], [b, c], [b, d], [c, d]]).
%% mrule([[a], [b, a], [b, a], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [b, a], [b, a], [b, c], [b, d], [d, c]]).
%% mrule([[a], [b, a], [b, a], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [b, a], [b, a], [b, c], [c, a], [c, a]]).
%% mrule([[a], [b, a], [b, a], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [b, a], [b, a], [b, c], [c, a], [c, b]]).
%% mrule([[a], [b, a], [b, a], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [b, a], [b, a], [b, c], [c, b], [c, b]]).
%% mrule([[a], [b, a], [b, a], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [b, a], [b, a], [b, c], [c, d], [c, d]]).
%% mrule([[a], [b, a], [b, a], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [b, a], [b, a], [b, c], [c, d], [d, a]]).
%% mrule([[a], [b, a], [b, a], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [b, a], [b, c], [c, d], [d, b]]).
%% mrule([[a], [b, a], [b, a], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [b, a], [b, a], [b, c], [c, d], [d, c]]).
%% mrule([[a], [b, a], [b, a], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [b, a], [b, a], [b, c], [d, a], [d, c]]).
%% mrule([[a], [b, a], [b, a], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [b, a], [b, c], [d, b], [d, c]]).
%% mrule([[a], [b, a], [b, a], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [b, a], [b, c], [d, c], [d, c]]).
%% mrule([[a], [b, a], [b, a], [c, a], [c, a], [c, a]]).
%% mrule([[a, b], [b, a], [b, a], [c, a], [c, a], [c, a]]).
%% mrule([[a], [b, a], [b, a], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [b, a], [b, a], [c, a], [c, a], [c, b]]).
%% mrule([[a], [b, a], [b, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b, a], [b, a], [c, a], [c, b], [c, b]]).
%% mrule([[a], [b, a], [b, a], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [b, a], [b, a], [c, a], [c, d], [c, d]]).
%% mrule([[a], [b, a], [b, a], [c, a], [c, d], [d, a]]).
%% mrule([[a, b], [b, a], [b, a], [c, a], [c, d], [d, a]]).
%% mrule([[a], [b, a], [b, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [b, a], [c, a], [c, d], [d, b]]).
%% mrule([[a], [b, a], [b, a], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [b, a], [b, a], [c, a], [c, d], [d, c]]).
%% mrule([[a], [b, a], [b, a], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [b, a], [b, a], [c, a], [d, a], [d, c]]).
%% mrule([[a], [b, a], [b, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [b, a], [c, a], [d, b], [d, c]]).
%% mrule([[a], [b, a], [b, a], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [b, a], [c, a], [d, c], [d, c]]).
%% mrule([[a], [b, a], [b, a], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [b, a], [b, a], [c, b], [c, b], [c, b]]).
%% mrule([[a], [b, a], [b, a], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [b, a], [b, a], [c, b], [c, d], [c, d]]).
%% mrule([[a], [b, a], [b, a], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b, a], [b, a], [c, b], [c, d], [d, a]]).
%% mrule([[a], [b, a], [b, a], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [b, a], [c, b], [c, d], [d, b]]).
%% mrule([[a], [b, a], [b, a], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [b, a], [b, a], [c, b], [c, d], [d, c]]).
%% mrule([[a], [b, a], [b, a], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b, a], [b, a], [c, b], [d, a], [d, c]]).
%% mrule([[a], [b, a], [b, a], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [b, a], [c, b], [d, b], [d, c]]).
%% mrule([[a], [b, a], [b, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [b, a], [c, b], [d, c], [d, c]]).
%% mrule([[a], [b, a], [b, a], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [b, a], [b, a], [c, d], [c, d], [d, a]]).
%% mrule([[a], [b, a], [b, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [b, a], [c, d], [c, d], [d, b]]).
%% mrule([[a], [b, a], [b, a], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [b, a], [b, a], [c, d], [d, a], [d, c]]).
%% mrule([[a], [b, a], [b, a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [b, a], [c, d], [d, b], [d, c]]).
%% mrule([[a], [b, a], [b, c], [b, c], [b, c], [b, c]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [b, c], [b, c]]).
%% mrule([[a], [b, a], [b, c], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [b, c], [c, a]]).
%% mrule([[a], [b, a], [b, c], [b, c], [b, c], [c, b]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [b, c], [c, b]]).
%% mrule([[a], [b, a], [b, c], [b, c], [b, d], [b, d]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [b, d], [b, d]]).
%% mrule([[a], [b, a], [b, c], [b, c], [b, d], [c, d]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [b, d], [c, d]]).
%% mrule([[a], [b, a], [b, c], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [b, d], [d, a]]).
%% mrule([[a], [b, a], [b, c], [b, c], [b, d], [d, b]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [b, d], [d, b]]).
%% mrule([[a], [b, a], [b, c], [b, c], [b, d], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [b, d], [d, c]]).
%% mrule([[a], [b, a], [b, c], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [c, a], [c, a]]).
%% mrule([[a], [b, a], [b, c], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [c, a], [c, b]]).
%% mrule([[a], [b, a], [b, c], [b, c], [c, b], [c, b]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [c, b], [c, b]]).
%% mrule([[a], [b, a], [b, c], [b, c], [c, d], [c, d]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [c, d], [c, d]]).
%% mrule([[a], [b, a], [b, c], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [c, d], [d, a]]).
%% mrule([[a], [b, a], [b, c], [b, c], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [c, d], [d, b]]).
%% mrule([[a], [b, a], [b, c], [b, c], [c, d], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [c, d], [d, c]]).
%% mrule([[a], [b, a], [b, c], [b, c], [d, a], [d, a]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [d, a], [d, a]]).
%% mrule([[a], [b, a], [b, c], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [d, a], [d, b]]).
%% mrule([[a], [b, a], [b, c], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [d, a], [d, c]]).
%% mrule([[a], [b, a], [b, c], [b, c], [d, b], [d, b]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [d, b], [d, b]]).
%% mrule([[a], [b, a], [b, c], [b, c], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [d, b], [d, c]]).
%% mrule([[a], [b, a], [b, c], [b, c], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [b, c], [d, c], [d, c]]).
%% mrule([[a], [b, a], [b, c], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [b, d], [c, a]]).
%% mrule([[a], [b, a], [b, c], [b, d], [b, d], [c, b]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [b, d], [c, b]]).
%% mrule([[a], [b, a], [b, c], [b, d], [b, d], [c, d]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [b, d], [c, d]]).
%% mrule([[a], [b, a], [b, c], [b, d], [b, d], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [b, d], [d, c]]).
%% mrule([[a], [b, a], [b, c], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [c, a], [c, d]]).
%% mrule([[a], [b, a], [b, c], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [c, a], [d, a]]).
%% mrule([[a], [b, a], [b, c], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [c, a], [d, b]]).
%% mrule([[a], [b, a], [b, c], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [c, a], [d, c]]).
%% mrule([[a], [b, a], [b, c], [b, d], [c, b], [c, d]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [c, b], [c, d]]).
%% mrule([[a], [b, a], [b, c], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [c, b], [d, a]]).
%% mrule([[a], [b, a], [b, c], [b, d], [c, b], [d, b]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [c, b], [d, b]]).
%% mrule([[a], [b, a], [b, c], [b, d], [c, b], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [c, b], [d, c]]).
%% mrule([[a], [b, a], [b, c], [b, d], [c, d], [c, d]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [c, d], [c, d]]).
%% mrule([[a], [b, a], [b, c], [b, d], [c, d], [d, a]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [c, d], [d, a]]).
%% mrule([[a], [b, a], [b, c], [b, d], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [c, d], [d, b]]).
%% mrule([[a], [b, a], [b, c], [b, d], [c, d], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [c, d], [d, c]]).
%% mrule([[a], [b, a], [b, c], [b, d], [c, e], [d, e]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [c, e], [d, e]]).
%% mrule([[a], [b, a], [b, c], [b, d], [c, e], [e, d]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [c, e], [e, d]]).
%% mrule([[a], [b, a], [b, c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [d, a], [d, c]]).
%% mrule([[a], [b, a], [b, c], [b, d], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [d, b], [d, c]]).
%% mrule([[a], [b, a], [b, c], [b, d], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [d, c], [d, c]]).
%% mrule([[a], [b, a], [b, c], [b, d], [d, e], [e, c]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [d, e], [e, c]]).
%% mrule([[a], [b, a], [b, c], [b, d], [e, c], [e, d]]).
%% mrule([[a, b], [b, a], [b, c], [b, d], [e, c], [e, d]]).
%% mrule([[a], [b, a], [b, c], [c, a], [c, a], [c, a]]).
%% mrule([[a, b], [b, a], [b, c], [c, a], [c, a], [c, a]]).
%% mrule([[a], [b, a], [b, c], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [b, a], [b, c], [c, a], [c, a], [c, b]]).
%% mrule([[a], [b, a], [b, c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b, a], [b, c], [c, a], [c, b], [c, b]]).
%% mrule([[a], [b, a], [b, c], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [b, a], [b, c], [c, a], [c, d], [c, d]]).
%% mrule([[a], [b, a], [b, c], [c, a], [c, d], [d, a]]).
%% mrule([[a, b], [b, a], [b, c], [c, a], [c, d], [d, a]]).
%% mrule([[a], [b, a], [b, c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [b, c], [c, a], [c, d], [d, b]]).
%% mrule([[a], [b, a], [b, c], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [c, a], [c, d], [d, c]]).
%% mrule([[a], [b, a], [b, c], [c, a], [d, a], [d, a]]).
%% mrule([[a, b], [b, a], [b, c], [c, a], [d, a], [d, a]]).
%% mrule([[a], [b, a], [b, c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [b, a], [b, c], [c, a], [d, a], [d, b]]).
%% mrule([[a], [b, a], [b, c], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [c, a], [d, a], [d, c]]).
%% mrule([[a], [b, a], [b, c], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [b, a], [b, c], [c, a], [d, b], [d, b]]).
%% mrule([[a], [b, a], [b, c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [c, a], [d, b], [d, c]]).
%% mrule([[a], [b, a], [b, c], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [c, a], [d, c], [d, c]]).
%% mrule([[a], [b, a], [b, c], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [b, a], [b, c], [c, b], [c, b], [c, b]]).
%% mrule([[a], [b, a], [b, c], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [b, a], [b, c], [c, b], [c, d], [c, d]]).
%% mrule([[a], [b, a], [b, c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b, a], [b, c], [c, b], [c, d], [d, a]]).
%% mrule([[a], [b, a], [b, c], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [b, c], [c, b], [c, d], [d, b]]).
%% mrule([[a], [b, a], [b, c], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [c, b], [c, d], [d, c]]).
%% mrule([[a], [b, a], [b, c], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [b, a], [b, c], [c, b], [d, a], [d, a]]).
%% mrule([[a], [b, a], [b, c], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [b, a], [b, c], [c, b], [d, a], [d, b]]).
%% mrule([[a], [b, a], [b, c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [c, b], [d, a], [d, c]]).
%% mrule([[a], [b, a], [b, c], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [b, a], [b, c], [c, b], [d, b], [d, b]]).
%% mrule([[a], [b, a], [b, c], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [c, b], [d, b], [d, c]]).
%% mrule([[a], [b, a], [b, c], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [c, b], [d, c], [d, c]]).
%% mrule([[a], [b, a], [b, c], [c, d], [c, d], [c, d]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [c, d], [c, d]]).
%% mrule([[a], [b, a], [b, c], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [c, d], [d, a]]).
%% mrule([[a], [b, a], [b, c], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [c, d], [d, b]]).
%% mrule([[a], [b, a], [b, c], [c, d], [c, d], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [c, d], [d, c]]).
%% mrule([[a], [b, a], [b, c], [c, d], [c, e], [d, e]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [c, e], [d, e]]).
%% mrule([[a], [b, a], [b, c], [c, d], [c, e], [e, d]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [c, e], [e, d]]).
%% mrule([[a], [b, a], [b, c], [c, d], [d, a], [d, a]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [d, a], [d, a]]).
%% mrule([[a], [b, a], [b, c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [d, a], [d, b]]).
%% mrule([[a], [b, a], [b, c], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [d, a], [d, c]]).
%% mrule([[a], [b, a], [b, c], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [d, b], [d, b]]).
%% mrule([[a], [b, a], [b, c], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [d, b], [d, c]]).
%% mrule([[a], [b, a], [b, c], [c, d], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [d, c], [d, c]]).
%% mrule([[a], [b, a], [b, c], [c, d], [d, e], [d, e]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [d, e], [d, e]]).
%% mrule([[a], [b, a], [b, c], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [d, e], [e, a]]).
%% mrule([[a], [b, a], [b, c], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [d, e], [e, b]]).
%% mrule([[a], [b, a], [b, c], [c, d], [d, e], [e, c]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [d, e], [e, c]]).
%% mrule([[a], [b, a], [b, c], [c, d], [d, e], [e, d]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [d, e], [e, d]]).
%% mrule([[a], [b, a], [b, c], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [e, a], [e, d]]).
%% mrule([[a], [b, a], [b, c], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [e, b], [e, d]]).
%% mrule([[a], [b, a], [b, c], [c, d], [e, c], [e, d]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [e, c], [e, d]]).
%% mrule([[a], [b, a], [b, c], [c, d], [e, d], [e, d]]).
%% mrule([[a, b], [b, a], [b, c], [c, d], [e, d], [e, d]]).
%% mrule([[a], [b, a], [b, c], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [d, a], [d, a], [d, c]]).
%% mrule([[a], [b, a], [b, c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [d, a], [d, b], [d, c]]).
%% mrule([[a], [b, a], [b, c], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [d, a], [d, c], [d, c]]).
%% mrule([[a], [b, a], [b, c], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [b, a], [b, c], [d, a], [d, e], [e, c]]).
%% mrule([[a], [b, a], [b, c], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [b, a], [b, c], [d, a], [e, c], [e, d]]).
%% mrule([[a], [b, a], [b, c], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [d, b], [d, b], [d, c]]).
%% mrule([[a], [b, a], [b, c], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [d, b], [d, c], [d, c]]).
%% mrule([[a], [b, a], [b, c], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [b, a], [b, c], [d, b], [d, e], [e, c]]).
%% mrule([[a], [b, a], [b, c], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [b, a], [b, c], [d, b], [e, c], [e, d]]).
%% mrule([[a], [b, a], [b, c], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [b, c], [d, c], [d, c], [d, c]]).
%% mrule([[a], [b, a], [b, c], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [b, a], [b, c], [d, c], [d, e], [d, e]]).
%% mrule([[a], [b, a], [b, c], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [b, a], [b, c], [d, c], [d, e], [e, a]]).
%% mrule([[a], [b, a], [b, c], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [b, a], [b, c], [d, c], [d, e], [e, b]]).
%% mrule([[a], [b, a], [b, c], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [b, a], [b, c], [d, c], [d, e], [e, c]]).
%% mrule([[a], [b, a], [b, c], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [b, a], [b, c], [d, c], [d, e], [e, d]]).
%% mrule([[a], [b, a], [b, c], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [b, a], [b, c], [d, c], [e, a], [e, d]]).
%% mrule([[a], [b, a], [b, c], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [b, a], [b, c], [d, c], [e, b], [e, d]]).
%% mrule([[a], [b, a], [b, c], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [b, a], [b, c], [d, c], [e, c], [e, d]]).
%% mrule([[a], [b, a], [b, c], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [b, a], [b, c], [d, c], [e, d], [e, d]]).
%% mrule([[a], [b, a], [b, c], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [b, a], [b, c], [d, e], [d, e], [e, c]]).
%% mrule([[a], [b, a], [b, c], [d, e], [e, c], [e, d]]).
%% mrule([[a, b], [b, a], [b, c], [d, e], [e, c], [e, d]]).
%% mrule([[a], [b, a], [b, c], [d, a], [d, e], [c, e]]).
%% mrule([[a, b], [b, a], [b, c], [d, a], [d, e], [c, e]]).
%% mrule([[a], [b, a], [b, c], [d, a], [c, e], [e, d]]).
%% mrule([[a, b], [b, a], [b, c], [d, a], [c, e], [e, d]]).
%% mrule([[a], [b, a], [b, c], [d, b], [d, e], [c, e]]).
%% mrule([[a, b], [b, a], [b, c], [d, b], [d, e], [c, e]]).
%% mrule([[a], [b, a], [b, c], [d, b], [c, e], [e, d]]).
%% mrule([[a, b], [b, a], [b, c], [d, b], [c, e], [e, d]]).
%% mrule([[a], [b, a], [b, c], [d, c], [d, e], [c, e]]).
%% mrule([[a, b], [b, a], [b, c], [d, c], [d, e], [c, e]]).
%% mrule([[a], [b, a], [b, c], [d, c], [c, e], [e, d]]).
%% mrule([[a, b], [b, a], [b, c], [d, c], [c, e], [e, d]]).
%% mrule([[a], [b, a], [b, c], [d, e], [d, e], [c, e]]).
%% mrule([[a, b], [b, a], [b, c], [d, e], [d, e], [c, e]]).
%% mrule([[a], [b, a], [b, c], [d, e], [c, e], [e, d]]).
%% mrule([[a, b], [b, a], [b, c], [d, e], [c, e], [e, d]]).
%% mrule([[a, b], [b, a], [c, a], [c, a], [c, a], [c, a]]).
%% mrule([[a], [b, a], [c, a], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, a], [c, a], [c, b]]).
%% mrule([[a], [b, a], [c, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [b, a], [c, a], [c, a], [c, d], [d, a]]).
%% mrule([[a], [b, a], [c, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [c, a], [d, a], [d, a]]).
%% mrule([[a], [b, a], [c, a], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, a], [d, a], [d, c]]).
%% mrule([[a], [b, a], [c, a], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, a], [d, b], [d, b]]).
%% mrule([[a], [b, a], [c, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [c, a], [d, c], [d, c]]).
%% mrule([[a], [b, a], [c, a], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, b], [c, b], [c, b]]).
%% mrule([[a], [b, a], [c, a], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [b, a], [c, a], [c, b], [c, d], [c, d]]).
%% mrule([[a], [b, a], [c, a], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b, a], [c, a], [c, b], [c, d], [d, a]]).
%% mrule([[a], [b, a], [c, a], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, b], [c, d], [d, b]]).
%% mrule([[a], [b, a], [c, a], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [c, b], [c, d], [d, c]]).
%% mrule([[a], [b, a], [c, a], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [b, a], [c, a], [c, b], [d, a], [d, a]]).
%% mrule([[a], [b, a], [c, a], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, b], [d, a], [d, b]]).
%% mrule([[a], [b, a], [c, a], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [c, b], [d, a], [d, c]]).
%% mrule([[a], [b, a], [c, a], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, b], [d, b], [d, b]]).
%% mrule([[a], [b, a], [c, a], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [c, b], [d, b], [d, c]]).
%% mrule([[a], [b, a], [c, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [c, d], [c, d]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [c, d], [d, a]]).
%% mrule([[a], [b, a], [c, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [c, d], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [c, e], [d, e]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [c, e], [e, d]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [d, a], [d, a]]).
%% mrule([[a], [b, a], [c, a], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [d, a], [d, c]]).
%% mrule([[a], [b, a], [c, a], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [d, b], [d, b]]).
%% mrule([[a], [b, a], [c, a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [d, e], [d, e]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [d, e], [e, a]]).
%% mrule([[a], [b, a], [c, a], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [d, e], [e, c]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [d, e], [e, d]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [e, a], [e, d]]).
%% mrule([[a], [b, a], [c, a], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [e, c], [e, d]]).
%% mrule([[a, b], [b, a], [c, a], [c, d], [e, d], [e, d]]).
%% mrule([[a, b], [b, a], [c, a], [d, a], [d, a], [d, c]]).
%% mrule([[a], [b, a], [c, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [b, a], [c, a], [d, a], [e, c], [e, d]]).
%% mrule([[a], [b, a], [c, a], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [d, b], [d, b], [d, c]]).
%% mrule([[a], [b, a], [c, a], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [d, b], [d, c], [d, c]]).
%% mrule([[a], [b, a], [c, a], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [b, a], [c, a], [d, b], [d, e], [e, c]]).
%% mrule([[a], [b, a], [c, a], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [b, a], [c, a], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [b, a], [c, a], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [c, a], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [b, a], [c, a], [d, c], [d, e], [e, a]]).
%% mrule([[a], [b, a], [c, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [b, a], [c, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [b, a], [c, a], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [b, a], [c, a], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [b, a], [c, a], [d, c], [e, a], [e, d]]).
%% mrule([[a], [b, a], [c, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [b, a], [c, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [b, a], [c, a], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [b, a], [c, a], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [b, a], [c, a], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [b, a], [c, a], [d, e], [e, c], [e, d]]).
%% mrule([[a], [b, a], [c, b], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [b, a], [c, b], [c, b], [c, b], [c, b]]).
%% mrule([[a], [b, a], [c, b], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [b, a], [c, b], [c, b], [c, d], [c, d]]).
%% mrule([[a], [b, a], [c, b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b, a], [c, b], [c, b], [c, d], [d, a]]).
%% mrule([[a], [b, a], [c, b], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [c, b], [c, b], [c, d], [d, b]]).
%% mrule([[a], [b, a], [c, b], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [b, a], [c, b], [c, b], [c, d], [d, c]]).
%% mrule([[a], [b, a], [c, b], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [b, a], [c, b], [c, b], [d, a], [d, a]]).
%% mrule([[a], [b, a], [c, b], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [b, a], [c, b], [c, b], [d, a], [d, b]]).
%% mrule([[a], [b, a], [c, b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b, a], [c, b], [c, b], [d, a], [d, c]]).
%% mrule([[a], [b, a], [c, b], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [b, a], [c, b], [c, b], [d, b], [d, b]]).
%% mrule([[a], [b, a], [c, b], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, b], [c, b], [d, b], [d, c]]).
%% mrule([[a], [b, a], [c, b], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [c, b], [c, b], [d, c], [d, c]]).
%% mrule([[a], [b, a], [c, b], [c, d], [c, d], [c, d]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [c, d], [c, d]]).
%% mrule([[a], [b, a], [c, b], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [c, d], [d, a]]).
%% mrule([[a], [b, a], [c, b], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [c, d], [d, b]]).
%% mrule([[a], [b, a], [c, b], [c, d], [c, d], [d, c]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [c, d], [d, c]]).
%% mrule([[a], [b, a], [c, b], [c, d], [c, e], [d, e]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [c, e], [d, e]]).
%% mrule([[a], [b, a], [c, b], [c, d], [c, e], [e, d]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [c, e], [e, d]]).
%% mrule([[a], [b, a], [c, b], [c, d], [d, a], [d, a]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [d, a], [d, a]]).
%% mrule([[a], [b, a], [c, b], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [d, a], [d, b]]).
%% mrule([[a], [b, a], [c, b], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [d, a], [d, c]]).
%% mrule([[a], [b, a], [c, b], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [d, b], [d, b]]).
%% mrule([[a], [b, a], [c, b], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [d, b], [d, c]]).
%% mrule([[a], [b, a], [c, b], [c, d], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [d, c], [d, c]]).
%% mrule([[a], [b, a], [c, b], [c, d], [d, e], [d, e]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [d, e], [d, e]]).
%% mrule([[a], [b, a], [c, b], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [d, e], [e, a]]).
%% mrule([[a], [b, a], [c, b], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [d, e], [e, b]]).
%% mrule([[a], [b, a], [c, b], [c, d], [d, e], [e, c]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [d, e], [e, c]]).
%% mrule([[a], [b, a], [c, b], [c, d], [d, e], [e, d]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [d, e], [e, d]]).
%% mrule([[a], [b, a], [c, b], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [e, a], [e, d]]).
%% mrule([[a], [b, a], [c, b], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [e, b], [e, d]]).
%% mrule([[a], [b, a], [c, b], [c, d], [e, c], [e, d]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [e, c], [e, d]]).
%% mrule([[a], [b, a], [c, b], [c, d], [e, d], [e, d]]).
%% mrule([[a, b], [b, a], [c, b], [c, d], [e, d], [e, d]]).
%% mrule([[a], [b, a], [c, b], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [b, a], [c, b], [d, a], [d, a], [d, c]]).
%% mrule([[a], [b, a], [c, b], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, b], [d, a], [d, b], [d, c]]).
%% mrule([[a], [b, a], [c, b], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [c, b], [d, a], [d, c], [d, c]]).
%% mrule([[a], [b, a], [c, b], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [b, a], [c, b], [d, a], [d, e], [e, c]]).
%% mrule([[a], [b, a], [c, b], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [b, a], [c, b], [d, a], [e, c], [e, d]]).
%% mrule([[a], [b, a], [c, b], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, b], [d, b], [d, b], [d, c]]).
%% mrule([[a], [b, a], [c, b], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [c, b], [d, b], [d, c], [d, c]]).
%% mrule([[a], [b, a], [c, b], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [b, a], [c, b], [d, b], [d, e], [e, c]]).
%% mrule([[a], [b, a], [c, b], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [b, a], [c, b], [d, b], [e, c], [e, d]]).
%% mrule([[a], [b, a], [c, b], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [c, b], [d, c], [d, c], [d, c]]).
%% mrule([[a], [b, a], [c, b], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [b, a], [c, b], [d, c], [d, e], [d, e]]).
%% mrule([[a], [b, a], [c, b], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [b, a], [c, b], [d, c], [d, e], [e, a]]).
%% mrule([[a], [b, a], [c, b], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [b, a], [c, b], [d, c], [d, e], [e, b]]).
%% mrule([[a], [b, a], [c, b], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [b, a], [c, b], [d, c], [d, e], [e, c]]).
%% mrule([[a], [b, a], [c, b], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [b, a], [c, b], [d, c], [d, e], [e, d]]).
%% mrule([[a], [b, a], [c, b], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [b, a], [c, b], [d, c], [e, a], [e, d]]).
%% mrule([[a], [b, a], [c, b], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [b, a], [c, b], [d, c], [e, b], [e, d]]).
%% mrule([[a], [b, a], [c, b], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [b, a], [c, b], [d, c], [e, c], [e, d]]).
%% mrule([[a], [b, a], [c, b], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [b, a], [c, b], [d, c], [e, d], [e, d]]).
%% mrule([[a], [b, a], [c, b], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [b, a], [c, b], [d, e], [d, e], [e, c]]).
%% mrule([[a], [b, a], [c, b], [d, e], [e, c], [e, d]]).
%% mrule([[a, b], [b, a], [c, b], [d, e], [e, c], [e, d]]).
%% mrule([[a, b], [b, a], [c, d], [c, d], [c, d], [d, a]]).
%% mrule([[a], [b, a], [c, d], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [c, d], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [b, a], [c, d], [c, d], [c, e], [e, a]]).
%% mrule([[a], [b, a], [c, d], [c, d], [c, e], [e, b]]).
%% mrule([[a, b], [b, a], [c, d], [c, d], [c, e], [e, b]]).
%% mrule([[a, b], [b, a], [c, d], [c, d], [d, a], [d, a]]).
%% mrule([[a], [b, a], [c, d], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b, a], [c, d], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b, a], [c, d], [c, d], [d, a], [d, c]]).
%% mrule([[a], [b, a], [c, d], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [b, a], [c, d], [c, d], [d, b], [d, b]]).
%% mrule([[a], [b, a], [c, d], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, d], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, d], [c, d], [d, e], [e, a]]).
%% mrule([[a], [b, a], [c, d], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [b, a], [c, d], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [b, a], [c, d], [c, d], [e, a], [e, c]]).
%% mrule([[a, b], [b, a], [c, d], [c, d], [e, a], [e, d]]).
%% mrule([[a], [b, a], [c, d], [c, d], [e, b], [e, c]]).
%% mrule([[a, b], [b, a], [c, d], [c, d], [e, b], [e, c]]).
%% mrule([[a], [b, a], [c, d], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [b, a], [c, d], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [b, a], [c, d], [c, e], [c, e], [d, a]]).
%% mrule([[a], [b, a], [c, d], [c, e], [c, e], [d, b]]).
%% mrule([[a, b], [b, a], [c, d], [c, e], [c, e], [d, b]]).
%% mrule([[a, b], [b, a], [c, d], [c, e], [d, a], [d, e]]).
%% mrule([[a, b], [b, a], [c, d], [c, e], [d, a], [e, a]]).
%% mrule([[a], [b, a], [c, d], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [b, a], [c, d], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [b, a], [c, d], [c, e], [d, a], [e, c]]).
%% mrule([[a, b], [b, a], [c, d], [c, e], [d, a], [e, d]]).
%% mrule([[a], [b, a], [c, d], [c, e], [d, b], [d, e]]).
%% mrule([[a, b], [b, a], [c, d], [c, e], [d, b], [d, e]]).
%% mrule([[a], [b, a], [c, d], [c, e], [d, b], [e, a]]).
%% mrule([[a, b], [b, a], [c, d], [c, e], [d, b], [e, a]]).
%% mrule([[a], [b, a], [c, d], [c, e], [d, b], [e, b]]).
%% mrule([[a, b], [b, a], [c, d], [c, e], [d, b], [e, b]]).
%% mrule([[a], [b, a], [c, d], [c, e], [d, b], [e, c]]).
%% mrule([[a, b], [b, a], [c, d], [c, e], [d, b], [e, c]]).
%% mrule([[a], [b, a], [c, d], [c, e], [d, b], [e, d]]).
%% mrule([[a, b], [b, a], [c, d], [c, e], [d, b], [e, d]]).
%% mrule([[a, b], [b, a], [c, d], [c, e], [d, c], [e, a]]).
%% mrule([[a], [b, a], [c, d], [c, e], [d, c], [e, b]]).
%% mrule([[a, b], [b, a], [c, d], [c, e], [d, c], [e, b]]).
%% mrule([[a, b], [b, a], [c, d], [c, e], [d, e], [e, a]]).
%% mrule([[a], [b, a], [c, d], [c, e], [d, e], [e, b]]).
%% mrule([[a, b], [b, a], [c, d], [c, e], [d, e], [e, b]]).
%% mrule([[a, b], [b, a], [c, d], [c, e], [e, a], [e, d]]).
%% mrule([[a], [b, a], [c, d], [c, e], [e, b], [e, d]]).
%% mrule([[a, b], [b, a], [c, d], [c, e], [e, b], [e, d]]).
%% mrule([[a, b], [b, a], [c, d], [d, a], [d, a], [d, c]]).
%% mrule([[a], [b, a], [c, d], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, d], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, d], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [c, d], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [b, a], [c, d], [d, a], [e, a], [e, c]]).
%% mrule([[a], [b, a], [c, d], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [b, a], [c, d], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [b, a], [c, d], [d, a], [e, c], [e, c]]).
%% mrule([[a, b], [b, a], [c, d], [d, a], [e, c], [e, d]]).
%% mrule([[a], [b, a], [c, d], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [b, a], [c, d], [d, b], [d, b], [d, c]]).
%% mrule([[a], [b, a], [c, d], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [b, a], [c, d], [d, b], [d, c], [d, c]]).
%% mrule([[a], [b, a], [c, d], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [b, a], [c, d], [d, b], [d, e], [e, c]]).
%% mrule([[a], [b, a], [c, d], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [b, a], [c, d], [d, b], [e, a], [e, c]]).
%% mrule([[a], [b, a], [c, d], [d, b], [e, b], [e, c]]).
%% mrule([[a, b], [b, a], [c, d], [d, b], [e, b], [e, c]]).
%% mrule([[a], [b, a], [c, d], [d, b], [e, c], [e, c]]).
%% mrule([[a, b], [b, a], [c, d], [d, b], [e, c], [e, c]]).
%% mrule([[a], [b, a], [c, d], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [b, a], [c, d], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [b, a], [c, d], [d, c], [d, e], [e, a]]).
%% mrule([[a], [b, a], [c, d], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [b, a], [c, d], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [b, a], [c, d], [d, c], [e, a], [e, c]]).
%% mrule([[a, b], [b, a], [c, d], [d, c], [e, a], [e, d]]).
%% mrule([[a], [b, a], [c, d], [d, c], [e, b], [e, c]]).
%% mrule([[a, b], [b, a], [c, d], [d, c], [e, b], [e, c]]).
%% mrule([[a], [b, a], [c, d], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [b, a], [c, d], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [b, a], [c, d], [d, e], [e, a], [e, c]]).
%% mrule([[a], [b, a], [c, d], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [b, a], [c, d], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [b, a], [c, d], [e, a], [e, c], [e, d]]).
%% mrule([[a], [b, a], [c, d], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [b, a], [c, d], [e, b], [e, c], [e, d]]).
%% mrule([[a], [b, c], [b, c], [b, c], [b, c], [c, a]]).
%% mrule([[a, b], [b, c], [b, c], [b, c], [b, c], [c, a]]).
%% mrule([[a], [b, c], [b, c], [b, c], [b, d], [d, a]]).
%% mrule([[a, b], [b, c], [b, c], [b, c], [b, d], [d, a]]).
%% mrule([[a], [b, c], [b, c], [b, c], [c, a], [c, a]]).
%% mrule([[a, b], [b, c], [b, c], [b, c], [c, a], [c, a]]).
%% mrule([[a], [b, c], [b, c], [b, c], [c, a], [c, b]]).
%% mrule([[a, b], [b, c], [b, c], [b, c], [c, a], [c, b]]).
%% mrule([[a], [b, c], [b, c], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [b, c], [b, c], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [b, c], [b, c], [d, a], [d, a]]).
%% mrule([[a], [b, c], [b, c], [b, c], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [b, c], [b, c], [d, a], [d, b]]).
%% mrule([[a], [b, c], [b, c], [b, c], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [b, c], [b, c], [d, a], [d, c]]).
%% mrule([[a], [b, c], [b, c], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [b, c], [b, c], [b, d], [b, d], [c, a]]).
%% mrule([[a], [b, c], [b, c], [b, d], [b, d], [d, a]]).
%% mrule([[a, b], [b, c], [b, c], [b, d], [b, d], [d, a]]).
%% mrule([[a], [b, c], [b, c], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [b, c], [b, c], [b, d], [c, a], [c, d]]).
%% mrule([[a], [b, c], [b, c], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [b, c], [b, c], [b, d], [c, a], [d, a]]).
%% mrule([[a], [b, c], [b, c], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [b, c], [b, c], [b, d], [c, a], [d, b]]).
%% mrule([[a], [b, c], [b, c], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [b, c], [b, c], [b, d], [c, a], [d, c]]).
%% mrule([[a], [b, c], [b, c], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [b, c], [b, c], [b, d], [c, b], [d, a]]).
%% mrule([[a], [b, c], [b, c], [b, d], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [b, c], [b, d], [c, d], [d, a]]).
%% mrule([[a], [b, c], [b, c], [b, d], [d, a], [d, a]]).
%% mrule([[a, b], [b, c], [b, c], [b, d], [d, a], [d, a]]).
%% mrule([[a], [b, c], [b, c], [b, d], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [b, c], [b, d], [d, a], [d, b]]).
%% mrule([[a], [b, c], [b, c], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [b, c], [b, d], [d, a], [d, c]]).
%% mrule([[a], [b, c], [b, c], [b, d], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [b, c], [b, d], [d, e], [e, a]]).
%% mrule([[a], [b, c], [b, c], [b, d], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [b, c], [b, d], [e, a], [e, d]]).
%% mrule([[a], [b, c], [b, c], [c, a], [c, a], [c, a]]).
%% mrule([[a, b], [b, c], [b, c], [c, a], [c, a], [c, a]]).
%% mrule([[a], [b, c], [b, c], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [b, c], [b, c], [c, a], [c, a], [c, b]]).
%% mrule([[a], [b, c], [b, c], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b, c], [b, c], [c, a], [c, b], [c, b]]).
%% mrule([[a], [b, c], [b, c], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [b, c], [b, c], [c, a], [c, d], [c, d]]).
%% mrule([[a], [b, c], [b, c], [c, a], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [b, c], [c, a], [c, d], [d, a]]).
%% mrule([[a], [b, c], [b, c], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b, c], [b, c], [c, a], [c, d], [d, b]]).
%% mrule([[a], [b, c], [b, c], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [b, c], [b, c], [c, a], [c, d], [d, c]]).
%% mrule([[a], [b, c], [b, c], [c, a], [d, a], [d, a]]).
%% mrule([[a, b], [b, c], [b, c], [c, a], [d, a], [d, a]]).
%% mrule([[a], [b, c], [b, c], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [b, c], [c, a], [d, a], [d, b]]).
%% mrule([[a], [b, c], [b, c], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [b, c], [c, a], [d, a], [d, c]]).
%% mrule([[a], [b, c], [b, c], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [b, c], [b, c], [c, a], [d, b], [d, b]]).
%% mrule([[a], [b, c], [b, c], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [b, c], [c, a], [d, b], [d, c]]).
%% mrule([[a], [b, c], [b, c], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [b, c], [b, c], [c, a], [d, c], [d, c]]).
%% mrule([[a], [b, c], [b, c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [b, c], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [b, c], [c, b], [d, a], [d, a]]).
%% mrule([[a], [b, c], [b, c], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [b, c], [c, b], [d, a], [d, b]]).
%% mrule([[a], [b, c], [b, c], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [b, c], [c, b], [d, a], [d, c]]).
%% mrule([[a], [b, c], [b, c], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [b, c], [c, d], [c, d], [d, a]]).
%% mrule([[a], [b, c], [b, c], [c, d], [d, a], [d, a]]).
%% mrule([[a, b], [b, c], [b, c], [c, d], [d, a], [d, a]]).
%% mrule([[a], [b, c], [b, c], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [b, c], [c, d], [d, a], [d, b]]).
%% mrule([[a], [b, c], [b, c], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [b, c], [c, d], [d, a], [d, c]]).
%% mrule([[a], [b, c], [b, c], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [b, c], [c, d], [d, e], [e, a]]).
%% mrule([[a], [b, c], [b, c], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [b, c], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [b, c], [d, a], [d, a], [d, a]]).
%% mrule([[a], [b, c], [b, c], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [b, c], [d, a], [d, a], [d, b]]).
%% mrule([[a], [b, c], [b, c], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [b, c], [d, a], [d, a], [d, c]]).
%% mrule([[a], [b, c], [b, c], [d, a], [d, b], [d, b]]).
%% mrule([[a, b], [b, c], [b, c], [d, a], [d, b], [d, b]]).
%% mrule([[a], [b, c], [b, c], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [b, c], [d, a], [d, b], [d, c]]).
%% mrule([[a], [b, c], [b, c], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [b, c], [b, c], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [b, c], [b, c], [d, a], [d, e], [d, e]]).
%% mrule([[a, b], [b, c], [b, c], [d, a], [d, e], [e, a]]).
%% mrule([[a], [b, c], [b, c], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [b, c], [b, c], [d, a], [d, e], [e, b]]).
%% mrule([[a], [b, c], [b, c], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [b, c], [b, c], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [b, c], [b, c], [d, a], [d, e], [e, d]]).
%% mrule([[a, b], [b, c], [b, c], [d, a], [e, a], [e, d]]).
%% mrule([[a], [b, c], [b, c], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [b, c], [d, a], [e, b], [e, d]]).
%% mrule([[a], [b, c], [b, c], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [b, c], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [b, c], [d, a], [e, d], [e, d]]).
%% mrule([[a], [b, c], [b, c], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [b, c], [d, b], [d, e], [e, a]]).
%% mrule([[a], [b, c], [b, c], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [b, c], [d, b], [e, a], [e, d]]).
%% mrule([[a], [b, c], [b, c], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [b, c], [d, c], [d, e], [e, a]]).
%% mrule([[a], [b, c], [b, c], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [b, c], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [b, c], [d, e], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [b, c], [d, e], [e, a], [e, d]]).
%% mrule([[a], [b, c], [b, d], [b, d], [b, d], [c, a]]).
%% mrule([[a, b], [b, c], [b, d], [b, d], [b, d], [c, a]]).
%% mrule([[a], [b, c], [b, d], [b, d], [c, a], [c, a]]).
%% mrule([[a, b], [b, c], [b, d], [b, d], [c, a], [c, a]]).
%% mrule([[a], [b, c], [b, d], [b, d], [c, a], [c, b]]).
%% mrule([[a, b], [b, c], [b, d], [b, d], [c, a], [c, b]]).
%% mrule([[a], [b, c], [b, d], [b, d], [c, a], [c, d]]).
%% mrule([[a, b], [b, c], [b, d], [b, d], [c, a], [c, d]]).
%% mrule([[a], [b, c], [b, d], [b, d], [c, a], [d, a]]).
%% mrule([[a, b], [b, c], [b, d], [b, d], [c, a], [d, a]]).
%% mrule([[a], [b, c], [b, d], [b, d], [c, a], [d, b]]).
%% mrule([[a, b], [b, c], [b, d], [b, d], [c, a], [d, b]]).
%% mrule([[a], [b, c], [b, d], [b, d], [c, a], [d, c]]).
%% mrule([[a, b], [b, c], [b, d], [b, d], [c, a], [d, c]]).
%% mrule([[a], [b, c], [b, d], [b, d], [c, b], [d, a]]).
%% mrule([[a, b], [b, c], [b, d], [b, d], [c, b], [d, a]]).
%% mrule([[a], [b, c], [b, d], [b, d], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [b, d], [b, d], [c, d], [d, a]]).
%% mrule([[a], [b, c], [b, d], [b, d], [c, e], [e, a]]).
%% mrule([[a, b], [b, c], [b, d], [b, d], [c, e], [e, a]]).
%% mrule([[a], [b, c], [b, d], [b, d], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [b, d], [b, d], [d, a], [d, c]]).
%% mrule([[a], [b, c], [b, d], [b, d], [e, a], [e, c]]).
%% mrule([[a, b], [b, c], [b, d], [b, d], [e, a], [e, c]]).
%% mrule([[a], [b, c], [b, d], [b, e], [c, a], [d, e]]).
%% mrule([[a, b], [b, c], [b, d], [b, e], [c, a], [d, e]]).
%% mrule([[a], [b, c], [b, d], [b, e], [c, a], [e, d]]).
%% mrule([[a, b], [b, c], [b, d], [b, e], [c, a], [e, d]]).
%% mrule([[a], [b, c], [b, d], [b, e], [c, d], [e, a]]).
%% mrule([[a, b], [b, c], [b, d], [b, e], [c, d], [e, a]]).
%% mrule([[a], [b, c], [b, d], [b, e], [c, e], [d, a]]).
%% mrule([[a, b], [b, c], [b, d], [b, e], [c, e], [d, a]]).
%% mrule([[a], [b, c], [b, d], [b, e], [d, a], [e, c]]).
%% mrule([[a, b], [b, c], [b, d], [b, e], [d, a], [e, c]]).
%% mrule([[a], [b, c], [b, d], [b, e], [d, c], [e, a]]).
%% mrule([[a, b], [b, c], [b, d], [b, e], [d, c], [e, a]]).
%% mrule([[a], [b, c], [b, d], [c, a], [c, a], [c, d]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [c, a], [c, d]]).
%% mrule([[a], [b, c], [b, d], [c, a], [c, a], [d, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [c, a], [d, a]]).
%% mrule([[a], [b, c], [b, d], [c, a], [c, a], [d, b]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [c, a], [d, b]]).
%% mrule([[a], [b, c], [b, d], [c, a], [c, a], [d, c]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [c, a], [d, c]]).
%% mrule([[a], [b, c], [b, d], [c, a], [c, b], [c, d]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [c, b], [c, d]]).
%% mrule([[a], [b, c], [b, d], [c, a], [c, b], [d, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [c, b], [d, a]]).
%% mrule([[a], [b, c], [b, d], [c, a], [c, b], [d, b]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [c, b], [d, b]]).
%% mrule([[a], [b, c], [b, d], [c, a], [c, b], [d, c]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [c, b], [d, c]]).
%% mrule([[a], [b, c], [b, d], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [c, d], [c, d]]).
%% mrule([[a], [b, c], [b, d], [c, a], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [c, d], [d, a]]).
%% mrule([[a], [b, c], [b, d], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [c, d], [d, b]]).
%% mrule([[a], [b, c], [b, d], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [c, d], [d, c]]).
%% mrule([[a], [b, c], [b, d], [c, a], [c, e], [d, e]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [c, e], [d, e]]).
%% mrule([[a], [b, c], [b, d], [c, a], [c, e], [e, d]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [c, e], [e, d]]).
%% mrule([[a], [b, c], [b, d], [c, a], [d, a], [d, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [d, a], [d, a]]).
%% mrule([[a], [b, c], [b, d], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [d, a], [d, b]]).
%% mrule([[a], [b, c], [b, d], [c, a], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [d, a], [d, c]]).
%% mrule([[a], [b, c], [b, d], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [d, b], [d, b]]).
%% mrule([[a], [b, c], [b, d], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [d, b], [d, c]]).
%% mrule([[a], [b, c], [b, d], [c, a], [d, c], [d, c]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [d, c], [d, c]]).
%% mrule([[a], [b, c], [b, d], [c, a], [d, e], [d, e]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [d, e], [d, e]]).
%% mrule([[a], [b, c], [b, d], [c, a], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [d, e], [e, a]]).
%% mrule([[a], [b, c], [b, d], [c, a], [d, e], [e, b]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [d, e], [e, b]]).
%% mrule([[a], [b, c], [b, d], [c, a], [d, e], [e, c]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [d, e], [e, c]]).
%% mrule([[a], [b, c], [b, d], [c, a], [d, e], [e, d]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [d, e], [e, d]]).
%% mrule([[a], [b, c], [b, d], [c, a], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [e, a], [e, d]]).
%% mrule([[a], [b, c], [b, d], [c, a], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [e, b], [e, d]]).
%% mrule([[a], [b, c], [b, d], [c, a], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [e, c], [e, d]]).
%% mrule([[a], [b, c], [b, d], [c, a], [e, d], [e, d]]).
%% mrule([[a, b], [b, c], [b, d], [c, a], [e, d], [e, d]]).
%% mrule([[a], [b, c], [b, d], [c, b], [c, b], [d, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, b], [c, b], [d, a]]).
%% mrule([[a], [b, c], [b, d], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, b], [c, d], [d, a]]).
%% mrule([[a], [b, c], [b, d], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, b], [d, a], [d, a]]).
%% mrule([[a], [b, c], [b, d], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [b, d], [c, b], [d, a], [d, b]]).
%% mrule([[a], [b, c], [b, d], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [b, d], [c, b], [d, a], [d, c]]).
%% mrule([[a], [b, c], [b, d], [c, b], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, b], [d, e], [e, a]]).
%% mrule([[a], [b, c], [b, d], [c, b], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [b, d], [c, b], [e, a], [e, d]]).
%% mrule([[a], [b, c], [b, d], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, d], [c, d], [d, a]]).
%% mrule([[a], [b, c], [b, d], [c, d], [c, e], [e, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, d], [c, e], [e, a]]).
%% mrule([[a], [b, c], [b, d], [c, d], [d, a], [d, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, d], [d, a], [d, a]]).
%% mrule([[a], [b, c], [b, d], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [b, d], [c, d], [d, a], [d, b]]).
%% mrule([[a], [b, c], [b, d], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [b, d], [c, d], [d, a], [d, c]]).
%% mrule([[a], [b, c], [b, d], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, d], [e, a], [e, a]]).
%% mrule([[a], [b, c], [b, d], [c, d], [e, a], [e, b]]).
%% mrule([[a, b], [b, c], [b, d], [c, d], [e, a], [e, b]]).
%% mrule([[a], [b, c], [b, d], [c, d], [e, a], [e, c]]).
%% mrule([[a, b], [b, c], [b, d], [c, d], [e, a], [e, c]]).
%% mrule([[a], [b, c], [b, d], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [b, d], [c, d], [e, a], [e, d]]).
%% mrule([[a], [b, c], [b, d], [c, e], [c, e], [d, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, e], [c, e], [d, a]]).
%% mrule([[a], [b, c], [b, d], [c, e], [d, a], [d, e]]).
%% mrule([[a, b], [b, c], [b, d], [c, e], [d, a], [d, e]]).
%% mrule([[a], [b, c], [b, d], [c, e], [d, a], [e, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, e], [d, a], [e, a]]).
%% mrule([[a], [b, c], [b, d], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [b, c], [b, d], [c, e], [d, a], [e, b]]).
%% mrule([[a], [b, c], [b, d], [c, e], [d, a], [e, c]]).
%% mrule([[a, b], [b, c], [b, d], [c, e], [d, a], [e, c]]).
%% mrule([[a], [b, c], [b, d], [c, e], [d, a], [e, d]]).
%% mrule([[a, b], [b, c], [b, d], [c, e], [d, a], [e, d]]).
%% mrule([[a], [b, c], [b, d], [c, e], [d, b], [e, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, e], [d, b], [e, a]]).
%% mrule([[a], [b, c], [b, d], [c, e], [d, c], [e, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, e], [d, c], [e, a]]).
%% mrule([[a], [b, c], [b, d], [c, e], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [b, d], [c, e], [d, e], [e, a]]).
%% mrule([[a], [b, c], [b, d], [c, e], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [b, d], [c, e], [e, a], [e, d]]).
%% mrule([[a], [b, c], [b, d], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [b, d], [d, a], [d, a], [d, c]]).
%% mrule([[a], [b, c], [b, d], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [b, d], [d, a], [d, b], [d, c]]).
%% mrule([[a], [b, c], [b, d], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [b, c], [b, d], [d, a], [d, c], [d, c]]).
%% mrule([[a], [b, c], [b, d], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [b, c], [b, d], [d, a], [d, e], [e, c]]).
%% mrule([[a], [b, c], [b, d], [d, a], [e, a], [e, c]]).
%% mrule([[a, b], [b, c], [b, d], [d, a], [e, a], [e, c]]).
%% mrule([[a], [b, c], [b, d], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [b, c], [b, d], [d, a], [e, b], [e, c]]).
%% mrule([[a], [b, c], [b, d], [d, a], [e, c], [e, c]]).
%% mrule([[a, b], [b, c], [b, d], [d, a], [e, c], [e, c]]).
%% mrule([[a], [b, c], [b, d], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [b, d], [d, a], [e, c], [e, d]]).
%% mrule([[a], [b, c], [b, d], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [b, c], [b, d], [d, b], [e, a], [e, c]]).
%% mrule([[a], [b, c], [b, d], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [b, d], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [b, d], [d, c], [e, a], [e, a]]).
%% mrule([[a], [b, c], [b, d], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [b, c], [b, d], [d, c], [e, a], [e, b]]).
%% mrule([[a], [b, c], [b, d], [d, c], [e, a], [e, c]]).
%% mrule([[a, b], [b, c], [b, d], [d, c], [e, a], [e, c]]).
%% mrule([[a], [b, c], [b, d], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [b, d], [d, c], [e, a], [e, d]]).
%% mrule([[a], [b, c], [b, d], [d, e], [e, a], [e, c]]).
%% mrule([[a, b], [b, c], [b, d], [d, e], [e, a], [e, c]]).
%% mrule([[a], [b, c], [b, d], [e, a], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [b, d], [e, a], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [c, a], [c, a], [c, a]]).
%% mrule([[a], [b, c], [c, a], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, a], [c, a], [c, b]]).
%% mrule([[a], [b, c], [c, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, a], [c, d], [c, d]]).
%% mrule([[a, b], [b, c], [c, a], [c, a], [c, d], [d, a]]).
%% mrule([[a], [b, c], [c, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, a], [c, d], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [c, a], [d, a], [d, a]]).
%% mrule([[a], [b, c], [c, a], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, a], [d, a], [d, c]]).
%% mrule([[a], [b, c], [c, a], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, a], [d, b], [d, b]]).
%% mrule([[a], [b, c], [c, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [c, a], [d, c], [d, c]]).
%% mrule([[a], [b, c], [c, a], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, b], [c, b], [c, b]]).
%% mrule([[a], [b, c], [c, a], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [b, c], [c, a], [c, b], [c, d], [c, d]]).
%% mrule([[a], [b, c], [c, a], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [c, a], [c, b], [c, d], [d, a]]).
%% mrule([[a], [b, c], [c, a], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, b], [c, d], [d, b]]).
%% mrule([[a], [b, c], [c, a], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [c, b], [c, d], [d, c]]).
%% mrule([[a], [b, c], [c, a], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [b, c], [c, a], [c, b], [d, a], [d, a]]).
%% mrule([[a], [b, c], [c, a], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, b], [d, a], [d, b]]).
%% mrule([[a], [b, c], [c, a], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [c, b], [d, a], [d, c]]).
%% mrule([[a], [b, c], [c, a], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, b], [d, b], [d, b]]).
%% mrule([[a], [b, c], [c, a], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [c, b], [d, b], [d, c]]).
%% mrule([[a], [b, c], [c, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [c, d], [c, d]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [c, d], [d, a]]).
%% mrule([[a], [b, c], [c, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [c, d], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [c, e], [d, e]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [c, e], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [d, a], [d, a]]).
%% mrule([[a], [b, c], [c, a], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [d, a], [d, c]]).
%% mrule([[a], [b, c], [c, a], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [d, b], [d, b]]).
%% mrule([[a], [b, c], [c, a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [d, c], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [d, e], [d, e]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [d, e], [e, a]]).
%% mrule([[a], [b, c], [c, a], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [d, e], [e, c]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [d, e], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [e, a], [e, d]]).
%% mrule([[a], [b, c], [c, a], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [c, d], [e, d], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [d, a], [d, a], [d, a]]).
%% mrule([[a], [b, c], [c, a], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [d, a], [d, a], [d, c]]).
%% mrule([[a], [b, c], [c, a], [d, a], [d, b], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [d, a], [d, b], [d, b]]).
%% mrule([[a], [b, c], [c, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [d, a], [d, e], [d, e]]).
%% mrule([[a, b], [b, c], [c, a], [d, a], [d, e], [e, a]]).
%% mrule([[a], [b, c], [c, a], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [b, c], [c, a], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [b, c], [c, a], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [b, c], [c, a], [d, a], [d, e], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [d, a], [e, a], [e, d]]).
%% mrule([[a], [b, c], [c, a], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [d, a], [e, d], [e, d]]).
%% mrule([[a], [b, c], [c, a], [d, b], [d, b], [d, b]]).
%% mrule([[a, b], [b, c], [c, a], [d, b], [d, b], [d, b]]).
%% mrule([[a], [b, c], [c, a], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [d, b], [d, b], [d, c]]).
%% mrule([[a], [b, c], [c, a], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [d, b], [d, c], [d, c]]).
%% mrule([[a], [b, c], [c, a], [d, b], [d, e], [d, e]]).
%% mrule([[a, b], [b, c], [c, a], [d, b], [d, e], [d, e]]).
%% mrule([[a], [b, c], [c, a], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [c, a], [d, b], [d, e], [e, a]]).
%% mrule([[a], [b, c], [c, a], [d, b], [d, e], [e, b]]).
%% mrule([[a, b], [b, c], [c, a], [d, b], [d, e], [e, b]]).
%% mrule([[a], [b, c], [c, a], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [b, c], [c, a], [d, b], [d, e], [e, c]]).
%% mrule([[a], [b, c], [c, a], [d, b], [d, e], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [d, b], [d, e], [e, d]]).
%% mrule([[a], [b, c], [c, a], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [d, b], [e, a], [e, d]]).
%% mrule([[a], [b, c], [c, a], [d, b], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [d, b], [e, b], [e, d]]).
%% mrule([[a], [b, c], [c, a], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [d, b], [e, c], [e, d]]).
%% mrule([[a], [b, c], [c, a], [d, b], [e, d], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [d, b], [e, d], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [b, c], [c, a], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [b, c], [c, a], [d, c], [d, e], [e, a]]).
%% mrule([[a], [b, c], [c, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [b, c], [c, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [b, c], [c, a], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [b, c], [c, a], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [d, c], [e, a], [e, d]]).
%% mrule([[a], [b, c], [c, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [d, e], [d, e], [e, a]]).
%% mrule([[a], [b, c], [c, a], [d, e], [d, e], [e, b]]).
%% mrule([[a, b], [b, c], [c, a], [d, e], [d, e], [e, b]]).
%% mrule([[a, b], [b, c], [c, a], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [b, c], [c, a], [d, e], [e, a], [e, d]]).
%% mrule([[a], [b, c], [c, a], [d, e], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [d, e], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [c, a], [d, e], [e, c], [e, d]]).
%% mrule([[a], [b, c], [c, b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [c, b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [c, b], [c, b], [d, a], [d, a]]).
%% mrule([[a], [b, c], [c, b], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [c, b], [c, b], [d, a], [d, b]]).
%% mrule([[a], [b, c], [c, b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [c, b], [c, b], [d, a], [d, c]]).
%% mrule([[a], [b, c], [c, b], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [c, b], [c, d], [c, d], [d, a]]).
%% mrule([[a], [b, c], [c, b], [c, d], [d, a], [d, a]]).
%% mrule([[a, b], [b, c], [c, b], [c, d], [d, a], [d, a]]).
%% mrule([[a], [b, c], [c, b], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [c, b], [c, d], [d, a], [d, b]]).
%% mrule([[a], [b, c], [c, b], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [c, b], [c, d], [d, a], [d, c]]).
%% mrule([[a], [b, c], [c, b], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [c, b], [c, d], [d, e], [e, a]]).
%% mrule([[a], [b, c], [c, b], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [c, b], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [c, b], [d, a], [d, a], [d, a]]).
%% mrule([[a], [b, c], [c, b], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [c, b], [d, a], [d, a], [d, b]]).
%% mrule([[a], [b, c], [c, b], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [c, b], [d, a], [d, a], [d, c]]).
%% mrule([[a], [b, c], [c, b], [d, a], [d, b], [d, b]]).
%% mrule([[a, b], [b, c], [c, b], [d, a], [d, b], [d, b]]).
%% mrule([[a], [b, c], [c, b], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [c, b], [d, a], [d, b], [d, c]]).
%% mrule([[a], [b, c], [c, b], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [b, c], [c, b], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [b, c], [c, b], [d, a], [d, e], [d, e]]).
%% mrule([[a, b], [b, c], [c, b], [d, a], [d, e], [e, a]]).
%% mrule([[a], [b, c], [c, b], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [b, c], [c, b], [d, a], [d, e], [e, b]]).
%% mrule([[a], [b, c], [c, b], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [b, c], [c, b], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [b, c], [c, b], [d, a], [d, e], [e, d]]).
%% mrule([[a, b], [b, c], [c, b], [d, a], [e, a], [e, d]]).
%% mrule([[a], [b, c], [c, b], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [c, b], [d, a], [e, b], [e, d]]).
%% mrule([[a], [b, c], [c, b], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [c, b], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [c, b], [d, a], [e, d], [e, d]]).
%% mrule([[a], [b, c], [c, b], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [c, b], [d, b], [d, e], [e, a]]).
%% mrule([[a], [b, c], [c, b], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [c, b], [d, b], [e, a], [e, d]]).
%% mrule([[a], [b, c], [c, b], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [c, b], [d, c], [d, e], [e, a]]).
%% mrule([[a], [b, c], [c, b], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [c, b], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [c, b], [d, e], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [c, b], [d, e], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [c, d], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [b, c], [c, d], [c, d], [c, e], [e, a]]).
%% mrule([[a, b], [b, c], [c, d], [c, d], [d, a], [d, a]]).
%% mrule([[a], [b, c], [c, d], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [c, d], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [c, d], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [b, c], [c, d], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [c, d], [c, d], [e, a], [e, a]]).
%% mrule([[a], [b, c], [c, d], [c, d], [e, a], [e, b]]).
%% mrule([[a, b], [b, c], [c, d], [c, d], [e, a], [e, b]]).
%% mrule([[a, b], [b, c], [c, d], [c, d], [e, a], [e, c]]).
%% mrule([[a, b], [b, c], [c, d], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [c, d], [c, e], [c, e], [d, a]]).
%% mrule([[a, b], [b, c], [c, d], [c, e], [d, a], [d, e]]).
%% mrule([[a, b], [b, c], [c, d], [c, e], [d, a], [e, a]]).
%% mrule([[a], [b, c], [c, d], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [b, c], [c, d], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [b, c], [c, d], [c, e], [d, a], [e, c]]).
%% mrule([[a, b], [b, c], [c, d], [c, e], [d, a], [e, d]]).
%% mrule([[a], [b, c], [c, d], [c, e], [d, b], [e, a]]).
%% mrule([[a, b], [b, c], [c, d], [c, e], [d, b], [e, a]]).
%% mrule([[a, b], [b, c], [c, d], [c, e], [d, c], [e, a]]).
%% mrule([[a, b], [b, c], [c, d], [c, e], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [c, d], [c, e], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [d, a], [d, a]]).
%% mrule([[a], [b, c], [c, d], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [d, a], [d, c]]).
%% mrule([[a], [b, c], [c, d], [d, a], [d, b], [d, b]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [d, b], [d, b]]).
%% mrule([[a], [b, c], [c, d], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [d, e], [d, e]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [d, e], [e, a]]).
%% mrule([[a], [b, c], [c, d], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [d, e], [e, d]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [e, a], [e, a]]).
%% mrule([[a], [b, c], [c, d], [d, a], [e, a], [e, b]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [e, a], [e, b]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [e, a], [e, c]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [e, a], [e, d]]).
%% mrule([[a], [b, c], [c, d], [d, a], [e, b], [e, b]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [e, b], [e, b]]).
%% mrule([[a], [b, c], [c, d], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [e, b], [e, c]]).
%% mrule([[a], [b, c], [c, d], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [e, c], [e, c]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [c, d], [d, a], [e, d], [e, d]]).
%% mrule([[a], [b, c], [c, d], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [c, d], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [c, d], [d, b], [e, a], [e, a]]).
%% mrule([[a], [b, c], [c, d], [d, b], [e, a], [e, b]]).
%% mrule([[a, b], [b, c], [c, d], [d, b], [e, a], [e, b]]).
%% mrule([[a], [b, c], [c, d], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [b, c], [c, d], [d, b], [e, a], [e, c]]).
%% mrule([[a], [b, c], [c, d], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [c, d], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [c, d], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [c, d], [d, c], [e, a], [e, a]]).
%% mrule([[a], [b, c], [c, d], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [b, c], [c, d], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [b, c], [c, d], [d, c], [e, a], [e, c]]).
%% mrule([[a, b], [b, c], [c, d], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [c, d], [d, e], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [c, d], [d, e], [e, a], [e, a]]).
%% mrule([[a], [b, c], [c, d], [d, e], [e, a], [e, b]]).
%% mrule([[a, b], [b, c], [c, d], [d, e], [e, a], [e, b]]).
%% mrule([[a, b], [b, c], [c, d], [d, e], [e, a], [e, c]]).
%% mrule([[a, b], [b, c], [c, d], [d, e], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [c, d], [d, e], [e, f], [f, a]]).
%% mrule([[a, b], [b, c], [c, d], [d, e], [f, a], [f, e]]).
%% mrule([[a, b], [b, c], [c, d], [e, a], [e, a], [e, d]]).
%% mrule([[a], [b, c], [c, d], [e, a], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [c, d], [e, a], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [c, d], [e, a], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [c, d], [e, a], [e, d], [e, d]]).
%% mrule([[a, b], [b, c], [c, d], [e, a], [e, f], [f, d]]).
%% mrule([[a, b], [b, c], [c, d], [e, a], [f, d], [f, e]]).
%% mrule([[a, b], [b, c], [c, d], [e, d], [e, f], [f, a]]).
%% mrule([[a, b], [b, c], [c, d], [e, d], [f, a], [f, e]]).
%% mrule([[a, b], [b, c], [c, d], [e, a], [e, f], [d, f]]).
%% mrule([[a, b], [b, c], [c, d], [e, a], [d, f], [f, e]]).
%% mrule([[a, b], [b, c], [d, a], [d, a], [d, a], [d, c]]).
%% mrule([[a], [b, c], [d, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, a], [e, a], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, a], [e, c], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, a], [e, c], [e, d]]).
%% mrule([[a], [b, c], [d, a], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, b], [d, b], [d, c]]).
%% mrule([[a], [b, c], [d, a], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, b], [d, c], [d, c]]).
%% mrule([[a], [b, c], [d, a], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, b], [d, e], [e, c]]).
%% mrule([[a], [b, c], [d, a], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, b], [e, a], [e, c]]).
%% mrule([[a], [b, c], [d, a], [d, b], [e, b], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, b], [e, b], [e, c]]).
%% mrule([[a], [b, c], [d, a], [d, b], [e, c], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, b], [e, c], [e, c]]).
%% mrule([[a], [b, c], [d, a], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [d, e], [e, a]]).
%% mrule([[a], [b, c], [d, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [e, a], [e, a]]).
%% mrule([[a], [b, c], [d, a], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [e, a], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [e, a], [e, d]]).
%% mrule([[a], [b, c], [d, a], [d, c], [e, b], [e, b]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [e, b], [e, b]]).
%% mrule([[a], [b, c], [d, a], [d, c], [e, b], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [e, b], [e, c]]).
%% mrule([[a], [b, c], [d, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [e, c], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, e], [e, a], [e, c]]).
%% mrule([[a], [b, c], [d, a], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, e], [e, c], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, e], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [d, e], [e, f], [f, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, e], [f, c], [f, e]]).
%% mrule([[a, b], [b, c], [d, a], [e, a], [e, c], [e, d]]).
%% mrule([[a], [b, c], [d, a], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [e, c], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [e, c], [e, d], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [e, c], [e, f], [f, d]]).
%% mrule([[a, b], [b, c], [d, a], [e, c], [f, d], [f, e]]).
%% mrule([[a, b], [b, c], [d, a], [e, d], [e, f], [f, c]]).
%% mrule([[a, b], [b, c], [d, a], [e, d], [f, c], [f, e]]).
%% mrule([[a], [b, c], [d, b], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [b, c], [d, b], [d, b], [e, a], [e, c]]).
%% mrule([[a], [b, c], [d, b], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [d, b], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [d, b], [d, c], [e, a], [e, a]]).
%% mrule([[a], [b, c], [d, b], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [b, c], [d, b], [d, c], [e, a], [e, b]]).
%% mrule([[a], [b, c], [d, b], [d, c], [e, a], [e, c]]).
%% mrule([[a, b], [b, c], [d, b], [d, c], [e, a], [e, c]]).
%% mrule([[a], [b, c], [d, b], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [d, b], [d, c], [e, a], [e, d]]).
%% mrule([[a], [b, c], [d, b], [d, e], [e, a], [e, c]]).
%% mrule([[a, b], [b, c], [d, b], [d, e], [e, a], [e, c]]).
%% mrule([[a], [b, c], [d, b], [e, a], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [d, b], [e, a], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [d, c], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [d, c], [d, c], [e, a], [e, a]]).
%% mrule([[a], [b, c], [d, c], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [b, c], [d, c], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [b, c], [d, c], [d, c], [e, a], [e, c]]).
%% mrule([[a, b], [b, c], [d, c], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [d, c], [d, e], [d, e], [e, a]]).
%% mrule([[a, b], [b, c], [d, c], [d, e], [e, a], [e, a]]).
%% mrule([[a], [b, c], [d, c], [d, e], [e, a], [e, b]]).
%% mrule([[a, b], [b, c], [d, c], [d, e], [e, a], [e, b]]).
%% mrule([[a, b], [b, c], [d, c], [d, e], [e, a], [e, c]]).
%% mrule([[a, b], [b, c], [d, c], [d, e], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [d, c], [d, e], [e, f], [f, a]]).
%% mrule([[a, b], [b, c], [d, c], [d, e], [f, a], [f, e]]).
%% mrule([[a, b], [b, c], [d, c], [e, a], [e, a], [e, d]]).
%% mrule([[a], [b, c], [d, c], [e, a], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [d, c], [e, a], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [d, c], [e, a], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [d, c], [e, a], [e, d], [e, d]]).
%% mrule([[a, b], [b, c], [d, c], [e, a], [e, f], [f, d]]).
%% mrule([[a, b], [b, c], [d, c], [e, a], [f, d], [f, e]]).
%% mrule([[a, b], [b, c], [d, c], [e, d], [e, f], [f, a]]).
%% mrule([[a, b], [b, c], [d, c], [e, d], [f, a], [f, e]]).
%% mrule([[a, b], [b, c], [d, e], [d, e], [e, a], [e, c]]).
%% mrule([[a, b], [b, c], [d, e], [d, f], [e, a], [f, c]]).
%% mrule([[a, b], [b, c], [d, e], [d, f], [e, c], [f, a]]).
%% mrule([[a, b], [b, c], [d, e], [e, a], [e, c], [e, d]]).
%% mrule([[a, b], [b, c], [d, e], [e, a], [f, c], [f, d]]).
%% mrule([[a, b], [b, c], [d, e], [e, c], [f, a], [f, d]]).
%% mrule([[a], [b, c], [b, c], [d, a], [d, e], [c, e]]).
%% mrule([[a, b], [b, c], [b, c], [d, a], [d, e], [c, e]]).
%% mrule([[a], [b, c], [b, c], [d, a], [c, e], [e, d]]).
%% mrule([[a, b], [b, c], [b, c], [d, a], [c, e], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [d, a], [d, e], [c, e]]).
%% mrule([[a, b], [b, c], [d, a], [d, a], [c, e], [c, e]]).
%% mrule([[a, b], [b, c], [d, a], [d, a], [c, e], [e, a]]).
%% mrule([[a, b], [b, c], [d, a], [d, a], [c, e], [e, b]]).
%% mrule([[a, b], [b, c], [d, a], [d, a], [c, e], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [d, a], [c, e], [e, c]]).
%% mrule([[a], [b, c], [d, a], [d, b], [d, e], [c, e]]).
%% mrule([[a, b], [b, c], [d, a], [d, b], [d, e], [c, e]]).
%% mrule([[a], [b, c], [d, a], [d, b], [c, e], [c, e]]).
%% mrule([[a, b], [b, c], [d, a], [d, b], [c, e], [c, e]]).
%% mrule([[a], [b, c], [d, a], [d, b], [c, e], [e, a]]).
%% mrule([[a, b], [b, c], [d, a], [d, b], [c, e], [e, a]]).
%% mrule([[a], [b, c], [d, a], [d, b], [c, e], [e, b]]).
%% mrule([[a, b], [b, c], [d, a], [d, b], [c, e], [e, b]]).
%% mrule([[a], [b, c], [d, a], [d, b], [c, e], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [d, b], [c, e], [e, d]]).
%% mrule([[a], [b, c], [d, a], [d, b], [c, e], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, b], [c, e], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [d, e], [c, e]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [c, e], [c, e]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [c, e], [e, a]]).
%% mrule([[a], [b, c], [d, a], [d, c], [c, e], [e, b]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [c, e], [e, b]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [c, e], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [d, c], [c, e], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, e], [d, e], [c, e]]).
%% mrule([[a, b], [b, c], [d, a], [d, e], [c, a], [c, e]]).
%% mrule([[a], [b, c], [d, a], [d, e], [c, b], [c, e]]).
%% mrule([[a, b], [b, c], [d, a], [d, e], [c, b], [c, e]]).
%% mrule([[a, b], [b, c], [d, a], [d, e], [c, e], [c, e]]).
%% mrule([[a, b], [b, c], [d, a], [d, e], [c, e], [e, a]]).
%% mrule([[a], [b, c], [d, a], [d, e], [c, e], [e, b]]).
%% mrule([[a, b], [b, c], [d, a], [d, e], [c, e], [e, b]]).
%% mrule([[a, b], [b, c], [d, a], [d, e], [c, e], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [d, e], [c, e], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [d, e], [c, f], [e, f]]).
%% mrule([[a, b], [b, c], [d, a], [d, e], [c, f], [f, e]]).
%% mrule([[a, b], [b, c], [d, a], [c, a], [c, e], [e, d]]).
%% mrule([[a], [b, c], [d, a], [c, b], [c, e], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [c, b], [c, e], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [c, e], [c, e], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [c, e], [e, a], [e, d]]).
%% mrule([[a], [b, c], [d, a], [c, e], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [c, e], [e, b], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [c, e], [e, d], [e, d]]).
%% mrule([[a, b], [b, c], [d, a], [c, e], [e, d], [e, c]]).
%% mrule([[a, b], [b, c], [d, a], [c, e], [e, f], [f, d]]).
%% mrule([[a, b], [b, c], [d, a], [c, e], [f, d], [f, e]]).
%% mrule([[a], [b, c], [d, b], [d, b], [c, e], [e, a]]).
%% mrule([[a, b], [b, c], [d, b], [d, b], [c, e], [e, a]]).
%% mrule([[a], [b, c], [d, b], [d, c], [c, e], [e, a]]).
%% mrule([[a, b], [b, c], [d, b], [d, c], [c, e], [e, a]]).
%% mrule([[a], [b, c], [d, b], [d, e], [c, a], [c, e]]).
%% mrule([[a, b], [b, c], [d, b], [d, e], [c, a], [c, e]]).
%% mrule([[a], [b, c], [d, b], [d, e], [c, e], [e, a]]).
%% mrule([[a, b], [b, c], [d, b], [d, e], [c, e], [e, a]]).
%% mrule([[a], [b, c], [d, b], [c, a], [c, e], [e, d]]).
%% mrule([[a, b], [b, c], [d, b], [c, a], [c, e], [e, d]]).
%% mrule([[a], [b, c], [d, b], [c, e], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [d, b], [c, e], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [d, c], [d, c], [c, e], [e, a]]).
%% mrule([[a, b], [b, c], [d, c], [d, e], [c, a], [c, e]]).
%% mrule([[a, b], [b, c], [d, c], [d, e], [c, e], [e, a]]).
%% mrule([[a, b], [b, c], [d, c], [c, a], [c, e], [e, d]]).
%% mrule([[a, b], [b, c], [d, c], [c, e], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [d, e], [d, e], [c, a], [c, e]]).
%% mrule([[a, b], [b, c], [d, e], [d, e], [c, e], [e, a]]).
%% mrule([[a, b], [b, c], [d, e], [d, f], [c, e], [f, a]]).
%% mrule([[a, b], [b, c], [d, e], [d, f], [c, f], [e, a]]).
%% mrule([[a, b], [b, c], [d, e], [c, a], [c, e], [e, d]]).
%% mrule([[a, b], [b, c], [d, e], [c, e], [e, a], [e, d]]).
%% mrule([[a, b], [b, c], [d, e], [c, e], [f, a], [f, d]]).
%% mrule([[a, b], [b, c], [d, e], [c, f], [e, a], [f, d]]).
%% mrule([[a, b], [b, c], [d, a], [e, d], [e, f], [c, f]]).
%% mrule([[a, b], [b, c], [d, a], [e, d], [c, f], [f, e]]).
%% mrule([[a, b], [c, a], [c, a], [c, a], [c, a], [c, b]]).
%% mrule([[a, b], [c, a], [c, a], [c, a], [c, b], [c, b]]).
%% mrule([[a, b], [c, a], [c, a], [c, a], [c, d], [d, b]]).
%% mrule([[a, b], [c, a], [c, a], [c, a], [d, a], [d, b]]).
%% mrule([[a, b], [c, a], [c, a], [c, a], [d, b], [d, b]]).
%% mrule([[a, b], [c, a], [c, a], [c, a], [d, b], [d, c]]).
%% mrule([[a, b], [c, a], [c, a], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [c, a], [c, a], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [c, a], [c, a], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [c, a], [c, a], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [c, a], [c, a], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [c, a], [c, a], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [c, a], [c, a], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [c, a], [c, a], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [c, a], [c, a], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [c, a], [c, a], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [c, a], [c, a], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [c, a], [c, a], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [c, a], [c, a], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c, a], [c, a], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [c, a], [c, a], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [c, a], [c, a], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [c, a], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [c, a], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [c, a], [c, a], [d, a], [d, b], [d, b]]).
%% mrule([[a, b], [c, a], [c, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c, a], [c, a], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [c, a], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [c, a], [d, b], [d, b], [d, b]]).
%% mrule([[a, b], [c, a], [c, a], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [c, a], [c, a], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [c, a], [c, a], [d, b], [d, e], [d, e]]).
%% mrule([[a, b], [c, a], [c, a], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [c, a], [c, a], [d, b], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [c, a], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [c, a], [c, a], [d, b], [d, e], [e, d]]).
%% mrule([[a, b], [c, a], [c, a], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [c, a], [c, a], [d, b], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [c, a], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [c, a], [c, a], [d, b], [e, d], [e, d]]).
%% mrule([[a, b], [c, a], [c, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [c, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [c, a], [d, e], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [c, a], [d, e], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [c, b], [c, b], [c, b]]).
%% mrule([[a, b], [c, a], [c, b], [c, b], [c, d], [c, d]]).
%% mrule([[a, b], [c, a], [c, b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [c, a], [c, b], [c, b], [c, d], [d, b]]).
%% mrule([[a, b], [c, a], [c, b], [c, b], [c, d], [d, c]]).
%% mrule([[a, b], [c, a], [c, b], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [c, a], [c, b], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [c, a], [c, b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [c, a], [c, b], [c, b], [d, b], [d, b]]).
%% mrule([[a, b], [c, a], [c, b], [c, b], [d, b], [d, c]]).
%% mrule([[a, b], [c, a], [c, b], [c, b], [d, c], [d, c]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [c, d], [c, d]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [c, d], [d, c]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [c, e], [d, e]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [c, e], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [d, a], [d, a]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [d, c], [d, c]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [d, e], [d, e]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [d, e], [e, c]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [d, e], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [e, c], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [c, d], [e, d], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [d, a], [d, a], [d, a]]).
%% mrule([[a, b], [c, a], [c, b], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [c, a], [c, b], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [c, a], [c, b], [d, a], [d, b], [d, b]]).
%% mrule([[a, b], [c, a], [c, b], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c, a], [c, b], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [c, a], [c, b], [d, a], [d, e], [d, e]]).
%% mrule([[a, b], [c, a], [c, b], [d, a], [d, e], [e, a]]).
%% mrule([[a, b], [c, a], [c, b], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [c, b], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [c, a], [c, b], [d, a], [d, e], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [d, a], [e, a], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [d, a], [e, d], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [d, b], [d, b], [d, b]]).
%% mrule([[a, b], [c, a], [c, b], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [c, a], [c, b], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [c, a], [c, b], [d, b], [d, e], [d, e]]).
%% mrule([[a, b], [c, a], [c, b], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [c, a], [c, b], [d, b], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [c, b], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [c, a], [c, b], [d, b], [d, e], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [d, b], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [d, b], [e, d], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [c, a], [c, b], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [c, a], [c, b], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [c, a], [c, b], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [c, b], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [c, a], [c, b], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [d, e], [d, e], [e, a]]).
%% mrule([[a, b], [c, a], [c, b], [d, e], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [c, b], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [c, a], [c, b], [d, e], [e, a], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [d, e], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [c, b], [d, e], [e, c], [e, d]]).
%% mrule([[a, b], [c, a], [c, d], [c, d], [c, d], [d, b]]).
%% mrule([[a, b], [c, a], [c, d], [c, d], [c, e], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c, a], [c, d], [c, d], [d, b], [d, b]]).
%% mrule([[a, b], [c, a], [c, d], [c, d], [d, b], [d, c]]).
%% mrule([[a, b], [c, a], [c, d], [c, d], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [c, d], [e, a], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [c, d], [e, b], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [c, d], [e, b], [e, c]]).
%% mrule([[a, b], [c, a], [c, d], [c, d], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [c, d], [c, e], [c, e], [d, b]]).
%% mrule([[a, b], [c, a], [c, d], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [c, e], [d, b], [d, e]]).
%% mrule([[a, b], [c, a], [c, d], [c, e], [d, b], [e, a]]).
%% mrule([[a, b], [c, a], [c, d], [c, e], [d, b], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [c, e], [d, b], [e, c]]).
%% mrule([[a, b], [c, a], [c, d], [c, e], [d, b], [e, d]]).
%% mrule([[a, b], [c, a], [c, d], [c, e], [d, c], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [c, e], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [c, e], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [c, d], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [c, a], [c, d], [d, a], [d, b], [d, b]]).
%% mrule([[a, b], [c, a], [c, d], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c, a], [c, d], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [d, a], [e, a], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [d, a], [e, b], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [c, a], [c, d], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [c, d], [d, b], [d, b], [d, b]]).
%% mrule([[a, b], [c, a], [c, d], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [c, a], [c, d], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [c, a], [c, d], [d, b], [d, e], [d, e]]).
%% mrule([[a, b], [c, a], [c, d], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [c, a], [c, d], [d, b], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [c, a], [c, d], [d, b], [d, e], [e, d]]).
%% mrule([[a, b], [c, a], [c, d], [d, b], [e, a], [e, a]]).
%% mrule([[a, b], [c, a], [c, d], [d, b], [e, a], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [c, a], [c, d], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [c, a], [c, d], [d, b], [e, b], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [d, b], [e, b], [e, c]]).
%% mrule([[a, b], [c, a], [c, d], [d, b], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [c, d], [d, b], [e, c], [e, c]]).
%% mrule([[a, b], [c, a], [c, d], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [c, a], [c, d], [d, b], [e, d], [e, d]]).
%% mrule([[a, b], [c, a], [c, d], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [d, c], [e, b], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [d, c], [e, b], [e, c]]).
%% mrule([[a, b], [c, a], [c, d], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [c, d], [d, e], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [d, e], [e, a], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [d, e], [e, b], [e, b]]).
%% mrule([[a, b], [c, a], [c, d], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [c, a], [c, d], [d, e], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [c, d], [d, e], [e, f], [f, b]]).
%% mrule([[a, b], [c, a], [c, d], [d, e], [f, b], [f, e]]).
%% mrule([[a, b], [c, a], [c, d], [e, a], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [c, d], [e, b], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [c, d], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [c, a], [c, d], [e, b], [e, d], [e, d]]).
%% mrule([[a, b], [c, a], [c, d], [e, b], [e, f], [f, d]]).
%% mrule([[a, b], [c, a], [c, d], [e, b], [f, d], [f, e]]).
%% mrule([[a, b], [c, a], [c, d], [e, d], [e, f], [f, b]]).
%% mrule([[a, b], [c, a], [c, d], [e, d], [f, b], [f, e]]).
%% mrule([[a, b], [c, a], [c, d], [e, b], [e, f], [d, f]]).
%% mrule([[a, b], [c, a], [c, d], [e, b], [d, f], [f, e]]).
%% mrule([[a, b], [c, a], [d, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c, a], [d, a], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [c, a], [d, a], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [c, a], [d, a], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [c, a], [d, a], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [c, a], [d, a], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [c, a], [d, a], [d, b], [e, b], [e, c]]).
%% mrule([[a, b], [c, a], [d, a], [d, b], [e, c], [e, c]]).
%% mrule([[a, b], [c, a], [d, a], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [c, a], [d, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [d, a], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [c, a], [d, a], [d, c], [e, b], [e, b]]).
%% mrule([[a, b], [c, a], [d, a], [d, c], [e, b], [e, c]]).
%% mrule([[a, b], [c, a], [d, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [d, a], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [c, a], [d, a], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [c, a], [d, b], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [c, a], [d, b], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [c, a], [d, b], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [c, a], [d, b], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [c, a], [d, b], [d, b], [e, b], [e, c]]).
%% mrule([[a, b], [c, a], [d, b], [d, b], [e, c], [e, c]]).
%% mrule([[a, b], [c, a], [d, b], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [c, a], [d, b], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [c, a], [d, b], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [c, a], [d, b], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [c, a], [d, b], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [d, b], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [c, a], [d, b], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [c, a], [d, b], [d, c], [e, a], [e, a]]).
%% mrule([[a, b], [c, a], [d, b], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [c, a], [d, b], [d, c], [e, a], [e, c]]).
%% mrule([[a, b], [c, a], [d, b], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [c, a], [d, b], [d, c], [e, b], [e, b]]).
%% mrule([[a, b], [c, a], [d, b], [d, c], [e, b], [e, c]]).
%% mrule([[a, b], [c, a], [d, b], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [d, b], [d, c], [e, c], [e, c]]).
%% mrule([[a, b], [c, a], [d, b], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [c, a], [d, b], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [c, a], [d, b], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [c, a], [d, b], [d, e], [e, a], [e, c]]).
%% mrule([[a, b], [c, a], [d, b], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [c, a], [d, b], [d, e], [e, c], [e, c]]).
%% mrule([[a, b], [c, a], [d, b], [d, e], [e, c], [e, d]]).
%% mrule([[a, b], [c, a], [d, b], [d, e], [e, f], [f, c]]).
%% mrule([[a, b], [c, a], [d, b], [d, e], [f, c], [f, e]]).
%% mrule([[a, b], [c, a], [d, b], [e, a], [e, c], [e, d]]).
%% mrule([[a, b], [c, a], [d, b], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [c, a], [d, b], [e, c], [e, c], [e, d]]).
%% mrule([[a, b], [c, a], [d, b], [e, c], [e, d], [e, d]]).
%% mrule([[a, b], [c, a], [d, b], [e, c], [e, f], [f, d]]).
%% mrule([[a, b], [c, a], [d, b], [e, c], [f, d], [f, e]]).
%% mrule([[a, b], [c, a], [d, b], [e, d], [e, f], [f, c]]).
%% mrule([[a, b], [c, a], [d, b], [e, d], [f, c], [f, e]]).
%% mrule([[a, b], [c, a], [d, c], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [d, c], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [c, a], [d, c], [d, c], [e, b], [e, b]]).
%% mrule([[a, b], [c, a], [d, c], [d, c], [e, b], [e, c]]).
%% mrule([[a, b], [c, a], [d, c], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [d, c], [d, e], [d, e], [e, b]]).
%% mrule([[a, b], [c, a], [d, c], [d, e], [e, a], [e, b]]).
%% mrule([[a, b], [c, a], [d, c], [d, e], [e, b], [e, b]]).
%% mrule([[a, b], [c, a], [d, c], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [c, a], [d, c], [d, e], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [d, c], [d, e], [e, f], [f, b]]).
%% mrule([[a, b], [c, a], [d, c], [d, e], [f, b], [f, e]]).
%% mrule([[a, b], [c, a], [d, c], [e, a], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [d, c], [e, b], [e, b], [e, d]]).
%% mrule([[a, b], [c, a], [d, c], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [c, a], [d, c], [e, b], [e, d], [e, d]]).
%% mrule([[a, b], [c, a], [d, c], [e, b], [e, f], [f, d]]).
%% mrule([[a, b], [c, a], [d, c], [e, b], [f, d], [f, e]]).
%% mrule([[a, b], [c, a], [d, c], [e, d], [e, f], [f, b]]).
%% mrule([[a, b], [c, a], [d, c], [e, d], [f, b], [f, e]]).
%% mrule([[a, b], [c, a], [d, e], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [c, a], [d, e], [d, f], [e, b], [f, c]]).
%% mrule([[a, b], [c, a], [d, e], [d, f], [e, c], [f, b]]).
%% mrule([[a, b], [c, a], [d, e], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [c, a], [d, e], [e, b], [f, c], [f, d]]).
%% mrule([[a, b], [c, a], [d, e], [e, c], [f, b], [f, d]]).
%% mrule([[a, b], [c, b], [c, b], [c, b], [c, d], [d, a]]).
%% mrule([[a, b], [c, b], [c, b], [c, b], [d, a], [d, a]]).
%% mrule([[a, b], [c, b], [c, b], [c, b], [d, a], [d, b]]).
%% mrule([[a, b], [c, b], [c, b], [c, b], [d, a], [d, c]]).
%% mrule([[a, b], [c, b], [c, b], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [c, b], [c, b], [c, d], [d, a], [d, a]]).
%% mrule([[a, b], [c, b], [c, b], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c, b], [c, b], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [c, b], [c, b], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [c, b], [c, b], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [c, b], [c, b], [d, a], [d, a], [d, a]]).
%% mrule([[a, b], [c, b], [c, b], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [c, b], [c, b], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [c, b], [c, b], [d, a], [d, b], [d, b]]).
%% mrule([[a, b], [c, b], [c, b], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c, b], [c, b], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [c, b], [c, b], [d, a], [d, e], [d, e]]).
%% mrule([[a, b], [c, b], [c, b], [d, a], [d, e], [e, a]]).
%% mrule([[a, b], [c, b], [c, b], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [c, b], [c, b], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [c, b], [c, b], [d, a], [d, e], [e, d]]).
%% mrule([[a, b], [c, b], [c, b], [d, a], [e, a], [e, d]]).
%% mrule([[a, b], [c, b], [c, b], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [c, b], [c, b], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [c, b], [c, b], [d, a], [e, d], [e, d]]).
%% mrule([[a, b], [c, b], [c, b], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [c, b], [c, b], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [c, b], [c, b], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [c, b], [c, b], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [c, b], [c, b], [d, e], [d, e], [e, a]]).
%% mrule([[a, b], [c, b], [c, b], [d, e], [e, a], [e, d]]).
%% mrule([[a, b], [c, b], [c, d], [c, d], [c, d], [d, a]]).
%% mrule([[a, b], [c, b], [c, d], [c, d], [c, e], [e, a]]).
%% mrule([[a, b], [c, b], [c, d], [c, d], [d, a], [d, a]]).
%% mrule([[a, b], [c, b], [c, d], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c, b], [c, d], [c, d], [d, a], [d, c]]).
%% mrule([[a, b], [c, b], [c, d], [c, d], [d, e], [e, a]]).
%% mrule([[a, b], [c, b], [c, d], [c, d], [e, a], [e, a]]).
%% mrule([[a, b], [c, b], [c, d], [c, d], [e, a], [e, b]]).
%% mrule([[a, b], [c, b], [c, d], [c, d], [e, a], [e, c]]).
%% mrule([[a, b], [c, b], [c, d], [c, d], [e, a], [e, d]]).
%% mrule([[a, b], [c, b], [c, d], [c, e], [c, e], [d, a]]).
%% mrule([[a, b], [c, b], [c, d], [c, e], [d, a], [d, e]]).
%% mrule([[a, b], [c, b], [c, d], [c, e], [d, a], [e, a]]).
%% mrule([[a, b], [c, b], [c, d], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [c, b], [c, d], [c, e], [d, a], [e, c]]).
%% mrule([[a, b], [c, b], [c, d], [c, e], [d, a], [e, d]]).
%% mrule([[a, b], [c, b], [c, d], [c, e], [d, b], [e, a]]).
%% mrule([[a, b], [c, b], [c, d], [c, e], [d, c], [e, a]]).
%% mrule([[a, b], [c, b], [c, d], [c, e], [d, e], [e, a]]).
%% mrule([[a, b], [c, b], [c, d], [c, e], [e, a], [e, d]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [d, a], [d, a]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [d, b], [d, b]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [d, e], [d, e]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [d, e], [e, a]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [d, e], [e, d]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [e, a], [e, a]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [e, a], [e, b]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [e, a], [e, c]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [e, a], [e, d]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [e, b], [e, b]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [e, c], [e, c]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [c, b], [c, d], [d, a], [e, d], [e, d]]).
%% mrule([[a, b], [c, b], [c, d], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [c, b], [c, d], [d, b], [e, a], [e, a]]).
%% mrule([[a, b], [c, b], [c, d], [d, b], [e, a], [e, b]]).
%% mrule([[a, b], [c, b], [c, d], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [c, b], [c, d], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [c, b], [c, d], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [c, b], [c, d], [d, c], [e, a], [e, a]]).
%% mrule([[a, b], [c, b], [c, d], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [c, b], [c, d], [d, c], [e, a], [e, c]]).
%% mrule([[a, b], [c, b], [c, d], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [c, b], [c, d], [d, e], [d, e], [e, a]]).
%% mrule([[a, b], [c, b], [c, d], [d, e], [e, a], [e, a]]).
%% mrule([[a, b], [c, b], [c, d], [d, e], [e, a], [e, b]]).
%% mrule([[a, b], [c, b], [c, d], [d, e], [e, a], [e, c]]).
%% mrule([[a, b], [c, b], [c, d], [d, e], [e, a], [e, d]]).
%% mrule([[a, b], [c, b], [c, d], [d, e], [e, f], [f, a]]).
%% mrule([[a, b], [c, b], [c, d], [d, e], [f, a], [f, e]]).
%% mrule([[a, b], [c, b], [c, d], [e, a], [e, a], [e, d]]).
%% mrule([[a, b], [c, b], [c, d], [e, a], [e, b], [e, d]]).
%% mrule([[a, b], [c, b], [c, d], [e, a], [e, c], [e, d]]).
%% mrule([[a, b], [c, b], [c, d], [e, a], [e, d], [e, d]]).
%% mrule([[a, b], [c, b], [c, d], [e, a], [e, f], [f, d]]).
%% mrule([[a, b], [c, b], [c, d], [e, a], [f, d], [f, e]]).
%% mrule([[a, b], [c, b], [c, d], [e, d], [e, f], [f, a]]).
%% mrule([[a, b], [c, b], [c, d], [e, d], [f, a], [f, e]]).
%% mrule([[a, b], [c, b], [c, d], [e, a], [e, f], [d, f]]).
%% mrule([[a, b], [c, b], [c, d], [e, a], [d, f], [f, e]]).
%% mrule([[a, b], [c, b], [d, a], [d, a], [d, a], [d, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, a], [d, c], [d, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, a], [d, e], [e, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, a], [e, a], [e, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, a], [e, c], [e, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, a], [e, c], [e, d]]).
%% mrule([[a, b], [c, b], [d, a], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, b], [e, b], [e, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, b], [e, c], [e, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [c, b], [d, a], [d, c], [d, c], [d, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, c], [d, e], [d, e]]).
%% mrule([[a, b], [c, b], [d, a], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [c, b], [d, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [c, b], [d, a], [d, c], [d, e], [e, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, c], [d, e], [e, d]]).
%% mrule([[a, b], [c, b], [d, a], [d, c], [e, a], [e, a]]).
%% mrule([[a, b], [c, b], [d, a], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [c, b], [d, a], [d, c], [e, a], [e, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [c, b], [d, a], [d, c], [e, b], [e, b]]).
%% mrule([[a, b], [c, b], [d, a], [d, c], [e, b], [e, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [c, b], [d, a], [d, c], [e, c], [e, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, c], [e, c], [e, d]]).
%% mrule([[a, b], [c, b], [d, a], [d, c], [e, d], [e, d]]).
%% mrule([[a, b], [c, b], [d, a], [d, e], [d, e], [e, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, e], [e, a], [e, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, e], [e, c], [e, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, e], [e, c], [e, d]]).
%% mrule([[a, b], [c, b], [d, a], [d, e], [e, f], [f, c]]).
%% mrule([[a, b], [c, b], [d, a], [d, e], [f, c], [f, e]]).
%% mrule([[a, b], [c, b], [d, a], [e, a], [e, c], [e, d]]).
%% mrule([[a, b], [c, b], [d, a], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [c, b], [d, a], [e, c], [e, c], [e, d]]).
%% mrule([[a, b], [c, b], [d, a], [e, c], [e, d], [e, d]]).
%% mrule([[a, b], [c, b], [d, a], [e, c], [e, f], [f, d]]).
%% mrule([[a, b], [c, b], [d, a], [e, c], [f, d], [f, e]]).
%% mrule([[a, b], [c, b], [d, a], [e, d], [e, f], [f, c]]).
%% mrule([[a, b], [c, b], [d, a], [e, d], [f, c], [f, e]]).
%% mrule([[a, b], [c, b], [d, b], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [c, b], [d, b], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [c, b], [d, b], [d, c], [e, a], [e, a]]).
%% mrule([[a, b], [c, b], [d, b], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [c, b], [d, b], [d, c], [e, a], [e, c]]).
%% mrule([[a, b], [c, b], [d, b], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [c, b], [d, b], [d, e], [e, a], [e, c]]).
%% mrule([[a, b], [c, b], [d, b], [e, a], [e, c], [e, d]]).
%% mrule([[a, b], [c, b], [d, c], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [c, b], [d, c], [d, c], [e, a], [e, a]]).
%% mrule([[a, b], [c, b], [d, c], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [c, b], [d, c], [d, c], [e, a], [e, c]]).
%% mrule([[a, b], [c, b], [d, c], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [c, b], [d, c], [d, e], [d, e], [e, a]]).
%% mrule([[a, b], [c, b], [d, c], [d, e], [e, a], [e, a]]).
%% mrule([[a, b], [c, b], [d, c], [d, e], [e, a], [e, b]]).
%% mrule([[a, b], [c, b], [d, c], [d, e], [e, a], [e, c]]).
%% mrule([[a, b], [c, b], [d, c], [d, e], [e, a], [e, d]]).
%% mrule([[a, b], [c, b], [d, c], [d, e], [e, f], [f, a]]).
%% mrule([[a, b], [c, b], [d, c], [d, e], [f, a], [f, e]]).
%% mrule([[a, b], [c, b], [d, c], [e, a], [e, a], [e, d]]).
%% mrule([[a, b], [c, b], [d, c], [e, a], [e, b], [e, d]]).
%% mrule([[a, b], [c, b], [d, c], [e, a], [e, c], [e, d]]).
%% mrule([[a, b], [c, b], [d, c], [e, a], [e, d], [e, d]]).
%% mrule([[a, b], [c, b], [d, c], [e, a], [e, f], [f, d]]).
%% mrule([[a, b], [c, b], [d, c], [e, a], [f, d], [f, e]]).
%% mrule([[a, b], [c, b], [d, c], [e, d], [e, f], [f, a]]).
%% mrule([[a, b], [c, b], [d, c], [e, d], [f, a], [f, e]]).
%% mrule([[a, b], [c, b], [d, e], [d, e], [e, a], [e, c]]).
%% mrule([[a, b], [c, b], [d, e], [d, f], [e, a], [f, c]]).
%% mrule([[a, b], [c, b], [d, e], [d, f], [e, c], [f, a]]).
%% mrule([[a, b], [c, b], [d, e], [e, a], [e, c], [e, d]]).
%% mrule([[a, b], [c, b], [d, e], [e, a], [f, c], [f, d]]).
%% mrule([[a, b], [c, b], [d, e], [e, c], [f, a], [f, d]]).
%% mrule([[a, b], [c, d], [c, d], [c, d], [d, a], [d, b]]).
%% mrule([[a, b], [c, d], [c, d], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [c, d], [c, d], [c, e], [d, b], [e, a]]).
%% mrule([[a, b], [c, d], [c, d], [c, e], [e, a], [e, b]]).
%% mrule([[a, b], [c, d], [c, d], [d, a], [d, a], [d, b]]).
%% mrule([[a, b], [c, d], [c, d], [d, a], [d, b], [d, b]]).
%% mrule([[a, b], [c, d], [c, d], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c, d], [c, d], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [c, d], [c, d], [d, a], [e, a], [e, b]]).
%% mrule([[a, b], [c, d], [c, d], [d, a], [e, b], [e, b]]).
%% mrule([[a, b], [c, d], [c, d], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [c, d], [c, d], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [c, d], [c, d], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [c, d], [c, d], [d, b], [e, a], [e, a]]).
%% mrule([[a, b], [c, d], [c, d], [d, b], [e, a], [e, b]]).
%% mrule([[a, b], [c, d], [c, d], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [c, d], [c, d], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [c, d], [c, d], [d, e], [e, a], [e, b]]).
%% mrule([[a, b], [c, d], [c, d], [e, a], [e, b], [e, c]]).
%% mrule([[a, b], [c, d], [c, d], [e, a], [e, b], [e, d]]).
%% mrule([[a, b], [c, d], [c, e], [c, e], [d, a], [d, b]]).
%% mrule([[a, b], [c, d], [c, e], [c, e], [d, a], [e, b]]).
%% mrule([[a, b], [c, d], [c, e], [c, e], [d, b], [e, a]]).
%% mrule([[a, b], [c, d], [c, e], [d, a], [d, a], [e, b]]).
%% mrule([[a, b], [c, d], [c, e], [d, a], [d, b], [d, e]]).
%% mrule([[a, b], [c, d], [c, e], [d, a], [d, b], [e, a]]).
%% mrule([[a, b], [c, d], [c, e], [d, a], [d, b], [e, b]]).
%% mrule([[a, b], [c, d], [c, e], [d, a], [d, b], [e, c]]).
%% mrule([[a, b], [c, d], [c, e], [d, a], [d, b], [e, d]]).
%% mrule([[a, b], [c, d], [c, e], [d, a], [d, c], [e, b]]).
%% mrule([[a, b], [c, d], [c, e], [d, a], [d, e], [e, b]]).
%% mrule([[a, b], [c, d], [c, e], [d, a], [e, a], [e, b]]).
%% mrule([[a, b], [c, d], [c, e], [d, a], [e, b], [e, b]]).
%% mrule([[a, b], [c, d], [c, e], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [c, d], [c, e], [d, a], [e, b], [e, d]]).
%% mrule([[a, b], [c, d], [c, e], [d, a], [e, f], [f, b]]).
%% mrule([[a, b], [c, d], [c, e], [d, a], [f, b], [f, e]]).
%% mrule([[a, b], [c, d], [c, e], [d, b], [d, b], [e, a]]).
%% mrule([[a, b], [c, d], [c, e], [d, b], [d, c], [e, a]]).
%% mrule([[a, b], [c, d], [c, e], [d, b], [d, e], [e, a]]).
%% mrule([[a, b], [c, d], [c, e], [d, b], [e, a], [e, a]]).
%% mrule([[a, b], [c, d], [c, e], [d, b], [e, a], [e, b]]).
%% mrule([[a, b], [c, d], [c, e], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [c, d], [c, e], [d, b], [e, a], [e, d]]).
%% mrule([[a, b], [c, d], [c, e], [d, b], [e, f], [f, a]]).
%% mrule([[a, b], [c, d], [c, e], [d, b], [f, a], [f, e]]).
%% mrule([[a, b], [c, d], [c, e], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [c, d], [c, e], [d, e], [e, a], [e, b]]).
%% mrule([[a, b], [c, d], [c, e], [d, f], [e, a], [f, b]]).
%% mrule([[a, b], [c, d], [c, e], [d, f], [e, b], [f, a]]).
%% mrule([[a, b], [c, d], [c, e], [e, a], [e, b], [e, d]]).
%% mrule([[a, b], [c, d], [c, e], [e, a], [f, b], [f, d]]).
%% mrule([[a, b], [c, d], [c, e], [e, b], [f, a], [f, d]]).
%% mrule([[a, b], [c, d], [d, a], [d, a], [d, b], [d, c]]).
%% mrule([[a, b], [c, d], [d, a], [d, a], [e, b], [e, c]]).
%% mrule([[a, b], [c, d], [d, a], [d, b], [d, b], [d, c]]).
%% mrule([[a, b], [c, d], [d, a], [d, b], [d, c], [d, c]]).
%% mrule([[a, b], [c, d], [d, a], [d, b], [d, e], [e, c]]).
%% mrule([[a, b], [c, d], [d, a], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [c, d], [d, a], [d, b], [e, b], [e, c]]).
%% mrule([[a, b], [c, d], [d, a], [d, b], [e, c], [e, c]]).
%% mrule([[a, b], [c, d], [d, a], [d, b], [e, c], [e, d]]).
%% mrule([[a, b], [c, d], [d, a], [d, c], [d, e], [e, b]]).
%% mrule([[a, b], [c, d], [d, a], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [c, d], [d, a], [d, c], [e, b], [e, b]]).
%% mrule([[a, b], [c, d], [d, a], [d, c], [e, b], [e, c]]).
%% mrule([[a, b], [c, d], [d, a], [d, c], [e, b], [e, d]]).
%% mrule([[a, b], [c, d], [d, a], [d, e], [e, b], [e, c]]).
%% mrule([[a, b], [c, d], [d, a], [e, a], [e, b], [e, c]]).
%% mrule([[a, b], [c, d], [d, a], [e, b], [e, b], [e, c]]).
%% mrule([[a, b], [c, d], [d, a], [e, b], [e, c], [e, c]]).
%% mrule([[a, b], [c, d], [d, a], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [c, d], [d, a], [e, b], [e, f], [f, c]]).
%% mrule([[a, b], [c, d], [d, a], [e, b], [f, c], [f, e]]).
%% mrule([[a, b], [c, d], [d, a], [e, c], [e, f], [f, b]]).
%% mrule([[a, b], [c, d], [d, a], [e, c], [f, b], [f, e]]).
%% mrule([[a, b], [c, d], [d, b], [d, b], [e, a], [e, c]]).
%% mrule([[a, b], [c, d], [d, b], [d, c], [d, e], [e, a]]).
%% mrule([[a, b], [c, d], [d, b], [d, c], [e, a], [e, a]]).
%% mrule([[a, b], [c, d], [d, b], [d, c], [e, a], [e, b]]).
%% mrule([[a, b], [c, d], [d, b], [d, c], [e, a], [e, c]]).
%% mrule([[a, b], [c, d], [d, b], [d, c], [e, a], [e, d]]).
%% mrule([[a, b], [c, d], [d, b], [d, e], [e, a], [e, c]]).
%% mrule([[a, b], [c, d], [d, b], [e, a], [e, a], [e, c]]).
%% mrule([[a, b], [c, d], [d, b], [e, a], [e, b], [e, c]]).
%% mrule([[a, b], [c, d], [d, b], [e, a], [e, c], [e, c]]).
%% mrule([[a, b], [c, d], [d, b], [e, a], [e, c], [e, d]]).
%% mrule([[a, b], [c, d], [d, b], [e, a], [e, f], [f, c]]).
%% mrule([[a, b], [c, d], [d, b], [e, a], [f, c], [f, e]]).
%% mrule([[a, b], [c, d], [d, b], [e, c], [e, f], [f, a]]).
%% mrule([[a, b], [c, d], [d, b], [e, c], [f, a], [f, e]]).
%% mrule([[a, b], [c, d], [d, c], [d, e], [e, a], [e, b]]).
%% mrule([[a, b], [c, d], [d, c], [e, a], [e, b], [e, c]]).
%% mrule([[a, b], [c, d], [d, c], [e, a], [e, b], [e, d]]).
%% mrule([[a, b], [c, d], [d, e], [e, a], [e, b], [e, c]]).
%% mrule([[a, b], [c, d], [d, e], [e, a], [f, b], [f, c]]).
%% mrule([[a, b], [c, d], [d, e], [e, b], [f, a], [f, c]]).
%% mrule([[a, b], [c, d], [e, a], [e, b], [e, c], [e, d]]).
%% mrule([[a, b], [c, d], [e, a], [e, c], [f, b], [f, d]]).
%% mrule([[a, b], [c, d], [e, a], [e, d], [f, b], [f, c]]).
%% mrule([[a, b], [c, d], [e, b], [e, c], [f, a], [f, d]]).
%% mrule([[a, b], [c, d], [e, b], [e, d], [f, a], [f, c]]).
%% mrule([[a, b], [c, d], [e, a], [e, c], [d, f], [f, b]]).
%% mrule([[a, b], [c, d], [e, b], [e, c], [d, f], [f, a]]).