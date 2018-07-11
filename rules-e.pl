metarule([P, Q],([P, A] :- [[Q, A]])).
metarule([P, Q],([P, A, B] :- [[Q, B, A]])).
metarule([P, Q, R],([P, A, B] :- [[Q, A], [R, B]])):-freeze(Q,freeze(R,Q\=R)).
metarule([P, Q, R],([P, A, B] :- [[Q, A, C], [R, C, B]])).
metarule([P, Q, R],([P, A] :- [[Q, A, B], [R, A, B]])):-freeze(Q,freeze(R,Q\=R)).