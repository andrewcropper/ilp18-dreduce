metarule([P, Q],([P, A] :- [[Q, A]])). %% $P(A) \leftarrow Q(A)$\\ % [[a],[a]],
metarule([P, Q, R],([P, A] :- [[Q, A], [R, A]])):-freeze(Q,freeze(R,Q\=R)). %% $P(A) \leftarrow Q(A),R(A)$\\ % [[a],[a],[a]],
metarule([P, Q, R],([P, A] :- [[Q, A,B],[R, B]])). %% $P(A) \leftarrow Q(B),R(A,B)$\\ % [[a],[b],[b,a]],
metarule([P, Q, R],([P, A] :- [[Q, A,B], [R,A,B]])):-freeze(Q,freeze(R,Q\=R)). %% $P(A) \leftarrow Q(A,B),R(A,B)$\\ %[[a],[b,a],[b,a]],
metarule([P, Q],([P, A, B] :- [[Q, B, A]])). %% $P(A,B) \leftarrow Q(B,A)$\\ % [[a,b],[b,a]],
metarule([P, Q, R],([P, A, B] :- [[Q, A], [R, B]])). %% $P(A,B) \leftarrow Q(A),R(B)$\\ % [[a,b],[a],[b]],
metarule([P, Q, R],([P, A, B] :- [[Q, A], [R, A, B]])). %% $P(A,B) \leftarrow Q(A),R(A,B)$\\
metarule([P, Q, R],([P, A, B] :- [[Q, A, B], [R, A, B]])):-freeze(Q,freeze(R,Q\=R)). %% $P(A,B) \leftarrow Q(A,B),R(A,B)$\\ % [[a,b],[a,b],[a,b]],
metarule([P, Q, R],([P, A, B] :- [[Q, A, C], [R, C,B]])). %% $P(A,B) \leftarrow Q(A,C),R(C,B)$\\ % [[a,b],[a,c],[b,c]],