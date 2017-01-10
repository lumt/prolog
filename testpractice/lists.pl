% 1)

remove_item(_, [], []).
remove_item(E, [E|T], NewL):-
	remove_item(E, T, NewL).

remove_item(E, [X|T], [X|T1]):-
	E \= X,
	remove_item(E, T, T1).

% 2)

drop_items(L, 0, L).
drop_items([_|T], N, NewL):-
	N1 is N - 1,
	drop_items(T, N1, NewL).

% 3)

drop_more_items(L, N, []):-
	length(L, Llen),
	Llen < N.

drop_more_items(L, N, R):-
	append(L1,L2,L),		%gets two lists L1 , L2 and see if they can make L
	length(L1,N),			%L1 must be size N (drops first N)
	remove_all(L1,L2,R).	%removes all elements in L1 from L2 to make R

remove_all([],L2,L2).
remove_all([X|L],L2,R):-
	remove_item(X,L2,NL2),
	remove_all(L,NL2,R).

% 4)

count([], []).
count([X|T], [(X,C)|T1]):-
	appears(X, [X|T], C),
	remove_item(X, [X|T], L),
	count(L, T1).

appears(_, [], 0).
appears(E, [E|T], N):-
	appears(E, T, N1),
	N is N1 + 1.
appears(E, [X|T], N):-
	E \= X,
	appears(E, T, N).

%%%% OR %%%

count1(X, C):-
	setof((E, N), (member(E,X), occurences(E, X, N)), C).

occurences(E, L, N):-
	remove_item(E, L, L1),
	length(L, LenL),
	length(L1, Subtract),
	N is LenL - Subtract.