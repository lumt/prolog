/*	Common predicates in Prolog	*/

/*	set(L1, L2):
	to mean L2 removes duplicates in L1
	*/
set([], []).
set([H|T1], [H|T2]):-
	\+ member(H, T1),
	set(T1, T2).
set([H|T1], Out):-
	member(H, T1),
	set(T1, Out).

/*	difference(L1, L2, L)
	to mean L consists of all the elements in L1 that are not in L2.
	Assume L1, L2 grounded in call.
	
	Test: 
	difference([1,1,2,3, 5, 5], [1,3,2, 3,4], L).
	L = [5,5].

	difference([a, c], [], L).
	L = [a,c].
	*/

difference([], _, []).

difference([H|T], L2, L):-
	member(H, L2),
	difference(T, L2, L).

difference([H|T],L2, [H|T2]):-
	\+member(H, L2),
	difference(T, L2, T2).

% OR
diff(L1, L2, L):-
	findall(E, (member(E, L1), \+ member(E, L2)), L).

/*	path(X, Y, P)
	P is a list of path from node X to Y
	*/

path(X, Y, [X, Y]):-
	edge(X, Y).

path(X, Y, [X|T]):-
	edge(X, Z),
	path(Z, Y, T).


/*	max(E, L)
	to mean E is the max element in list L
	*/

% base case copies E into single element list
max(E, [E]).

% element H1 =< H2, remove H1 from list
max(E, [H1, H2|T]):-
	H1 =< H2,
	max(E, [H2|T]).

% element H1 > H2, remove H2 from list
max(E, [H1, H2|T]):-
	H1 > H2,
	max(E, [H1|T]).

/*	rev(L, RevL)
	to mean RevL is the list L which is reversed
	*/

rev([], []).
rev([H|T], RevL):-
	rev(T, RT),
	append(RT, [H], RevL).


/*
	p16
	drop every N'th element from list
	?- drop([a,b,c,d,e,f,g,h,i,k],3,X).
	X = [a,b,d,e,g,h,k]
	*/

drop(L1,N,L2):-
	drop(L1,N,L2,1).

% drop(L1, N, L2, K) where K is counter for N and L2 is the modified L1
drop([], _, [], _).
drop([_|T], N, L2, N):-
	drop(T, N, L2, 1).
drop([H|T], N, [H|T2], M):-
	M < N,
	M1 is M + 1,
	drop(T, N, T2, M1).

/*
	p18
	extract slice from a list
	Example:
	?- slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
	X = [c,d,e,f,g]
*/

slice(L1, I, K, L):-
	slice(L1, I, K, 1, L).

slice(_, _, K, C, []):-
	C > K.

slice([_|L1], I, K, C, L):-
	C < I, C > 0,
	C1 is C + 1,
	slice(L1, I, K, C1, L).

slice([H|L1], I, K, C, [H|L]):-
	C >= I, C =< K,
	C1 is C + 1,
	slice(L1, I, K, C1, L).


/*
	p20
	remove K'th element from list
	?- remove_at(X,[a,b,c,d],2,R).
	X = b
	R = [a,c,d]
*/

remove_at(X, [X|T], 1, T).
remove_at(X, [H|T], N, [H|T1]):-
	N > 1,
	N1 is N - 1,
	remove_at(X, T, N1, T1).
