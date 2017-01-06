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

difference([H|T], L2, L):-
	\+ member(H, L2),
	difference(T, L2, L1),
	append([H], L1, L).

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



