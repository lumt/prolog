% 1) define mysort that sorts and removes duplicates

/* 
	1. Define and test the following predicates according to the 
	specification given below:
	a) mysort(L,SL)
	SL is list L sorted and all duplicates removed. So, for example:
	| ?- mysort([3,2,4,1,5,3,2], [1,2,3,4,5]). gets the answer yes.
	| ?- mysort([22, 11, 22, 10], X). gets the answer X = [10,11,22].
	Use setof and member. Prolog has an inbuilt predicate sort.
	Do not use it for this exercise.
*/

mysort(L, SL):-
	setof(X, member(X, L), SL).

/*
	b) rev(L, RevL)
	RevL is list L with the order of its elements reversed. So, for example:
	|?- rev([1,2,3],R). gets the answer R=[3,2,1].
	|?- rev([1,pears,[],[2,3]],R). gets the answer R=[[2,3],[],pears,1]
	Prolog has an inbuilt predicate reverse. Do not use it for this exercise.
	For this exercise give two different definitions for rev,
	one non-tail-recursive and one tail-recursive.
*/

revA([], A, A).

revA([H|T], A, RevL):-
	revA(T, [H|A], RevL).

rev(L, RevL):-
	revA(L, [], RevL).

%% Solution: (Tail recursive)
%% rev([],[]).
%% rev([H|T],R):- 
%% 	rev(T,RT),
%% 	append(RT,[H],R).

/* 
c) followedBy(X,Y,L)
	X is followed by Y on list L. So, for example:
	| ?- followedBy(4,6,[1,3,4,6,7]). gets the answer yes.
	| ?- followedBy(4,X,[1,3,4,6,7]). gets the answer X = 6.
	| ?- followedBy(X,Y,[1,3,4,6,7]). gets the answers
	X = 1, Y = 3 ? ; X = 3, Y = 4 ? ;
	X = 4, Y = 6 ? ; X = 6, Y = 7 ? .
	Here are some other queries you could try:
	| ?- followedBy(1,2,[X, Y, Z]). gets the answers
	X = 1, Y = 2 ? ; Y = 1, Z = 2.
	| ?- followedBy(1,2,X). gets the answers
	X = [1,2|_A] ;
	X = [_A,1,2|_B] ;
	X = [_A,_B,1,2|_C] ;
	X = [_A,_B,_C,1,2|_D] etc.
*/

followedBy(X,Y,[X,Y|_]).
followedBy(X,Y, [_|T]):-
	followedBy(X,Y, T).

/*
d) nextTo(X,Y,L)
	X and Y are next to one another on list L. So, for example:
	nextTo(3,6,[12,6,3,1,7]) and nextTo(6,3,[12,6,3,1,7]) 
	both get the answer yes.
*/

nextTo(X, Y, [X, Y|_]).
nextTo(X, Y, [Y, X|_]).
nextTo(X, Y, [_|T]):-
	nextTo(X, Y, T).

/*
e) sumList(L,S)
	S is the sum of all integers on list L. 
	Assume L is a list of positive or negative integers. 
	So, for example:
	?- sumList([1,3,4,6], S). gets the answers S=14.
*/

sumList([], 0).
sumList([H|T], S):-
	sumList(T, Ss),
	S is Ss + H.

/*
f) Write Prolog clauses for the relation last(E,L) 
	that finds the last element E of a list L.
*/

last(E, L):-
	rev(L, [E|_]).

/*
or:
last(X, [X]).
last(X, [H|T]):-
	last(X, T).
*/

/*
2) Path
*/

edge(a,b).
edge(a,e).
edge(b,c).
edge(e,f).
edge(f,c).
edge(c,d).
edge(f,d).

path(X, Y, [X,Y]):-
	edge(X, Y).

path(X,Y,[X|T]):-
	edge(X, Z),
	path(Z, Y, T).

/*
3) Max
*/

% base (copy).
max(X, [X]).

max(M, [H1, H2|T]):-
	H1 =< H2,
	max(M, [H2|T]).

max(M, [H1, H2|T]):-
	H1 > H2,
	max(M, [H1|T]).


/* 
4) max of all
*/

maxAll(MAX, [L]):-
	max(MAX, L).

maxAll(MAX, [H1, H2|Tl]):-
	max(H1_max, H1),
	max(H2_max, H2),
	H1_max =< H2_max,
	maxAll(MAX, [H2|Tl]).

maxAll(MAX, [H1, H2|Tl]):-
	max(H1_max, H1),
	max(H2_max, H2),
	H1_max > H2_max,
	maxAll(MAX, [H1|Tl]).








