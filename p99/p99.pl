%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(lists)).
/*	
	P01
	Find the last element of a list.
	Example:
	?- my_last(X,[a,b,c,d]).
	X = d
*/

my_last(X, [X]).
my_last(X,[_|T]):-
	my_last(X, T).

/*
	P02
	 Find the last but one element of a list.
	(the one before the last).
*/

last_but_one(X, [X,_]).
last_but_one(X, [_, Y |T]):-
	last_but_one(X, [Y|T]).

/*
	P03
	Find the K'th element of a list.
	The first element in the list is number 1.
	Example:
	?- element_at(X,[a,b,c,d,e],3).
	X = c
*/

element_at(X, [X|_], 1).
element_at(X, [_|T], K):-
	K > 1,
	K1 is K - 1,
	element_at(X, T, K1).

/*
	P04
	Find the number of elements of a list.
	my_len(Len, List).
*/

my_len(0, []).
my_len(Len, [_|T]):-
	my_len(Len1, T),
	Len is Len1 + 1.

/*
	P05
	Reverse a list.
	rev1(L, Rev).
*/

my_rev([], []).
my_rev([H|T], Rev):-
	my_rev(T, Rev1),
	append(Rev1, [H], Rev).


/*
	P06
	Find out whether a list is a palindrome.
	A palindrome can be read forward or backward; e.g. [x,a,m,a,x].
*/

palindrome(L):-
	rev(L, L).

/*
	P07
	Flatten a nested list structure.
	Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

	Example:
	?- myflatten([a, [b, [c, d], e]], X).
	X = [a, b, c, d, e]
*/

myflatten([], []).

myflatten([H|T], Flat):-
	\+ is_list(H),
	myflatten(T, Flat1),
	append([H], Flat1, Flat).

myflatten([H|T], Flat):-
	is_list(H),
	myflatten(H, FlatH),
	myflatten(T, FlatT),
	append(FlatH, FlatT, Flat).

% solution

my_flatten([],[]).
my_flatten(X,[X]):-
	\+ is_list(X).

my_flatten([X|Xs],Zs):-
	my_flatten(X,Y),
	my_flatten(Xs,Ys),
	append(Y,Ys,Zs).


/*	
	P08
	Eliminate consecutive duplicates of list elements.
	If a list contains repeated elements they should be replaced with a single
	copy of the element. The order of the elements should not be changed.
	
	Example:
	?- compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
	X = [a,b,c,a,d,e]
*/

compress([], []).
compress([H], [H]).

compress([H, H|T], X):-
	compress([H|T], X).

compress([H1, H2|T], [H1|T2]):-
	H1 \= H2,
	compress([H2|T], T2).

/*
	P09
	Pack consecutive duplicates of list elements into sublists.
	If a list contains repeated elements they should be placed in separate
	sublists.
	
	Example:
	?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
	X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
*/
pack([], []).

pack([H|T], [H1|T1]):-
	transfer(H, T, XT, H1),
	pack(XT, T1).

% transfer(X,Xs,Ys,Z) Ys is the list that remains from the list Xs
%    when all leading copies of X are removed and transfered to Z

transfer(X,[],[],[X]).
transfer(X,[Y|Ys],[Y|Ys],[X]):-
	X \= Y.
transfer(X,[X|Xs],Ys,[X|Zs]):-
	transfer(X,Xs,Ys,Zs).
/*
	P10
	Run-length encoding of a list.
	Use the result of problem P09 to implement the so-called run-length
	encoding data compression method. Consecutive duplicates of elements are
	encoded as terms [N,E] where N is the number of duplicates of the element
	E.

	Example:
	?- encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
	X = [[4,a],[1,b],[2,c],[2,a],[1,d][4,e]]
*/

encode(L, Out):-
	pack(L, Pck),
	findall([Len, X], (member(P, Pck),
						P = [X|_],
						length(P, Len)), Out).

% OR:
encode1(L1,L2):-
	pack(L1,L),
	transform(L,L2).

transform([],[]).
transform([[X|Xs]|Ys],[[N,X]|Zs]):-
	length([X|Xs],N),
	transform(Ys,Zs).


/*
	P11
	(*) Modified run-length encoding.
	Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as [N,E] terms.

	Example:
	?- encode_mod([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
	X = [[4,a],b,[2,c],[2,a],d,[4,e]]
*/

encode_mod(L, Out):-
	pack(L, L1),
	transform_mod(L1, Out).

transform_mod([], []).
transform_mod([H|T], [[N, X]|Z]):-
	length(H, N),
	N > 1,
	H = [X|_],
	transform_mod(T, Z).

transform_mod([H|T], [X|Z]):-
	length(H, N),
	N = 1,
	H = [X|_],
	transform_mod(T, Z).

/*
	P12
	Decode a run-length encoded list.
	Given a run-length code list generated as specified in problem P11.
	Construct its uncompressed version.

	decode([[4,a],b,[2,c],[2,a],d,[4,e]], X).
	X = [a,a,a,a,b,c,c,a,a,d,e,e,e,e].
*/

decode([], []).

decode([[N,X]|T], [X|Xt]):-
	N > 1,
	N1 is N - 1,
	decode([[N1,X]|T], Xt).

decode([[1,X]|T], [X|Xt]):-
	decode(T, Xt).

decode([H|T], [H|Xt]):-
	\+ is_list(H),
	decode(T, Xt).


/*	P13


