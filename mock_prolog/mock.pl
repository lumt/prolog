% child_mother(C, M) means C is a child of mother M.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

child_mother(amy, mary).
child_mother(arthur, mary).
child_mother(angel, jane).
child_mother(anton, rita).
child_mother(alan, rita).
child_mother(axel, susan).
child_mother(ann, tina).

% age(C, A) means C is of age A.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

age(amy, 6).
age(arthur, 15).
age(angel, 16).
age(anton, 4).
age(alan, 8).
age(axel, 16).
age(ann, 4).

% employed(X) means X is employed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

employed(susan).
employed(rita).

/*
Please ensure your predicates are spelled correctly.
Copy and paste from the following:

ecb(M)
mother_of_the_youngest(M)
mothers_of_young(LM)

merge(L1, L2, L)
findElement(N, L, E)
*/

% 1) i)
/*	ecb(M)
	to mean M is a mother who is entitled to child benefit.
	A mother is entitled to child benefit if she has a child who is
	14 years old or younger, or if she is not employed and has a child 
	who is older than 14 but not older than 16.

	Test: ecb(M).
	M = mary ;
	M = jane ;
	M = rita ;
	M = tina ;
	*/


:- use_module(library(lists)).

ecb(M):-
	child_mother(C, M),
	age(C, A),
	(A =< 14 ; A > 14, A =< 16, \+ employed(M)).


% 1) ii)
/*	mother_of_the_youngest(M)
	to mean M is the mother of the youngest child. If there is more than one
	child with the same youngest age, then M should be the mother whose name
	comes alphabetically before the others.

	Test: mother_of_the_youngest(M)
	Should succeed with M=rita.
	*/

mother_of_the_youngest(M):-
	setof(A-Mum, C^(child_mother(C, Mum), age(C, A)), [A-M|_]).

% solution ???????
mum_of_youngest(M):-
	setof(A-Mum, C^(child_mother(C, Mum), age(C, A)), L),
	L=[A-M| Rest].


% 1) iii)
/*	mother_of_young(LM)
	to mean LM is the list of all mothers who have a child of 10 or younger.
	LM should be sorted and have no repetition. If there are no mothers with
	children of 10 or younger then the query mothers_of_young(LM) should fail.

	Test: mothers_of_young(LM).
	Should return LM = [mary, rita, tina].
	*/

mothers_of_young(LM):-
	setof(M, C^A^(child_mother(C, M), age(C, A), A =< 10), LM).

% 2) i)
/*	merge(L1, L2, L)
	You can assume that in any call to merge the first two arguments
	(corresponding to L1 and L2) are sorted lists of integers.
	L should then be an ordered list resulting from merging the elements of
	L1 and L2.

	TestL: merge([1,3,5,5], [2,3,4,6], L).
	Should return L=[1,2,3,3,4,5,5,6].
	*/

merge([], [A], [A]).
merge([A], [], [A]).

% if H1 < H2
merge([H1|T1], [H2|T2], L):-
	H1 < H2,
	merge(T1, [H2|T2], L1),
	append([H1], L1, L).

% if H1 >= H2
merge([H1|T1], [H2|T2], L):-
	H1 >= H2,
	merge([H1|T1], T2, L1),
	append([H2], L1, L).

% 2) ii)
/*	findElement(N, L, E)
	to mean E is the Nth element in list L. You can assume that in any call to
	findElement N and L are given. If N is less than 1 or is greater than the
	length of L then the query should fail.

	Test: findElement(3, [1,3,5,5], E).
	Should return E=5.
	*/

% decrement N until 0, then return head of L
findElement(0, [E|_], E).
findElement(N, [_|T], E):-
	N1 is N - 1,
	findElement(N1, T, E).

% solution
findE(N, L, E):-
	N > 0,
	length(L, M),
	M >= N,
	append(L1, L2, L),
	P is N - 1,
	length(L1, P),
	L2 = [E|_].








