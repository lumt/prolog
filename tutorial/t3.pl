/* redo of tut3
   for revision
   4/1/17
   */

% 1.
% mysort sorts and removes duplicates
mysort(L, SL):-
	setof(X, member(X, L), SL).

% 2.
% rev(L, RevL)
% RevL the list L which is reversed

rev([], []).
rev([H|T], RevL):-
	rev(T, RT),
	append(RT, [H], RevL).


% 3.
% followedBy(X, Y, L)
% X is followed by Y on list L

followedBy(X, Y, [X, Y|_]).
followedBy(X, Y, [_|T]):-
	followedBy(X, Y, T).

% 4. 
% nextTo(X, Y, L)
% X and Y are next to each other on list L.

nextTo(X, Y, L):-
	followedBy(X, Y, L),
	rev(L, RevL),
	followedBy(Y, X, RevL).


% or:

next2(X, Y, [X, Y|_]).
next2(X, Y, [Y, X|_]).
next2(X, Y, [_|T]):-
	next2(X, Y, T).

% 5.
% sumList(L, S)
% S is sum of all integers on list L.

sumList([], 0).
sumList([H|T], S):-
	sumList(T, S1),
	S is S1 + H.

% 6.
% last(E, L)
% E is the last element of list L.

last([], []).
last(E, [E]).
last(E, [_|T]):-
	last(E, T).

% or:

last1(E, L):-
	rev(L, [E|_]).


%%%%%%%%%%%%%%%%%%%%%%%
% graph

edge(a, b).
edge(a, e).
edge(b, c).
edge(e, f).
edge(f, c).
edge(c, d).
edge(f, d).


% path(X, Y)
% determines if there is path from X to Y
path(X, Y):-
	edge(X, Y).
path(X, Y):-
	edge(X, Z),
	path(Z, Y).

% path(X, Y, P)
% P is path from node X to Y
path(X, Y, [X, Y]):-
	edge(X, Y).

path(X, Y, [X|T]):-
	edge(X, Z),
	path(Z, Y, T).

% max(E, L)
% determines maximum element E of list L.

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



% or (much less efficient):

% base
max1(E, [E]).

% found bigger or equal, replace E 
max1(E, [H|T]):-
	max(E1, T),
	H >= E1,
	E is H.

% found smaller
max1(E, [H|T]):-
	max(E1, T),
	H < E1,
	E is E1.


% max_of_all(E, Ls)
% E is max element of all lists Ls

% base, 1 list
max_of_all(E, [List]):-
	max(E, List).

% H1's max E1 >= E2 of H2
% remove H2 from list
max_of_all(E, [H1, H2|TL]):-
	max(E1, H1),
	max(E2, H2), 
	E1 >= E2,
	max_of_all(E, [H1| TL]).

% H1's max E1 < E2
% remove H1 from list
max_of_all(E, [H1, H2|TL]):-
	max(E1, H1),
	max(E2, H2),
	E1 < E2,
	max_of_all(E, [H2| TL]).







