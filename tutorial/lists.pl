
/*******************/
% to denote that X belongs / is a member of given list.
belongsTo(X, [X|_]).
belongsTo(X, [_|T]):-
	belongsTo(X, T).

/*******************/
% concatenate (append) lists.
concat([], L2, L2).
concat([H1|T1], L2, [H1|T3]):-
	concat(T1, L2, T3).

/*******************/
% partitioning
% even_odd(All, Even, Odd).

% base case
even_odd([], [], []).

% Even case
even_odd([N|TAll], [N|TEven], Odd):-
	N mod 2 =:= 0,
	even_odd(TAll, TEven, Odd).

% Odd case
even_odd([N|TAll], Even, [N|TOdd]):-
	N mod 2 =:= 1,
	even_odd(TAll, Even, TOdd).

/*******************/
% len of list
len([], 0).
len([_|T], N):-
	len(T, N1),
	N is N1 + 1.

/*******************/
% list_double
% every element in L2 is the double of L1

list_double([],[]).
list_double([H1|T1], [H1,H1|T2]):-
	list_double(T1, T2).

/*******************/
% list_avg
% A is average of all elements in L

sum([], 0).
sum([H|T], S):-
	sum(T, S1),
	S is H + S1.

list_avg([], 0).
list_avg(L, A):-
	sum(L, S),
	len(L, N),
	A is S/N.

/*******************/
% access_element(N, L, X)
% X is the Nth element in L

access_element(0, [X|_], X).
access_element(N, [_|T], X):-
	N1 is N - 1,
	access_element(N1, T, X).

/*******************/
% remove(X, L, Rest)
% Rest is list L from which every element = X is removed

remove(_, [], []).

% X is head
remove(X, [X|T2], T3):-
	remove(X, T2, T3).

% X is not head
remove(X, [Y|T2], [Y|T3]):-
	remove(X, T2, T3).

/*******************/
% a2b(L1, L2)
% every occurence of a in L1 occurs as b in L2,
% everything else is identical

a2b([], []).

a2b([a|Ta], [b|Tb]):-
	a2b(Ta, Tb).

a2b([X|Ta], [X|Tb]):-
	a2b(Ta, Tb).


