% 1)

subList([], _).
subList([H|T], L2):-
	member(H, L2),
	subList(T, L2).

% 2)

difference([], _, []).

difference([H|T], L2, L):-
	member(H, L2),
	difference(T, L2, L).

difference([H|T],L2, [H|T2]):-
	\+member(H, L2),
	difference(T, L2, T2).

% or simply
diff(L1, L2, L):-
	findall(E, (member(E, L1), \+member(E, L2)), L).

% 3)

sift([], _, []).
sift([H|T], N, [H|T2]):-
	H =< N,
	sift(T, N, T2).

sift([H|T], N, R):-
	H > N,
	sift(T, N, R).

% 4)

common(L1, L2, I):-
	findall(E, (member(E,L1), member(E, L2)), X),
	length(X, Xlen),
	(Xlen > 0, setof(E1, member(E1, X), I)).

common(L1, L2, []):-
	findall(E, (member(E,L1), member(E, L2)), X),
	length(X, Xlen),
	Xlen == 0.


% 5)

delete([], []).
delete([X], [X]).
delete([X, _|T], [X|Xs]):-
	delete(T, Xs).

% 6)

process(L1, L2, C, I):-
	findall((Name, Num, Info), (member((Name, Num), L1),
								member((Name, Num, Info), L2)), C),
	findall((Name, Num, Info), (member((Name, Num, Info), L2),
								\+member((Name, Num), L1)), I).

% 7)

split(L2, 0, [], L2).
split([H|T], N, [H|T1], L2):-
	N1 is N - 1,
	split(T, N1, T1, L2).

% 8)

get_idx(E, [E|_], 1).
get_idx(E, [X|T], N):-
	member(E, T),
	E \= X,
	get_idx(E, T, N1),
	N is N1 + 1.

drop(L, N, R):-
	findall(E, (member(E,L),
				get_idx(E, L, M),
				M mod N =\= 0), R).

% 9)

enrolment(L, S, D):-
	member((D, Students), L),
	member(S, Students).

% 10)

student_list(L, MEng, MSc):-
	findall(S, (member((meng, Ss), L), member(S, Ss)), MEng),
	findall(S, (member((msc, Ss), L), member(S, Ss)), MSc).


