% This exercise is not assessed and testing on LabTS is not available. It is
% not possible (and needed) to submit any files on CATE. However, please feel
% free to use gitlab to track your source files. A sample solution will be made
% available on CATE after the deadline.

arc(a,b).
arc(a,c).
arc(a,d).
arc(b,e).
arc(c,g).
arc(c,f).
arc(d,c).
arc(d,f).
arc(g,e).
arc(g,f).

/*	deadend(N)
	to mean node N is reachable (i.e. can be reached by some other node)
	but cannot reach any other node (i.e. has no arcs leaving it).
	
	Test:
	deadEnd(N).
*/

deadend(N):-
	arc(_, N),
	\+ arc(N, _).

/*	hubs(H)
	to mean H is the set of hub nodes, i.e. those nodes N that are part of at
	least 3 arcs, each arc either entering N or leaving N. The result H should
	have no repetition, and should be alphabetically ordered.

	Test:
	hubs(H).
	H should be [a,c,d,f,g].
*/

is_hub(N):-
	(arc(N, X); arc(X, N)),
	(arc(N, Y); arc(Y, N)),
	(arc(N, Z); arc(Z, N)),
	Y \= Z, X \= Z, Z \= Y, X \= Y.

hubs(H):-
	setof(N, is_hub(N), H).

/*	ideal(N)
	to mean N is a node from which there is a path to all other nodes in the
	graph. You can assume every node in the graph is connected, i.e. is the
	source or destination of an arc.

	Test:
	idea(N).
	The answer may be repeated.
*/

% all nodes in the graph
get_all_nodes(L):-
	setof(X, N^(arc(X, N); arc(N, X)), L).

% possible places you can go from N
destinations(N, L):-
	setof(X, path(N, X), L).

% path
path(X, Y):-
	arc(X, Y).

path(X, Y):-
	arc(X, Z),
	path(Z, Y).

% ideal(N) gets list of all nodes within the graph
% then gets list of all nodes you can go to
% see if node can get there
ideal(N):-
	get_all_nodes(L_all),
	setof(X, N1^arc(N1, X), L_to),
	member(N, L_all),
	destinations(N, L_to).


node(X) :-
	arc(X,Y) ; arc(Y, X).

idealANS(X) :-
	node(X),
	\+ (node(Y), X \= Y, \+ path(X,Y)).

/* shortest_path(N1, N2,P)
	to mean P is a shortest path from node N1 to node N2.
	Represent P as a list of nodes starting at N1 and ending at N2.
	A path is possible only in the direction of the arrows in the graph.
	
	Test:
	shortest_path(a, a, P) Answer: P = [a]
	shortest_path(a, e, P) Answer: P = [a,b,e]
	shortest_path(a, f, P) Answers: P = [a,c,f] P = [a,d,f]
	shortest_path(a, f, [a,d,c,f]) Answer: no
	shortest_path(g, b, P) Answer: no
	*/

% getPath(N1, N2, P)
% modified path such that P is the list of path from N1 to N2

path(N, N, [N]).
path(N1, N2, [N1, N2]):-
	arc(N1, N2).
path(N1, N2, [N1|T]):-
	arc(N1, N0),
	path(N0, N2, T).

% smallestList(L, Ls)
% L is the list with the smallest length in list of lists Ls.
smallestList(L, [L]).
smallestList(L, [H1, H2|T]):-
	length(H1, L1),
	length(H2, L2),
	L1 < L2,
	smallestList(L, [H1|T]).
smallestList(L, [H1, H2|T]):-
	length(H1, L1),
	length(H2, L2),
	L1 >= L2,
	smallestList(L, [H2|T]).

% base cases
shortest_path(N1, N2, P):-
	findall(Ps, path(N1, N2, Ps), All_P),
	smallestList(P, All_P).

% 2. i)
/* subList(L1, L2)
   to mean every element in list L1 is also in list L2, and an element in
   list L1 is repeated in L1 at most the number of times it is repeated in L2,
   and the order of the elements in list L1 matches their order in list L2.
   You can assume the second argument L2 is grounded in the call.

   Test:
   subList([1,2,3], [5,1,1,3,2,4,3]). yes.
   subList([1,1,3,4], [5,1,3,2,3,4]). no.
   subList([1,4,3], [1,3,2,3,4]). no.
   subList(X, [1,2,3]).
   X = [], X = [1], X = [1,2], X = [1,2,3], X = [1,3], X = [2], X = [2,3], X = [3].
   */

subList([], _).
subList([H|T1], [H|T2]):-
	subList(T1, T2).
subList([H1|T1], [H2|T2]):-
	subList([H1|T1], T2),
	H1 \= H2.


% 2. ii)
/*	cart(L1, L2, P)
	to mean P is the cartesian product of lists L1 and L2.
	eg if L1 = [a, b] and L2 = [1, 2, 3],
	P = [(a,1),(a,2),(a,3),(b,1),(b,2),(b,3)].

	Test:
	cart([a,b],[1,2,3], P).

	*/

cart([], _, []).
cart(_, [], []).
cart([H1|T1], L2, P):-
	cart(T1, L2, P1),
	setof((H1, E2), member(E2, L2), P2),
	append(P2, P1, P).








