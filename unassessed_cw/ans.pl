% :-use_module(library(lists)).

arc(a,b).
arc(a,c).
arc(a,d).
arc(b,e).
arc(c, g).
arc(c, f).
arc(d,c).
arc(d,f).
arc(g,e).
arc(g,f).

%%%%    1(i)   %%%%
deadEnd(N) :-
	arc(X,N),
	\+ arc(N,Y).

%%%%    1(ii)   %%%%
shub(N) :-
	findall(Y, (arc(Y,N) ; arc(N,Y)), L),
	length(L, LL),
	LL>=3.

hub(N) :-
	arc(X,N),
	findall(Y, (arc(Y,N) ; arc(N,Y)), L),
	length(L, LL),
	LL >= 3.

hub(N) :-
	arc(N,X),
	findall(Y, (arc(Y,N) ; arc(N,Y)), L),
	length(L, LL),
	LL>=3.

hubs(H) :- 
	setof(N, hub(N), H).


%%%% 1(iii)   %%%%
node(X) :-
	arc(X,Y) ; arc(Y, X).

% get any node X
% prove there does not exist any other node Y
% where X cannot find a path to
ideal(X) :-
	node(X),
	\+ (node(Y), X \= Y, \+ path(X,Y)).

path(X, Y) :- 
	arc(X,Y).

path(X, Y) :- 
	arc(X,Z),
	path(Z, Y).


%%%% 1(iv)   %%%%
ppath(X, X, [X]).
ppath(X, Y, [X,Y]) :-
	arc(X,Y).
ppath(X, Y, [X|Rest]) :-
	arc(X, Z),  
	ppath(Z, Y, Rest).

% find a path P
% prove there are no shorter paths
% hence P must be shortest path
shortest_path(X, Y, P):- 
	ppath(X, Y, P),
	\+ shorter(X, Y, P).

% shorter succeeds if a shorter path from X, Y
% exists which is less than P
shorter(X, Y, P):-
	length(P, L),
	ppath(X, Y, P1),
	length(P1, L1),
	L1<L.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%  2  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-------------Q2(i)-------------------------------------------------%
subList([], L2).
subList([X|Y], L2) :- 
	append(L1, [X|Z], L2),
	subList(Y, Z).

%-------------Q2(ii)----------------------------------------------------------------%

% Cartesian Product
cart([], L, []) :- !.
cart(L, [], []) :- !.
cart([X|Xs], L, P) :-
	prod(X, L, P1),
	cart(Xs, L, P2), 
	append(P1, P2, P).

prod(X, [], []).
prod(X, [Y|Ys], [(X,Y)| Rest]) :- 
	prod(X, Ys, Rest).

