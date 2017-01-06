%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% predefined facts and predicate

route(green, [a,b,c,d,e,f]).
route(blue, [g,b,c,h,i,j]).
route(silver, [f,i,k,m]).
route(red, [w,v,e,i,m,n]).
route(yellow, [p, q, r]).

 %  rev(+L1, -L2) means L2 is the list L1 reversed.
rev(L, R) :-
	tr_rev(L, [], R).
tr_rev([], R, R).
tr_rev([H|T], Acc, R) :-
	tr_rev(T, [H|Acc], R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*	The required predicates and argument positions are:
	a.	lines(+Station, ?Lines)
	b.	disjointed_lines(-Lines)
	c.	direct(+S1, +S2, ?Line, ?Route)
	d.	changes(?C, +PR)
	e.	noReps(+PR)
	f.	jp(+S1, +S2, -PR)   --- optional
	*/


/*	lines(+Station, ?Lines)
	to mean Lines is the alphabetically ordered (low to high) list of all
	underground lines passing through station Station.
	If no lines pass through Station then Lines should be the empty list.

	test: lines(i, Ls).
	Ls = [blue, red, silver].
	*/

% setof gets list of alphabetically ordered Lines
% then for each of them find Lines that S passes through
lines(S, Ls):-
	setof(L, Ss^route(L, Ss), Lines),
	findall(L, (member(L, Lines), route(L, Ss), member(S, Ss)), Ls).

/*	disjointed_lines(Lines)
	to mean Lines is the list of all pairs of underground lines that have no
	station in common. The list Lines should have no repetitions.

	test: disjointed_lines(Ls).
	Ls = [(yellow,blue),(yellow,green),(yellow,red),(yellow,silver)] OR
	Ls = [(blue, yellow),(green, yellow),(red, yellow),(silver, yellow)]
	*/


disjointed_line(L1, L2):-
	route(L1, Ss1),
	route(L2, Ss2),
	L1 \= L2,
	\+ setof(S, (member(S, Ss1), member(S, Ss2)), _).

all_disjointed(Lines):-
	setof((L1, L2), S1^S2^(route(L1,S1), route(L2,S2), disjointed_line(L1, L2)), Lines).

rem_dup([], []).
rem_dup([(L1, L2)|T1], [(L1, L2)|T2]):-
	\+ member((L2, L1), T1),
	rem_dup(T1, T2).
rem_dup([(L1, L2)|T], L):-
	member((L2, L1), T),
	rem_dup(T, L).

disjointed_lines(Lines):-
	all_disjointed(L),
	rem_dup(L, Lines).

/*	direct(S1, S2, Line, Route)
	to mean there is a direct route between stations S1 and S2 on underground
	line Line and route is Route.

	test:
	direct(a,e, L, R). L = green, R = [a,b,c,d,e],
	direct(e,a, L, R). L = green, R = [e,d,c,b,a].
	*/


% get_route(S1, S2, Ss, R) gets route R from S1 to S2 direction
% from stations Ss. Assumes Ss is given 
get_route(S, S, _, [S]).

% starts with S1
get_route(S1, S2, [S1,S| T], [S1|TR]):-
	get_route(S, S2, [S|T], TR).

% does not start with S1
get_route(S1, S2, [S|T], R):-
	S1 \= S,
	get_route(S1, S2, T, R).
	

direct(S1, S2, L, R):-
	route(L, Ss),
	member(S1, Ss),
	member(S2, Ss),
	get_route(S1, S2, Ss, R).

direct(S1, S2, L, R):-
	route(L, Ss),
	member(S1, Ss),
	member(S2, Ss),
	rev(Ss, Srev),
	get_route(S1, S2, Srev, R).

/*	changes(C, PR)
	where PR is a ‘planned route’ and C is the list of underground line
	changes on the planned route PR, as follows.
	PR is a list of tuples of the form (Line, List_of_stations).
	Assume PR is correct and C is of form (L1, L2, S).

	test: changes(C, [(green, [a,b,c,d,e]),(red, [e,i])]).
	C = [(green,red,e)]

	changes(C, [(green, [a,b,c,d,e]),(red, [e,i]), (silver, [i,k,m]), (red, [m,n])]).
	C = [(green,red,e),(red,silver,i),(silver,red,m)]

	changes(C, [(green, [a,b]), (green, [b,c,d,e]),(red, [e,i])]).
	C = [(green,red,e)]
	*/

changes([], [_]).
changes([(L1, L2, S)|TC], [(L1, _), (L2, [S|_])|TPR]):-
	L1 \= L2,
	changes(TC, [(L2, [S|_])|TPR]).
changes(C, [(L1, _), (L1, S2)|TPR]):-
	changes(C, [(L1, S2)|TPR]).

/*	noReps(PR)
	to mean that PR, which you can assume is ground and a correct planned
	route in any call to noReps, does not mention any underground line in two different places.

	Test: noReps([(green, [a,b]), (green, [b,c,d,e]),(red, [e,i])]).
	>> no.
	noReps( [(green, [a,b,c,d,e]),(red, [e,i]), (silver, [i,k,m]), (red, [m,n])]).
	>> no
	noReps( [(green, [a,b]),(blue, [b,c,h,i]), (silver, [i,k,m]), (red, [m,n])]). >> yes.
	*/

noReps([]).
noReps([(L, _)|T]):-
	noReps(T),
	\+member((L, _), T).


/*	jp(S1, S2, R) "journey planner"
	Here stations S1 and S2 are assumed given
	jp should generate planned route PR
	assume journey is possible only in order of stations given in facts above.
	eg a -> b is ok, b -> a is not.

	Test: jp(a, f, X). X = [(green,[a,b,c,d,e,f])].

	jp(a, m, X).
	X = [(green,[a,b]),(blue,[b,c,h,i]),(silver,[i,k,m])] ? ;
	X = [(green,[a,b]),(blue,[b,c,h,i]),(red,[i,m])] ? ;
	X = [(green,[a,b,c]),(blue,[c,h,i]),(silver,[i,k,m])] ? ;
	X = [(green,[a,b,c]),(blue,[c,h,i]),(red,[i,m])] ? ;
	X = [(green,[a,b,c,d,e]),(red,[e,i,m])] ? ;
	X = [(green,[a,b,c,d,e]),(red,[e,i]),(silver,[i,k,m])] ? ;
	X = [(green,[a,b,c,d,e,f]),(silver,[f,i,k,m])] ? ;
	X = [(green,[a,b,c,d,e,f]),(silver,[f,i]),(red,[i,m])] ? ;
	no
	*/

% modified direct to include station checking.
directX(S1, S2, [(L, R)]):-
	route(L, Ss),
	member(S1, Ss), member(S2, Ss),
	get_route(S1, S2, Ss, R),
	length(R, Rlen),
	Rlen > 1.


% base case both in same line
jp(S1, S2, [(L, R)]):-
	directX(S1, S2, [(L, R)]).

jp(S1, S2, [(L, R)|T]):-
	directX(S1, S3, [(L, R)]),
	jp(S3, S2, T),
	noReps([(L, R)|T]).
