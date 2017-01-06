natural_num(0).

natural_num(N):-
		natural_num(M),
		N is M+1.
		