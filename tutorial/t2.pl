% fibonacci sequence
fib(1, 1).
fib(2, 1).

fib(N, R):-
	N >= 3,
	N2 is N - 2,
	N1 is N - 1,
	fib(N2, R2),
	fib(N1, R1),
	R is R2 + R1.
