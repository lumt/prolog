% 1.
/*	subList(L1, L2)
	to mean every element in list L1 is also in list L2.
	Assume L1, L2 grounded in call.

	Test: 
	subList([1,2,3], [1,1,3,2,3,4]).
	subList([1,1,4,3], [5,1,3,2,3,4]).

	Should both succeed */

subList([], _).
subList([H|T], L2):-
	member(H, L2),
	subList(T, L2).

% 2.
/*	difference(L1, L2, L)
	to mean L consists of all the elements in L1 that are not in L2.
	Assume L1, L2 grounded in call.
	
	Test: 
	difference([1,1,2,3, 5, 5], [1,3,2, 3,4], L).
	L = [5,5].

	difference([a, c], [], L).
	L = [a,c].
	*/

difference([], _, []).
difference([H|T], L2, L):-
	member(H, L2),
	difference(T, L2, L).

difference([H|T], L2, L):-
	\+ member(H, L2),
	difference(T, L2, L1),
	append([H], L1, L).

% OR
diff(L1, L2, L):-
	findall(E, (member(E, L1), \+ member(E, L2)), L).

% 3.
/*	sift(L, N, Result)
	to mean Result is list L but with all occurrences of elements
	greater than N removed.
	Assume arguments L and N are grounded in the call.

	Test: sift([1,4,3,6,8], 3, X).
	Should get: X=[1,3].
	*/

sift(L, N, Result):-
	findall(E, (member(E, L), E =< N), Result).

% OR:
sift2([], _, []).
sift2([H|T1], N, [H|T2]):-
	H =< N,
	sift2(T1, N, T2).

sift2([H|T1], N, Res):-
	H > N,
	sift2(T1, N, Res).

% 4.
/*	common(L1, L2, I)
	to mean I is the list of the common elements of lists L1 and L2.

	Assume L1, L2 both grounded in call.
	result I should have no repeat, order is not important,
	if L1 L2 have no common output is [].

	Test: common([1,1,4,2,5], [1,1,7,2,3,4,4,8], I).
			I = [1,2,4].
		common([1,2], [4,8], I).
		I = [].
	*/

% unique(L1, L2): L2 removes duplicates in L1
unique([], []).
unique([H|T1], [H|T2]):-
	\+ member(H, T1),
	unique(T1, T2).
unique([H|T1], Out):-
	member(H, T1),
	unique(T1, Out).


common(L1, L2, I):-
	findall(E, (member(E, L1), member(E, L2)), I1),
	unique(I1, I).


% 5.
/*	delete(L,Result)
	Result is list L with every other element deleted.
	Test:  delete([1,2,3,4], R).
	should get: R=[1,3]
	*/

delete([], []).
delete([H1, _|T1], [H1|T2]):-
	delete(T1, T2).

% 6.
/*	process(L1, L2, C, I)
	where L1 is a given list of items of the form (Name, Number),
	L2 is a given list of items of the form (Name, Number, MoreInfo)
	Then the output Consistent should be those items (Name, Number, MoreInfo)
	in L2 that agree on (Name, Number) with list L1.
	Inconsistent should be whatever is left over from list L2.

	Test: 
	process([(mary, 20), (john, 30), (pete, 40)], [(mary, 20, single), (pete, 40, single), (joe, 35, widowed), (john, 35, married)], C, I).
	Should get:
	C= ([(mary, 20, single), (pete, 40, single)]
	I= ([(john, 35, married), (joe, 35, widowed)].
	*/

process(L1, L2, C, I):-
	findall((N, Num, I), (member((N, Num), L1), member((N, Num, I), L2)), C),
	findall((N, Num, I),
			(member((N, Num, I), L2),
			\+member((N, Num, I), C)), I).


% 7.
/*	split(L, N, L1, L2):
	Split a list L into two parts L1 and L2 such that the length of the first
	part is N.

	Test: split([a,b,c,d,e,f,g,h,i,k],3,L1,L2).
	L1 = [a,b,c] L2 = [d,e,f,g,h,i,k]
	*/

split(L2, 0, [], L2).
split([H|T], N, [H|T1], L2):-
	N1 is N - 1,
	split(T, N1, T1, L2).

% 8.
/*	drop(L, N, Result)
	Drop every N'th element from a list L.
	Test: drop([a,b,c,d,e,f,g,h,i,k],3,X).
	X = [a,b,d,e,g,h,k]
	*/

% helper to get index of element in list
getidx(E, [E|_], 1).
getidx(E, [_|T], N):-
	getidx(E, T, N1),
	N is N1 + 1.

drop(L, N, Res):-
	findall(E,
			(member(E, L),
			getidx(E, L, NE),
			mod(NE, N) =\= 0),
			Res).

% 9.
/*	enrolment(L, Student, Degree)
	Given a list L of enrolments, and a student’s name, Student,
	the program finds the degree of the student.
	
	L is a list of students and their degree. Each element of L is of the
	form (Degree, List of students).

	Test: 
	enrolment([(msc, [john, mary, pete]), (meng, [bob, rob, tod]), (msc, [dave, mave])], rob, D).
	Degree = meng.
	*/

enrolment(L, Student, Degree):-
	member((Degree, Ls), L),
	member(Student, Ls).

% 10.
/*	student_list(L, Meng, MSc)
	Separate a list L of students into the Meng students and the MSc students.
	L is a list of students and their degree. Each element of L is of the form
	(Degree, List of students).

	Test:
	student_list([(msc, [john, mary, pete]), (meng, [bob, rob, tod]), (msc, [dave, mave])], MEng, MSc).
	MEng = [bob,rob,tod]
	MSc = [john, mary, pete, dave, mave].
	*/

student_list(L, MEng, MSc):-
	findall(S, (member((Degree, LS), L), member(S, LS), Degree = meng), MEng),
	findall(S, (member((Degree, LS), L), member(S, LS), Degree = msc), MSc).












