%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sellsFor(S, I, P) Supplier S sells item I for price P
sellsFor('Peter Jones',swan123,28).
sellsFor('Peter Jones',electrolux214,27).
sellsFor('Peter Jones',hoover02,60).
sellsFor('Peter Jones',electrolux09,70).
sellsFor('Harrods', electrolux214, 31).
sellsFor('Harrods', swan123, 30).
sellsFor('Harrods', hoover02, 65).
sellsFor('Harrods', electrolux09, 80).

% inStock(S,I) Supplier S has item I in stock
inStock('Peter Jones',electrolux214).
inStock('Peter Jones',hoover02).
inStock('Peter Jones',electrolux09).
inStock('Harrods',swan123).
inStock('Harrods',electrolux09).

% locatedIn(S,C) Supplier S is located in city C
locatedIn('Peter Jones',london).
locatedIn('Harrods',london).
	
% typeOfItem(I,T) Item I is of type T
typeOfItem(swan123,electricKettle).
typeOfItem(electrolux214,electricKettle).
typeOfItem(hoover02, vacuum_cleaner).     
typeOfItem(electrolux09, vacuum_cleaner).        

% equivalentTo(I1,I2) Items I1 and I2 have equivalent functionality
equivalentTo(electrolux214,swan123).
equivalentTo(electrolux09,hoover02).

% The predicate forall/2 is implemented as \+ ( Cond, \+ Action)
% i.e. There is no instantiation of Cond for which Action is false.
% It proves a relation.
forall(Cond, Action):-
	\+ ((Cond, \+Action)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% i)
/*	sellsOneForLessThan(T, MP, S, I, P)
	Supplier S sells item I of type T at a price I which is less than MP.
	(Assume MP will always be given in any query to this relation.)

	Test: sellsOneForLessThan(electricKettle,30,S,I,P).

	Should get: S=’Peter Jones’.
	*/

sellsOneForLessThan(T, MP, S, I, P):-
	sellsFor(S, I, P),
	typeOfItem(I, T),
	P < MP.

% ii)
/*	equivalent(I1,I2)
	I1 and I2 have equivalent functionality no matter what the alphabetic
	order of identifiers I1, I2.
	
	Test: equivalent(I1,I2).
	Should get: You should get 4 answers.
	*/

equivalent(I1, I2):-
	equivalentTo(I1,I2); equivalentTo(I2, I1).

% iii)
/*	sellsEquivalentItemIn(I, C, EI, S)
	S is a supplier located in city C and S either has item I in stock 
	and EI=I, OR;
	S has an equivalent item EI in stock for no more than than its price 
	for item I.
	(I will be given in queries to this relation.)

	Test: sellsEquivalentItemIn(swan123,london,I,S).

	Should get 2 answers.
	*/

sellsEquivalentItemIn(I, C, EI, S):-
	 locatedIn(S, C),
	 inStock(S, I),
	 EI = I.

sellsEquivalentItemIn(I, C, EI, S):-
	locatedIn(S, C),
	equivalent(I, EI),
	sellsFor(S, I, PI),
	sellsFor(S, EI, PEI),
	PEI =< PI.

% iv)
/*	neverUnderSold(S,C)
	There is no other supplier that in city C sells any item that S sells
	for a price less than S.
	
	Test: neverUnderSold(S,C).
	Should get: C=london, S=’Peter Jones’.
	*/

% gets a supplier
% for all items by that supplier, no price is cheaper in that city
neverUnderSold(S, C):-
	locatedIn(S, C),
	forall( sellsFor(S, I, P),
		\+ ((locatedIn(OS, C),
			sellsFor(OS, I, PI),
			OS \= S,
			PI < P))).

% v)
/*	listOfSuppliersFor(I,C,L)
	L is a list of pairs (P,S) where S is a supplier located in city C that
	supplies item I for the price P and has I is in stock. L is ordered by
	increasing price – lowest price comes first.
	
	Test: listOfSuppliersFor(electrolux09,C,L).
	Should get: 
	C=london
	L=[(70,’Peter Jones’),(80,’Harrods’)]
	*/

listOfSuppliersFor(I, C, L):-
	setof((P,S), (locatedIn(S, C), sellsFor(S,I,P), inStock(S,I)), L).


