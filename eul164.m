:- module eul164.

:- interface.

:- import_module io.

%:- pred main(io::di, io::uo) is det.
:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int, list, solutions.

less_eq(Upto, N) :- from_to(0, Upto, N).

from_to(N0, Upto, N) :-
	N0 =< Upto,
	(	N = N0
	;
		from_to(N0 + 1, Upto, N)
	). 

:- mode gen(in, in, out) is nondet.
gen(L, T, T1) :- 
	(	L>0 ->
		From = (L=1 -> 1; 0),
		(	T = [],
			from_to(From, 9, H), 
			gen(L-1, [H|T], T1)
		;
			T=[H1],
			from_to(From, 9-H1, H), 
			gen(L-1, [H|T], T1)
		;
			T=[H1,H2|_],
			from_to(From, 9-H1-H2, H), 
			gen(L-1, [H|T], T1)
		)
	;
		T1 = T
	).

%%gen(L, [H1,H2,H3|_] @ T, T1) :- L>0 -> less_eq(9-H1-H2-H3, H), gen(L-1, [H|T], T1); T1 = T.

main -->
	unsorted_aggregate(gen(3, []), (pred(L::in, di, uo) is det -->
		print(L), nl)).

		