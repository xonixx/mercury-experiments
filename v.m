:- module v.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is cc_multi.

:- implementation.


:- import_module int, list, solutions.

:- pred positive(int::in, int::out) is nondet.
positive(N, R) :- N > 0, (R=1; R=2).

:- pred select(T::out, list(T)::in, list(T)::out) is nondet. 
select(A, [A | B], B).
select(A, [_ | B], B1) :- select(A, B, B1).

:- pred all_pairs_giving(int::in, list(int)::in, {int,int}::out) is nondet.
all_pairs_giving(N, L, {A, B}) :-
	select(A, L, L1),
	select(B, L1, _),
	A =< B,  % if not succeed -> backtrack
	A + B = N. % if not succeed -> backtrack

:- pred next_5_nums(int::in, int::out) is multi.
next_5_nums(N, R) :-
	( R = N+1
	; R = N+2
	; R = N+3
	; R = N+4
	; R = N+5
	).

main -->
	% prints 1 2
	unsorted_aggregate(positive(5),(pred(N::in, di, uo) is det -->
			       print(N),nl)),

	% prints nothing
	unsorted_aggregate(positive(-7),(pred(N::in, di, uo) is det -->
			       print(N),nl)),

	/* prints
	   {0, 10}
	   {1, 9}
	   {2, 8}
	   {3, 7}
	   {4, 6}
	   */
	unsorted_aggregate(all_pairs_giving(10, [0,1,2,3,4,5,6,7,8,9,10]),
		  (pred(N::in, di, uo) is det -->
		   print(N),nl)),

	/* prints
	   11
	   12
	   13
	   14
	   15
	   */
	unsorted_aggregate(next_5_nums(10),
			   (pred(N::in, di, uo) is det -->
			    print(N),nl))
	.
	