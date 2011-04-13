:- module t2.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, solutions, int, require.

%~ :- mode take(out, in, out) is multi.
%~ take(E, !L) :- !.L = [] -> error($pred); list.delete(!.L, E, !:L).

%~ :- pred perm(list(T), list(T)).
%~ :- mode perm(in, out) is multi.
%~ perm([], []).
%~ perm(L @ [_|_], [E|EE]) :- take(E, L, L1), t2.perm(L1, EE).

:- mode cnt(pred(out) is nondet) = out.
:- mode cnt(pred(out) is multi) = out.
cnt(P) = N :- 
	promise_equivalent_solutions [N] (
		unsorted_aggregate(P, (pred(_V::in, Acc0:int::in, Acc:int::out) is det :-
			Acc = Acc0 + 1), 0, N)
	).
	
:- mode qqq(out) is nondet.	
qqq({/*A,B,*/C,D,E})	:-
	L = 1..5,
	%~ perm(L,A),
	%~ perm(L,B),
	perm(L,C @ [H|_]), H \=2,
	perm(L,D),
	perm(L,E).

%main(!IO) :- write_int(cnt(perm(1..5)), !IO).
main(!IO) :- write_int(cnt(qqq), !IO).