:- module logic.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list, maybe, solutions.
/*
:- type logic_store(VN, T) == list(logic_var(VN, T)).
:- type logic_var(VN, T) ---> logic_var(VN, var_value(T)).
:- type var_value(T) ---> maybe(T).

:- type str_int ---> i(int); s(string).
:- type logic_store == logic_store(str_int).
*/
%%%
:- type lesson ---> lesson(num, lesson_name, lecturer_name).
:- type lesson_name == maybe(lesson_name_t).
:- type lecturer_name == maybe(lecturer_name_t).
:- type num == maybe(int).

:- type lessons == list(lesson).

:- type lesson_name_t ---> eng; phys; chem; biol; lit; math.
:- type lecturer_name_t ---> a; v; s; d; e; f.

:- func unify_maybe(maybe(T), maybe(T)) = maybe(T) is semidet.
unify_maybe(no,yes(T)) = yes(T).
unify_maybe(yes(T),no) = yes(T).
unify_maybe(no, no) = no. % ?
unify_maybe(yes(T), yes(T)) = yes(T).

:- func unify_lesson(lesson, lesson) = lesson is semidet.
unify_lesson(lesson(N0, L0, P0), lesson(N1, L1, P1)) =
  lesson(unify_maybe(N0, N1),
	 unify_maybe(L0, L1),
	 unify_maybe(P0, P1)).

:- mode select_l(in,out, in, out).
select_l(A0, unify_lesson(A0, A1), [A1|B], B).
select_l(!B, [A|C], [A|D]) :-
        select_l(!B, C, D).

select(A, [A|B], B).
select(B, [A|C], [A|D]) :-
        select(B, C, D).


check(_N, _L, _P) :- true.

trace(T) :-
	trace [io(!IO)] (
			 print(T, !IO), nl(!IO)
			 ).

check_all([L | LL1], PP, RR, [Lesson | RR2]) :-
	select(P, PP, PP1),
	%trace(P),
	select_l(lesson(no,yes(L),yes(P)),Lesson, RR, RR1),
	
	Lesson = lesson(yes(N1),yes(L1),yes(P1)),
	%trace(Lesson),
	
	check(N1, L1, P1),
	check_all(LL1, PP1, RR1,RR2).
check_all([], [], [],[]).


solve(OutRes) :-
	LL = [eng, phys, chem, biol, lit, math],
	PP = [a:lecturer_name_t, v, s, d, e, f],
	InRes = [lesson(yes(1),no,no),lesson(yes(2),no,no),lesson(yes(3),no,no),
	       lesson(yes(4),no,no),lesson(yes(5),no,no),lesson(yes(6),no,no)],
	check_all(LL, PP, InRes, OutRes).

main -->
	{
	 solutions((pred(Out::out) is nondet :-
		   solve(Out)), Res)
	 },
	print(Res).
