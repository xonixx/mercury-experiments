:- module logic.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list, maybe.
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
:- type num == int.

:- type lessons == list(lesson).

:- type lesson_name_t ---> eng; phys; chem; biol; lit; math.
:- type lecturer_name_t ---> a; v; s; d; e; f.

unify(no,yes(_)) = _ :- fail.
unify(yes(_),no) = _ :- fail.
unify(no, no) = no. % ?
unify(yes(T), yes(T)) = yes(T).

unify(lesson(N0, L0, P0), lesson(N1, L1, P1)) = lesson(unify(N0, N1), unify(L0, L1), unify(P0, P1)).

:- mode select(in,out, in,out, out).
select(A0,A, [A1|B],[A|B], B) :- A = unify(A0, A1).
select(!B, [A|!.C],[A,!:C], [A|D]) :-
        select(!B, !C, D).

check(_N, _L, _P) :- true.

check_all([L | LL1], PP, !RR) :-
	select(no,P, PP,_, PP1),
	select(lesson(N,L,P),lesson(N1,L1,P1), !RR, RR1),
	check(N1, L1, P1),
	check_all(LL1, PP1, RR1).
check_all([], [], []).


solve(!:Res) :-
	LL = [eng, phys, chem, biol, lit, math],
	PP = [a, v, s, d, e, f],
	!.Res = [lesson(1,no,no),lesson(2,no,no),lesson(3,no,no),
	       lesson(4,no,no),lesson(5,no,no),lesson(6,no,no)],
	check_all(LL, PP, !Res).
