:- module y.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

%:- func fix(func((func(A) = A), A) = A) = (func(A) = A).
:- func fix(func(func(A) = A) = (func(A) = A)) = (func(A) = A).
fix(F) = F(fix(F)).

%:- func fac(int) = int.
%fac = fix(func(F, N) = (N =< 0 -> 1; N * F(N-1))).
fac = fix(func(F) = (func(N) = (N =< 0 -> 1; N * F(N-1)))).

main(!IO) :-
	write_int((fac)(10),!IO).