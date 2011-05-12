:- module y.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

/*
%:- func fix(func((func(A) = A), A) = A) = (func(A) = A).
:- func fix(func(func(A) = A) = (func(A) = A)) = (func(A) = A).
fix(F) = F(fix(F)).

%:- func fac(int) = int.
%fac = fix(func(F, N) = (N =< 0 -> 1; N * F(N-1))).
fac = fix(func(F) = (func(N) = (N =< 0 -> 1; N * F(N-1)))).
*/

:- type mu(A) ---> roll(unroll :: (func(mu(A)) = A)).

%y(F) = (func(X, A) = F((unroll(X))(X), A))(roll(func(X, A) = F((unroll(X))(X), A))).
%%y(F) = (func(X) = (func(A) = F(unroll(X)(X))(A)))(roll(func(X) = (func(A) = F(unroll(X)(X))(A)))).
y(F) = F1(roll(F1)) :- F1 = (func(X) = (func(A) = F(unroll(X)(X))(A))).

y_fac = y(func(F) = (func(N) = (N =< 0 -> 1; N * F(N-1)))).
%%y_fac(N) = y(func(F) = (func(N =< 0 -> 1; N * F(N-1)))).
y_fac1(N) = (y_fac)(N).

main(!IO) :-
	%write_int((fac)(10),!IO)
	write_int((y_fac)(10),!IO), nl(!IO),
	write_int(y_fac1(10),!IO)
	
	.